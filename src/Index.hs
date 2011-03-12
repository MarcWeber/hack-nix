{-# OPTIONS -cpp #-}
{-# OPTIONS_GHC -XViewPatterns #-}
module Index where
import NixLanguage
import Distribution.Version
import Data.Function
import Control.Concurrent
import ProcessPool
import NixLangUtil
import Config
import Data.Maybe
import Data.List (isPrefixOf, nub, sortBy)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Codec.Compression.GZip(decompress)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Codec.Archive.Tar as Tar
import Distribution.PackageDescription as PD
import Distribution.Package as D
import Distribution.Text
import System.FilePath.Posix
import System.IO.Unsafe
import System.Directory
import Utils

type PreferredVersion = String -- TODO
data Index = Index {
    preferredVersions :: Map.Map PackageName [Dependency]
    , packages :: [ GenericPackageDescription ]
    , warnings :: [ String ]
  }

data IncludeReason = ITarget | IDependency

data PreparedPackage = PreparedPackage {
        preferred :: Bool,
        includeReason :: IncludeReason,
        pkg :: GenericPackageDescription
      }


-- parsePreferredVersions taken from cabal-install 
parsePreferredVersions :: String -> [Dependency]
parsePreferredVersions = catMaybes
                       . map simpleParse
                       . filter (not . isPrefixOf "--")
                       . lines

instance Show Index where
  show (Index pv p ws) = unlines $ "Index:" : "preferred versions" : map display ((concat . Map.elems) pv) ++ ["packages"] ++ map show p ++ [ "warnings:" ] ++ ws
emptyIndex :: Index
emptyIndex = Index Map.empty [] []

type IndexMap = Map.Map String (Set.Set Version)

-- there is no source package availible for those: 
faultyPackages :: Set.Set ([Char], String)
faultyPackages = Set.fromList [
    ("hsdns", "0.0-2006-04-08" )
    ,("hopenssl", "0.0-2005-02-14")
    ,("child", "0.0-2005-02-14")
    ,("blockio", "0.0-2006-02-03")
    ,("hsgnutls", "0.2.3.1-barracuda")
    ,("hsgnutls", "0.2.3")
    ,("monadenv", "0.0-2005-02-14")
   ]

-- map describing how tasks of which kind should run in parallel
maxConc :: Config -> Map.Map PTT Int
maxConc cfg = Map.fromList [ 
     (PTTReadIndex, 1)
   , (PTTFetch, parallelParsing cfg)
   , (PTTPatch, parallelPatching cfg)
   , (PTTParse, parallelFetching cfg)
   ]


-- determine list of packages to be installed based on TargetPackages setting
filterTargetPackages :: TargetPackages Dependency -> Map.Map String [Dependency] -> [(String, Version, String)] -> [(String, Version, String)]
filterTargetPackages targetPackages' preferred' packages' = do
  let byName = Map.map sortByVersion $ Map.fromListWith (++) [ (name, [p]) | p@(name,_,_) <- packages' ]
  -- after grouping all packages by name only keep wanted packages
  concatMap (filterByName targetPackages') $ Map.toList byName
  where
    -- match packages which has been selected by the user explicitly 
    elected :: [Dependency] -> [ (String, Version, String) ] -> [(String, Version, String)]
    elected deps pkgs = filter (\p -> any (dependencyMatchesPkg p) deps) pkgs
      where dependencyMatchesPkg (n,v,_) (Dependency (PackageName name) vRange) = name == n && withinRange v vRange

    sortByVersion :: [(String, Version, String)] -> [(String, Version, String)]
    sortByVersion = sortBy ( (flip compare) `on` (\(_,v,_) -> v) )

    latest = take 1 . sortByVersion

    filterByName :: TargetPackages Dependency -> (String, [(String, Version, String)]) -> [(String, Version, String)]
    filterByName tp (name, ps) = case tp of
      TPAll -> ps
      TPMostRecentPreferred deps _ -> nub $
        head ps -- most recent 
        : elected deps ps -- selected by user 
        ++ (latest (elected (fromMaybe [] (Map.lookup name preferred')) ps)) -- preferred. This list is contained in hackage index. Only keep latest
      TPCustom deps _ -> nub $ elected deps ps



-- first task starting more tasks
-- -> BL.ByteString -> 
readIndexTask :: Config
                 -> BL.ByteString
                 -> MVar
                      (Map.Map NixLanguage.NixType NixLanguage.NixType)
                 -> FilePath
                 -> Task PTT [Char] ()
readIndexTask cfg bs results tmpDir = Task PTTReadIndex "reading index" $ \newAction -> do
  let tarErr err = unsafePerformIO (putStrLn ("err decoding tar :" ++ err) >> return [])
  let tarEntries = (Tar.foldEntries (:) [] tarErr) $ Tar.read $ decompress bs
  let preferredVersionsFromTar = Map.empty -- TODO
  let targetPackages' = (filterTargetPackages (targetPackages cfg) preferredVersionsFromTar) $ catMaybes $ map readEntry tarEntries

  let strToNix name s =
        -- TODO: make this strict so that the work is done in threads !?
        case parsePkgDescToEither s of
          Left errs -> putStrLn $ "error parsing cabal file " ++ name ++ ":\n" ++ errs
          Right (ws, pd) -> do
            putStrLn $ "warnings parsing cabal file " ++ name ++ ":\n" ++ unlines ws
            -- create download tasks ?
            p <- packageDescriptionToNix STHackage pd
            modifyMVar_ results $ return . Map.insert p p

  let packageToNix :: (String, Version, String) -> Task PTT String ()
      packageToNix (name,version,cabalStr) = Task PTTParse ("parsing " ++ name) $ \_ -> do
      let fullName = name ++ "-" ++ show version
      let pf = patchDirectory cfg </> fullName ++ ".patch"
      e <- doesFileExist pf
      if e then newAction $ Task PTTPatch ("patching " ++ name) $ \_ -> do
                  let td = tmpDir </> fullName
                  createDirectoryIfMissing False td
                  let tmpFile = td </> name ++ ".cabal"
                  writeFile tmpFile cabalStr
                  run Nothing "patch" ["-p1", "--batch", "-i", pf] (Just td) Nothing
                  c <- readFile tmpFile
                  strToNix fullName c
        else strToNix fullName cabalStr

  mapM_ (newAction . packageToNix) targetPackages'

  where 
      -- tar entry to (name, version, String of cabal file)
      -- Don't parse .cabal file yet - because most .cabal files are disregarded
      readEntry e = case splitDirectories (Tar.entryPath e) of
                      [ "preferred-versions" ] -> Nothing -- TODO add these?
                      [ name, version, _ ] ->
                        let v = fromMaybe (error $ "error parsing version: " ++ version) $ simpleParse version
                        in if Set.member (name, version) faultyPackages
                              then Nothing
                              else Just (name, v, BL.unpack $ cont (Tar.entryPath e) $ Tar.entryContent e)
                      path -> error $ "unkown index path ? " ++ (show path)


      cont _ (Tar.NormalFile bs' _) = bs'
      cont s _ = error $ "NormalFile tar content expected at " ++ s
