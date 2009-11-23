{-# OPTIONS_GHC -XViewPatterns #-}
module Index where
import Data.Version
import Data.Maybe
import Data.List (isPrefixOf, nub)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Version (Version,parseVersion)
import Codec.Compression.GZip(decompress)
import qualified Data.ByteString.Lazy.Char8 as BL
import Codec.Archive.Tar as Tar
import Distribution.PackageDescription as PD
import Distribution.Package as D
import Distribution.Text
import Distribution.Package
import System.FilePath.Posix
import Distribution.PackageDescription.Parse
import MaybeRead (readPMaybe)
import Distribution.ParseUtils as PU
import Control.Monad (when)
import System.IO.Unsafe
import System.Directory
import Patching

type PreferredVersion = String -- TODO
data Index = Index {
    preferredVersions :: Map.Map PackageName [Dependency]
    , packages :: [ GenericPackageDescription ]
    , warnings :: [ String ]
  }

data IncludeReason = Target | Dependency

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
emptyIndex = Index Map.empty [] []

type IndexMap = Map.Map String (Set.Set Version)

parseResultToEither :: PU.ParseResult a -> Either String ([String],a)
parseResultToEither (PU.ParseOk warnings a) = Right (map show warnings, a)
parseResultToEither (PU.ParseFailed error) = Left (show error)

parsePkgDescToEither :: String -> Either String ([String], GenericPackageDescription)
parsePkgDescToEither = parseResultToEither . parsePackageDescription

parsePkgFormFile :: FilePath -> IO GenericPackageDescription
parsePkgFormFile file = do
  c <- readFile file
  case parsePkgDescToEither c of
    Left e -> error $ unlines [ "parsing of " ++ file ++ "failed :", e]
    Right (ws, pd) -> do
      when ((not . null) ws) $ putStrLn $ unlines $ [ "warnings while parsing " ++ file ++ ":"] ++ ws
      return pd


-- makes sure file is fully closed after reading
readFile' :: FilePath -> IO String
readFile' f = do s <- readFile f
                 return $! (length s `seq` s)

readIndex :: String -> BL.ByteString -> Index
readIndex patchDirectory = filterFaulty . foldEntries fold emptyIndex undefined . Tar.read . decompress
  where 
    filterFaulty index = index { packages = filterPackages (packages index) }
    filterPackages pkgs =
      -- there is no source package availible for those: 
      let faulty = [ ("hsdns", [0,0] )
                    ,("hopenssl", [0,0])
                    ,("child", [0,0])
                    ,("blockio", [0,0])
                    ,("monadenv", [0,0])
                    ,("hsgnutls", [0,2,3,1])
                    ,("hsgnutls", [0,2,3])
                   ]
      in filter (\gP -> let pkg = (package . packageDescription) gP
                            (PackageName n) = pkgName pkg
                        in not $ elem (n, (versionBranch . pkgVersion) pkg ) faulty)  pkgs
    tarError = error . ("error reading tar " ++)
    fold entry index =
      let path = entryPath entry
          cont (NormalFile bs _) = bs
          cont _ = error $ "NormalFile tar content expected at " ++ path
          -- content as string. If a patch exists apply it 
          asString' = BL.unpack $ cont $ entryContent entry
          asString name version = unsafePerformIO $ do -- hacky - I'm to lazy to move this into IO monad 
            let cont = asString'
            let fullName = name ++ "-" ++ version
            let pf = patchDirectory </> fullName ++ ".patch"
            e <- doesFileExist pf
            if e then do
                  tmpDir <- fmap ( </> "hack-nix-tmp")  getTemporaryDirectory
                  createDirectoryIfMissing False tmpDir
                  let tmpFile = tmpDir </> name ++ ".cabal"
                  writeFile tmpFile cont
                  run Nothing "patch" ["-p1","-i", pf] (Just tmpDir) Nothing
                  readFile' tmpFile
              else return cont

          toTuple d@(D.Dependency a _) = (a, [d])
      in case splitDirectories path of
        [ "preferred-versions" ] ->
            index { preferredVersions = Map.fromListWith (\ a b -> nub (a ++ b)) $ map toTuple $ parsePreferredVersions asString' }
        [ ".", name, version, _ ] ->
            case parsePkgDescToEither (asString name version) of
                Right (ws, a) -> index { packages = a:(packages index)
                                      , warnings = warnings index ++ path:ws }
                Left error -> index { warnings = warnings index ++ [ path ++ " !! parsing failed, reason: " ++ error] }
        path -> error $ "unkown index path ? " ++ (show path)
