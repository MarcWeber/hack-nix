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
import Distribution.PackageDescription
import Distribution.Package as D
import Distribution.Text
import Distribution.Package
import System.FilePath.Posix
import Distribution.PackageDescription.Parse
import MaybeRead (readPMaybe)

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

readIndex :: BL.ByteString -> Index
readIndex = filterFaulty . foldEntries fold emptyIndex undefined . Tar.read . decompress
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
          asString = BL.unpack $ cont $ entryContent entry
          toTuple d@(D.Dependency a _) = (a, [d])
      in case splitDirectories path of
        [ "preferred-versions" ] ->
            index { preferredVersions = Map.fromListWith (\ a b -> nub (a ++ b)) $ map toTuple $ parsePreferredVersions asString }
        [ ".", name, version, _ ] ->
            case parsePackageDescription asString of
                ParseOk ws a -> index { packages = a:(packages index)
                                      , warnings = warnings index ++ path:(map show ws)  }
                ParseFailed error -> index { warnings = warnings index ++ [ path ++ " !! parsing failed, reason: " ++ show error] }
        path -> error $ "unkown index path ? " ++ (show path)
