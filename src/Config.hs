module Config where
import Data.Maybe
import Data.Char
import Data.List 
import Control.Monad.Reader
hashCacheFile = ".nix-cache" -- contains lines ("url","store path")
defaultConfigPath = ".hack-nix"

data Config = Config
  { ghc :: String
  , ghcExtraLibs :: Maybe String
  , bleedingEdgeReposInfo :: FilePath
  , hackageIndex :: Maybe String
  , packagesDir :: [ FilePath ]
  , patchesDir :: [ FilePath ]
  , allPackages :: FilePath
  } deriving (Show)

emptyConfig = Config (error "no ghc given") Nothing Nothing [] [] ""

defaultConfigContents = unlines
  [ "ghc http://haskell.org/ghc/dist/6.10.1/ghc-6.10.1-src.tar.bz2 # url or sourceByName name"
  , "bleeding-edge-fetch-infos.nix /pr/ghc68/nixpkgs/pkgs/misc/bleeding-edge-fetch-infos.nix # bleeding-edge-fetch-infos.nix file containing information about bleeding edge file repos"
  , "ghc-extra-libs http://haskell.org/ghc/dist/6.10.1/ghc-6.10.1-src-extralibs.tar.bz2 # url or archive"
  , "hackage-index http://hackage.haskell.org/packages/archive/00-index.tar # url"
  , "packages-dir mydir # can be given multiple times"
  , "patches-dir mypatchdir # can be given multiple times"
  , "all-packages.nix /etc/nixos/nixpkgs/pkgs/top-level/all-packages.nix"
  ]

writeSampleConfig = (flip writeFile) defaultConfigContents

formatInfo = error "TODO"

parseConfig :: String -> Config
parseConfig config = 
    foldr parseLine emptyConfig $ (map dropSpaces) $ lines config
  where parseLine ('#':_) cfg = cfg
        parseLine l cfg 
          | isPrefixOf "all-packages.nix " l = cfg { allPackages = dropS (length "all-packages.nix") $ dropEOLComment l }
          | isPrefixOf "ghc " l = cfg { ghc = dropS 3 $ dropEOLComment l }
          | isPrefixOf "ghc-extra-libs " l = cfg { ghcExtraLibs = Just $ dropS (length "ghc-extra-libs") $ dropEOLComment l }
          | isPrefixOf "hackage-index" l = cfg { ghcExtraLibs = Just $ dropS (length "ghc-extra-libs") $ dropEOLComment l }
          | isPrefixOf "packages-dir " l = cfg { packagesDir = [dropS (length "packages-dir") $ dropEOLComment l] ++ (packagesDir cfg)  }
          | isPrefixOf "patches-dir " l = cfg { patchesDir = [dropS (length "packges-dir") $ dropEOLComment l] ++ (patchesDir cfg) }
          | otherwise = error $ "can't parse config line " ++ l

        dropSpaces = dropWhile isSpace
        dropEOLComment = takeWhile (/= '#')
        dropS n = dropSpaces . drop n

type ConfigR b = ReaderT Config IO b
withConfig config f = (runReaderT f) config
