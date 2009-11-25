module Config where
import System.Directory
import System.FilePath
import Data.Maybe
import Data.Char
import Data.List 
import Distribution.Package
import Distribution.Text
import Control.Monad.Reader

hacknixFile file = do
  fmap (</> ".hack-nix" </> file) getHomeDirectory
  
hashCacheFile = hacknixFile "nix-cache" -- contains lines ("url","store path")
defaultConfigPath = hacknixFile "config"
hackNixEnvs = "hack-nix-envs";

data TargetPackages a = TPAll
                    | TPMostRecentPreferred [a] -- most recent version and the ones listed in the preferred-versions file of the hackage index .tar file 
                    | TPCustom [a]
  deriving (Show, Read)

data Config = Config
  { ghc :: String
  , ghcExtraLibs :: Maybe String
  , hackageIndex :: String
  , packagesDir :: [ FilePath ]
  , patchesDir :: [ FilePath ]
  , allPackages :: FilePath
  , targetPackages :: TargetPackages Dependency
  , targetFile :: Maybe FilePath
  , testCabals :: [ FilePath ]
  , patchDirectory :: FilePath
  , workDirectory :: FilePath
  , haskellNixOverlay :: FilePath
  , nixConcurrency :: Int
  } deriving (Show)

configErr value = error $ "missing configuration file value: " ++ value
emptyConfig = Config (configErr "ghc tar file") Nothing (configErr "hackage index file")
                     [] [] "" (TPMostRecentPreferred []) Nothing [] "" "" "" 1

defaultConfigContents = unlines
  [ "ghc http://haskell.org/ghc/dist/6.10.1/ghc-6.10.1-src.tar.bz2 # url or sourceByName name"
  , "ghc-extra-libs http://haskell.org/ghc/dist/6.10.1/ghc-6.10.1-src-extralibs.tar.bz2 # url or archive"
  , "hackage-index http://hackage.haskell.org/packages/archive/00-index.tar # url"
  , "packages-dir mydir # can be given multiple times"
  , "patches-dir mypatchdir # can be given multiple times"
  , "all-packages.nix /etc/nixos/nixpkgs/pkgs/top-level/all-packages.nix"
  , "target-packages TPMostRecentPreferred [\"Cabal == 1.4.0.0\"] # additional list of non recent packages to be added"
  , "target-file Just \"result\" # where to write the result to. Addition"
  , "test-cabal-files [\"tests/test.cabal\"] # this will be added to the package db. used by test cases"
  , "patch-directory \"path-to-nix-haskell-repo/patches\" # Path to nix-haskell-repo/patches"
  , "work-directory \"/tmp/work-directory\"   # source will be put into this directory so that you can write patches easily"
  , "haskell-nix-overlay \"path-to-nix-haskell-repo\" # Path to nix-haskell-repo/patches"
  , "nix-concurrency 1 # -j flag for nix-* commands"
  ]

writeSampleConfig = (flip writeFile) defaultConfigContents

formatInfo = error "TODO"

parseConfig :: String -> Config
parseConfig config = 
    foldr parseLine emptyConfig $ (map dropSpaces) $ filter (not . all isSpace) $ lines config
  where parseLine ('#':_) cfg = cfg
        parseLine l cfg 
          | isPrefixOf "all-packages.nix " l = cfg { allPackages = dropS (length "all-packages.nix") $ dropEOLComment l }
          | isPrefixOf "ghc " l = cfg { ghc = dropS 3 $ dropEOLComment l }
          | isPrefixOf "ghc-extra-libs " l = cfg { ghcExtraLibs = Just $ dropS (length "ghc-extra-libs") $ dropEOLComment l }
          | isPrefixOf "hackage-index" l = cfg { hackageIndex = dropS (length "ghc-extra-libs") $ dropEOLComment l }
          | isPrefixOf "packages-dir " l = cfg { packagesDir = [dropS (length "packages-dir") $ dropEOLComment l] ++ (packagesDir cfg)  }
          | isPrefixOf "patches-dir " l = cfg { patchesDir = [dropS (length "packges-dir") $ dropEOLComment l] ++ (patchesDir cfg) }
          | isPrefixOf "target-packages " l = cfg { targetPackages = parseTargetPackages $! read $! dropS (length "target-packages") $! dropEOLComment l }
          | isPrefixOf "target-file " l = cfg { targetFile = read $ dropS (length "target-file") $ dropEOLComment l }
          | isPrefixOf "test-cabal-files " l = cfg { testCabals = read $ dropS (length "test-cabal-files") $ dropEOLComment l }
          | isPrefixOf "patch-directory" l = cfg { patchDirectory = read $ dropS (length "patch-directory") $ dropEOLComment l }
          | isPrefixOf "work-directory" l = cfg { workDirectory = read $ dropS (length "work-directory") $ dropEOLComment l }
          | isPrefixOf "haskell-nix-overlay" l = cfg { haskellNixOverlay = read $ dropS (length "haskell-nix-overlay") $ dropEOLComment l }
          | isPrefixOf "nix-concurrency" l = cfg { nixConcurrency = read $ dropS (length "nix-concurrency") $ dropEOLComment l }
          | otherwise = error $ "can't parse config line " ++ l

        dropSpaces = dropWhile isSpace
        dropEOLComment = takeWhile (/= '#')
        dropS n = dropSpaces . drop n
        parseTargetPackages :: TargetPackages String -> TargetPackages Dependency
        parseTargetPackages TPAll = TPAll
        parseTargetPackages (TPMostRecentPreferred list) = TPMostRecentPreferred $ map sP list
        parseTargetPackages (TPCustom list) = TPCustom $ map (fromJust . simpleParse) list
        sP x = case simpleParse x of
          Just a -> a
          Nothing -> error $ "could'n parse dependency information :" ++ x

type ConfigR b = ReaderT Config IO b
withConfig config f = (runReaderT f) config
 
hackNixCabalConfig :: String
hackNixCabalConfig = ".hack-nix-cabal-config" -- one definition 
