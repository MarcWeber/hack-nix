module Main where
import Control.Monad.Reader.Class
import Data.Function
import Text.PrettyPrint
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Distribution.Package
import Distribution.PackageDescription
import Config
import Control.Exception
import Nix
import NixLangUtil
import NixLanguage
import Index
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import System.Exit
import GetOptions
import System.Directory
import System.Environment
import Network.URI
import Data.List
import Utils


-- writeConfig Map file = writeFile file $ lines $ mapWithKey

-- [>minimal Config 
-- parseConfig :: String -> Map
-- parseConfig =
--   let split s = let  (a,_:set) (break ( == '=') in (a,set)
--   in fromList . map split . lines

help = unlines [ 
    "hack-nix: brings hackage packages to nix"
  , "to update the hackage index simply remove 00-index.tar (current dir)"
  ]

-- determine list of packages to be installed based on TargetPackages setting
filterTargetPackages :: M.Map PackageName [Dependency] -> [GenericPackageDescription] -> ConfigR [GenericPackageDescription]
filterTargetPackages preferred packages = do
  tp <- asks targetPackages
  let 
      sortByVersion :: [GenericPackageDescription] -> [GenericPackageDescription]
      sortByVersion = sortBy ( (flip compare) `on` (pkgVersion . package . packageDescription) )
      byName :: M.Map PackageName [GenericPackageDescription]      
      byName = M.map sortByVersion $ M.fromListWith (++) [ let name = (pkgName . package . packageDescription) p in (name, [p]) | p <- packages ]
  -- after grouping all packages by name only keep wanted packages
  return $ concatMap (filterByName tp) $ M.toList byName
  where
    -- match packages which has been selected by the user explicitly 
    elected :: [Dependency] -> [ GenericPackageDescription ] -> [GenericPackageDescription]
    elected deps pkgs = concat [ filter (matchDepndencyConstraints d . package . packageDescription ) pkgs | d <- deps ]

    filterByName :: TargetPackages Dependency -> (PackageName, [GenericPackageDescription]) -> [GenericPackageDescription]
    filterByName tp (pn@(PackageName name), ps) = case tp of
      TPAll -> ps
      TPMostRecentPreferred deps -> nub $ head ps : elected deps ps ++ elected (fromMaybe [] (M.lookup pn preferred)) ps
      TPCustom deps -> nub $ elected deps ps

run :: String -> IO ()
run cfg =  do
  cfg <- liftM parseConfig $ readFile cfg
  withConfig cfg $ do
    (hackageIndex, _) <- liftIO $ downloadCached (hackageIndex cfg) False
    liftIO $ putStrLn $ "hackage index is " ++ hackageIndex
    parsedTestCabals <- liftIO $ mapM parsePkgFormFile $ testCabals cfg
    indexContents <- liftIO $ liftM readIndex $ BL.readFile hackageIndex

    liftIO $ do
      print "parsed test cabals"
      print parsedTestCabals

    let allPkgs = packages indexContents ++ parsedTestCabals

    -- liftIO $ writeFile "all" (show (packages indexContents))
    -- let (pkgs :: [ GenericPackageDescription ]) = packages indexContents
    targetPackages <- filterTargetPackages (preferredVersions indexContents ) $ allPkgs
    
    -- liftIO $ print indexContents
    attrs <- liftIO $ mapM (\(nr,b) -> do
                let (PackageName name) = pkgName $ package $ packageDescription $ b
                putStrLn $ "checking source of " ++ name  ++ "  " ++ show nr ++ "/" ++ (show . length ) allPkgs
                packageDescriptionToNix parsedTestCabals b) $ zip [1 ..] $ targetPackages
    let result = unlines $ "["
                           : (map (renderStyle style . toDoc) attrs)
                           ++ ["]"]

    liftIO $ do
      -- STDOUT 
      -- putStrLn result
      case targetFile cfg of 
        Just f -> writeFile f result
        Nothing -> return ()

main = (flip finally) saveNixCache $ do
  loadNixCache
  args <- getArgs
  case args of
    ["--write-config"] -> writeSampleConfig defaultConfigPath
    ["--write-config", cfg] -> writeSampleConfig cfg
    ["--print-format-info"] -> putStrLn $ formatInfo
    ["-h"] -> help
    ["--help"] -> help
    [cfg] -> do run cfg
    [] -> do
      de <- doesFileExist defaultConfigPath
      case de of
        True -> run defaultConfigPath
        False -> do  putStrLn $ "sample config does not exist, writing it to " ++ defaultConfigPath
                     putStrLn "adjust it to your needs and rerun .."
                     writeSampleConfig defaultConfigPath
                     putStrLn "done, also see --help"
                     exitWith (ExitFailure 1)
    _ -> help

  where help = do
                progName <- getProgName
                putStrLn $ unlines $ map ("  " ++ ) $
                      [ progName ++ ": a small helper which takes a list of haskell packages as input"
                      , "then resolves dependencies and finally outpus a bunch of nix expressions"
                      , progName ++ " [cfg] dest         : create nix expressions and put them into dest"
                      , ""
                      , progName ++ "--print-format-info : prints format info about config"
                      , progName ++ "--write-config [cfg]: writes an initial config file"
                      , "default config path is: " ++ defaultConfigPath
                      ]


  -- c <- readFile configFile
  -- [>cfg <- parseConfig c
  
  --   (as,(updateIndex,verb,count,xs)) <- getOptions (
  --       "i|update-index" ?? "upadate index file (hackage package list)"
  --       )
  --   when verb $ putStrLn "Verbose"
  --   putStrLn $ "Output Name is: " ++ output_name
  --   putStrLn $ "Args: " ++ show as
  --   sequence (replicate (count::Int) (putStrLn "Check!"))
  --   mapM_ putStrLn xs
