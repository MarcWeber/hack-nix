module Main where
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Config
import Control.Exception
import Nix
import Index
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import System.Exit
import GetOptions
import System.Directory
import System.Environment
import Network.URI


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

run :: String -> IO ()
run cfg =  do
  cfg <- liftM parseConfig $ readFile cfg
  withConfig cfg $ do
    (liftIO . print) =<< unpack =<< downloadCached (ghc cfg)
    case ghcExtraLibs cfg of
      Nothing -> return ()
      Just p -> (liftIO . print) =<< unpack =<< downloadCached p

main = do
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
