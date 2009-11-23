module Patching where
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
import System.FilePath
import System.Process
import System.IO


{- implementation of actions helping creating patches fast -}

-- download src of fullName and unpack it into the store
-- returns store path 

wd, patchFile :: String -> ConfigR FilePath

wd fullName = do
  liftM (</> fullName) $ asks workDirectory
patchFile fullName = do
  liftM (</> fullName ++ ".patch") $ asks patchDirectory

nixStoreSrcDir :: String -> ConfigR FilePath
nixStoreSrcDir fullName =
  let (versionStrR,_:nameR) = break (== '-') $ reverse fullName
      url = hackageSrcUrl (reverse nameR) (reverse versionStrR)
  in do
    (p,_) <- liftIO $ downloadCached url False
    unpack p

-- exits if command exitst with non zero exit status
run :: Maybe Int -> String -> [String] -> Maybe String -> Maybe Handle -> IO ()
run mbExpectedEC prog args mbWorkDir mbStdout = do
  let str = prog ++ " " ++ show args ++ " in " ++ show mbWorkDir
  putStrLn $ "running :" ++ str
  h <- runProcess prog args mbWorkDir  Nothing Nothing mbStdout Nothing
  ec <- waitForProcess h
  let got = case ec of
              ExitSuccess -> 0
              ExitFailure ec -> ec
  case mbExpectedEC of
    Nothing -> return ()
    (Just expectedEC) -> 
      when (expectedEC /= got) $ do
          putStrLn $ "run: " ++ str
          putStrLn $ "expected exit code: " ++ show expectedEC ++ " but got: " ++ show got
          exitWith (ExitFailure 1)
  

unpackPackage, applyPatch, createPatch, patchWorkflow :: String -> ConfigR ()

unpackPackage fullName = do
  wd <- wd fullName
  src <- nixStoreSrcDir fullName
  es <- liftIO $ doesDirectoryExist wd
  let clean = liftIO $ run (Just 0) "rm" ["-fr", wd] Nothing Nothing
  let copy = do
      liftIO $ do
        run (Just 0) "cp" ["-r", src, wd] Nothing Nothing
        -- files are readonly in /nix/store. Make them writable again 
        run (Just 0) "chmod" ["u+w", "-R", wd] Nothing Nothing
      applyPatch fullName
  case es of
    True -> do
      liftIO $ putStrLn $ "working directory " ++ wd ++ " exists. clean ? y/[n]"
      a <- liftIO $ getChar
      when (a == 'y') $ clean >> copy
    False -> copy
  liftIO $ putStrLn $ "working directory is " ++ wd

applyPatch fullName = do
  wd <- wd fullName
  pf <- patchFile fullName
  e <- liftIO $ doesFileExist pf
  if e then liftIO $ do
      putStrLn $ "patch " ++ pf ++ " found, apply it? y/[n]"
      a <- getChar
      when (a == 'y') $ do
        run Nothing "patch" ["-p1" , "-d", wd, "-i", pf] Nothing Nothing
    else liftIO $ putStrLn $ "applyPatch: patchfile " ++ pf ++ " doesn't exist. Doing nothing"

createPatch fullName = do
  wd <- wd fullName
  src <- nixStoreSrcDir fullName
  pf <- patchFile fullName
  liftIO $ do
    pfH <- openFile pf WriteMode
    run (Just 1) "diff" ["-U", "3", src, wd] Nothing (Just pfH)
    hClose pfH
    run (Just 0) "sed" ["-i", "-e", "s@" ++ src ++ "@" ++ "a@g", pf ] Nothing Nothing
    run (Just 0) "sed" ["-i", "-e", "s@" ++ wd ++ "@" ++ "b@g", pf ] Nothing Nothing

patchWorkflow fullName = do
  unpackPackage fullName
  applyPatch fullName
  wd <- wd fullName
  liftIO $ do
    shell <- getEnv "SHELL" -- TODO catch exception 
    putStrLn  "running shell, so that you can start patching"
    run (Just 0) shell [] (Just wd) Nothing
  createPatch fullName
  pf <- patchFile fullName
  liftIO $ do
    putStrLn "commit ? y/[n]"
    a <- getChar
    when (a == 'y') $ do
      run (Just 0) "git" ["reset"] (Just wd) Nothing
      run (Just 0) "git" ["add", pf] (Just wd) Nothing
      run (Just 0) "git" ["commit", "-c", "-m", "adding / updating patchfile for " ++ fullName] (Just wd) Nothing
