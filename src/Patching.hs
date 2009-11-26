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
  let (name, version) = splitName fullName
      url = hackageSrcUrl name version
  in do
    (p,_) <- liftIO $ downloadCached url False
    unpack p


unpackPackage, applyPatch, createPatch:: String -> ConfigR ()

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

patchWorkflow :: String -> ConfigR () -> ConfigR ()
patchWorkflow fullName recreateHackNixFile = do
  unpackPackage fullName
  applyPatch fullName
  wd <- wd fullName
  liftIO $ do
    shell <- getEnv "SHELL" -- TODO catch exception 
    putStrLn  "running shell, so that you can start patching"
    run (Just 0) shell [] (Just wd) Nothing
  createPatch fullName
  pf <- patchFile fullName
  a <- liftIO $ do
    putStrLn "recreate hack-nix-db.nix file? y/[n]"
    getChar
  when (a == 'y') $
    recreateHackNixFile
