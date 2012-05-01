{-# OPTIONS -cpp #-}
{-# OPTIONS_GHC -XScopedTypeVariables #-}
-- uses the file .nix-cache to associate given urls with hash values 
-- the .nix-cache file line fromat is
-- ("url", "storepath")
--
-- using /nix/store as storage
module Nix where
import Data.List
import qualified Data.Map as M
import System.Directory
import System.IO
import System.IO.Unsafe
import Config
import Data.IORef
import System.Process
import Control.Monad
import Control.Monad.Reader
import System.Exit
import Utils
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS

#include "interlude.h"
import Interlude

type Cache = (M.Map String (String, String)) -- url (path, hash) 

{-# NOINLINE nixCache #-}

nixCache, nixCacheRead :: IORef Cache
-- this is uset to find out whether the cache has been changed
nixCacheRead = unsafePerformIO $ newIORef undefined
-- the cache keeping hashes
nixCache = unsafePerformIO $ newIORef undefined

cacheContents :: IO Cache
cacheContents = do
  cacheFile <- hashCacheFile
  de <- doesFileExist cacheFile
  if de then do
              contents <- readFileLocked cacheFile
              return $ M.fromList $ map read $ lines $ contents
        else return $ M.empty

loadNixCache :: IO ()
loadNixCache = do
  cc <- cacheContents
  writeIORef nixCache cc
  writeIORef nixCacheRead cc

saveNixCache :: IO ()
saveNixCache = do
    r <- readIORef nixCacheRead
    this <- readIORef nixCache
    when (r /= this) $ do
        -- merge in cache contents which another process may have written
        -- this is still not perfect but probably good enough
        c <- cacheContents
        cacheFile <-  hashCacheFile
        writeFileLocked cacheFile $ unlines $ map show $ M.toList (M.union this c)

-- | downloads the url using nix-prefetch url and puts the (url , nix/store) path tuple into hashCacheFile 
--  so that it only has to be fetched once
--  could have used (url,hash), but I was to lazy to look the path up...
downloadCached :: String -> Bool -> IO (String, String) -- (storepath, hash) 
downloadCached url forceStorePath = do
  p <- liftM (M.lookup url) $ readIORef nixCache

  let retrieve = do 
        putStr $ "retrieving " ++ url ++ " using nix-prefetch-url ..  \n\n"
        (inH, outH, errH, p) <- runInteractiveProcess "nix-prefetch-url" [url] Nothing Nothing
        e <- hGetContents errH
        out <- hGetContents outH
        -- bad: force reading:
        putStrLn out
        putStrLn e
        ec <- waitForProcess p
        putStrLn "done"
        case ec of 
          ExitFailure _ -> error $ "nix-prefetch-url could not download '" ++ url ++ "' cause :\n" ++ e
          _ -> do
              hClose outH -- we don't need the contents 
              let asWords = (map words . lines) (e  ++ "\n" ++ out)
              let (storePath :: String) = head [s | ("path":"is":s:_) <- asWords ]
              let (hash :: String) = case [s | ("hash":"is":s:_) <- asWords ] of
                                          [x] -> x
                                          [] -> last $ last asWords
              when (not $ (all (`elem` (['0'..'9'] ++ ['a'..'z'] ++ [ 'A'..'Z']))) hash) $ error $ "bad hash: " ++ hash ++ " words :" ++ (show asWords)
              modifyIORef nixCache (M.insert url (storePath, hash))
              return (filter (not .(`elem` "’‘")) storePath, hash)

  case p of
    Just t@(p,_) -> do
      de <- doesExist p
      if de then return t
            else retrieve
    Nothing -> retrieve

doesExist p = liftM2 (||) (doesDirectoryExist p) (doesFileExist p)

withTempFile f = do
  -- d <- liftIO $ getTemporaryDirectory
  t@(n, h) <- liftIO $ openTempFile "/tmp" "hack-nix"
  a <- uncurry f t
  liftIO $ do hClose h
              removeFile n
              return a

-- using srcUnpack phase from nix to unpack and cache the result 
unpack :: String -> ConfigR String
unpack sp =
  ( withTempFile $ \name h -> do
    d <- derivation "[]" sp "cp -ra . \\$out"
    liftIO $ do 
      hPutStr h d
      hClose h
      (_,o,err,p) <- runInteractiveProcess "nix-build" [name,"--show-trace"] Nothing Nothing
      sp' <- hGetContents o
      e <- hGetContents err
      putStrLn e
      ec <- waitForProcess p
      case ec of
        ExitSuccess -> return $ init sp' -- remove trailing \n 
        ExitFailure i -> error $ "couldn't unpack " ++ (show sp) ++ " nix-build exited with " ++ (show i) ++ "\n, derivation was:\n" ++ d
                            ++ "\n\n errors:" ++ e )


derivation :: String -> String -> String -> ConfigR String
derivation buildInputs src buildPhase = do
  allP <- asks allPackages
  return $ unlines 
          [ "let pkgs = import " ++  allP ++ " {}; in with pkgs;"
          , "pkgs.stdenv.mkDerivation {"
          , "  src = " ++ quot src ++ ";"
          , "  buildInputs = " ++ buildInputs ++ ";"
          , "  phases = [\"unpackPhase\" \"buildPhase\"];"
          , "  buildPhase = \"" ++ buildPhase ++ "\";"
          , "  system = __currentSystem;"
          , "  name = " ++ quot ("hack-nix-temp") ++ ";"
          , "}"
          ]

  where quot = (\s -> "\"" ++ s ++ "\"") -- sufficient 
