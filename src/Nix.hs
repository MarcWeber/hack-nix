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

{-# NOINLINE nixCache #-}
nixCache :: IORef (M.Map String (String, String)) -- url (path, hash) 
nixCache = unsafePerformIO $ newIORef undefined

loadNixCache :: IO ()
loadNixCache = do
  cacheFile <-  hashCacheFile
  de <- doesFileExist cacheFile
  writeIORef nixCache . M.fromList =<< if de
      then liftM (map read . lines ) $ readFile' cacheFile
      else return []

saveNixCache :: IO ()
saveNixCache = do
    cacheFile <-  hashCacheFile
    writeFile cacheFile . unlines . map show . M.toList =<< readIORef nixCache

-- | downloads the url using nix-prefetch url and puts the (url , nix/store) path tuple into hashCacheFile 
--  so that it only has to be fetched once
--  could have used (url,hash), but I was to lazy to look the path up...
downloadCached :: String -> Bool -> IO (String, String) -- (storepath, hash) 
downloadCached url ignoreCached = do
  p <- if ignoreCached
      then return Nothing
      else liftM (M.lookup url) $ readIORef nixCache

  let retrieve = do 
        putStr $ "retrieving " ++ url ++ " using nix-prefetch-url ..  \n\n"
        (inH, outH, errH, p) <- runInteractiveProcess "nix-prefetch-url" [url] Nothing Nothing
        e <- hGetContents errH
        putStrLn e
        ec <- waitForProcess p
        putStrLn "done"
        case ec of 
          ExitFailure _ -> error $ "nix-prefetch-url could not download '" ++ url ++ "' cause :\n" ++ e
          _ -> do
              hClose outH -- we don't need the contents 
              let asWords = (map words . lines) e 
              let (storePath :: String) = head [s | ("path":"is":s:_) <- asWords ]
              let (hash :: String) = head [s | ("hash":"is":s:_) <- asWords ]
              modifyIORef nixCache (M.insert url (storePath, hash))
              return (storePath, hash)

  case p of
    Just t -> return $ t
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
