{-# OPTIONS_GHC -XPatternSignatures #-}
-- uses the file .nix-cache to associate given urls with hash values 
-- the .nix-cache file line fromat is
-- ("url", "storepath")
--
-- using /nix/store as storage
module Nix where
import Data.List
import System.Directory
import System.IO
import Config
import System.Process
import Control.Monad
import Control.Monad.Reader
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS

newtype StorePath = SP String
instance Show StorePath where show (SP a) = a


-- | downloads the url using nix-prefetch url and puts the (url , nix/store) path tuple into hashCacheFile 
--  so that it only has to be fetched once
--  could have used (url,hash), but I was to lazy to look the path up...
downloadCached :: String -> ConfigR StorePath
downloadCached url = liftIO $ do
  de <- doesFileExist hashCacheFile
  f <- if de 
      then liftM (map read . lines . BS.unpack) $ BS.readFile hashCacheFile -- strict 
      else return []
  p <- case lookup url f of
          (Just p) -> liftM (\de -> if de then Just p else Nothing) $ doesExist p
          Nothing ->  return Nothing
  case p of
    Just p -> return $ SP p
    Nothing -> do putStr $ "retrieving " ++ url ++ " using nix-prefetch-url ..  \n\n"
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
                        -- let (hash :: String) = head [s | ("hash":"is":s:_) <- asWords ]
                        writeFile hashCacheFile $ unlines $ map show $ (url, storePath): filter ( (/=url) . fst) f
                        return $ SP $ storePath

doesExist p = liftM2 (||) (doesDirectoryExist p) (doesFileExist p)

withTempFile f = do
  -- d <- liftIO $ getTemporaryDirectory
  t@(n, h) <- liftIO $ openTempFile "/tmp" "hack-nix"
  a <- uncurry f t
  liftIO $ do hClose h
              removeFile n
              return a

-- using srcUnpack phase from nix to unpack and cache the result 
unpack :: StorePath -> ConfigR StorePath
unpack sp =
  ( withTempFile $ \name h -> do
    d <- derivation "[]" (show sp) "cp -ra . \\$out"
    liftIO $ do 
      hPutStr h d
      hClose h
      (_,o,err,p) <- runInteractiveProcess "nix-build" [name] Nothing Nothing
      sp' <- hGetContents o
      e <- hGetContents err
      putStrLn e
      ec <- waitForProcess p
      case ec of
        ExitSuccess -> return $ SP sp'
        ExitFailure i -> error $ "couldn't unpack " ++ (show sp) ++ " nix-build exited with " ++ (show i) ++ "\n, derivation was:\n" ++ d
                            ++ "\n\n errors:" ++ e )


derivation :: String -> String -> String -> ConfigR String
derivation buildInputs src buildPhase = do
  allP <- asks allPackages
  return $ unlines 
          [ "let pkgs = import " ++  allP ++ " {}; in with pkgs;"
          , "stdenv.mkDerivation {"
          , "  src = " ++ quot src ++ ";"
          , "  buildInputs = " ++ buildInputs ++ ";"
          , "  phases = [\"unpackPhase\" \"buildPhase\"];"
          , "  buildPhase = \"" ++ buildPhase ++ "\";"
          , "  system = __currentSystem;"
          , "  name = " ++ quot ("hack-nix-temp") ++ ";"
          , "}"
          ]

  where quot = (\s -> "\"" ++ s ++ "\"") -- sufficient 
