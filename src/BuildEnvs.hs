{-# OPTIONS -cpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BuildEnvs where
import System.IO.Unsafe
import qualified Data.Map as M
import Control.Exception
import Data.Maybe
import Control.Monad.Reader.Class
import System.FilePath
import Text.PrettyPrint
import GHC.MVar
import NixLangUtil
import NixLanguage
import Data.List
import System.Process
import System.Directory
import System.Exit
import Control.Monad
import Utils
import Config
import Control.Monad.Trans
import System.IO
import Distribution.PackageDescription
import Distribution.Package
import Interlude
#include "interlude.h"
-- writes ".hack-nix-cabal-config" sample file
writeHackNixCabalConfig :: ConfigR ()
writeHackNixCabalConfig = do
  -- never overwrite a config. Append instead 
  h <- liftIO $ openFile hackNixCabalConfig AppendMode

  -- first is default 
  pd <- parseCabalFileCurrentDir

  let flags = [ (flagName', flagDefault')
             | (MkFlag (FlagName flagName') _ flagDefault' _) <- genPackageFlags pd
             ]
  liftIO $ print flags

  -- calculate all flag combinations 
  let combinations =
        foldr (\n e -> e >>= n) [[]] [ (\l -> [ (name, def):l, (name, not def):l ]) | (name, def) <- flags ]

  let names = "default":(map ( ("way" ++ ) . show) [(2::Int)..])
  let header = "# generated lines:\n"
  liftIO $ do
    hPutStrLn h $
      if null combinations then
        header ++ "default:[]\n"
      else header ++ (unlines $ zipWith (\n flags' -> n ++ ":" ++ show [("haskellPackages", defaultHaskellPackages),("flags", flagsToString flags')]) names combinations)
    hClose h

  where flagsToString list =
          intercalate " " [ (if value then "" else "-") ++ name | (name, value) <- list ]


-- runs ./[Ss]etup dist
-- and creates dist/name.nix 
packageToNix :: ConfigR FilePath
packageToNix = do
  pd <- parseCabalFileCurrentDir
  setupE <- liftIO $ findSetup
  (_, outH, _, p) <- liftIO $ runInteractiveProcess ("./"++setupE) ["sdist"] Nothing Nothing
  e <- liftIO $ liftM lines $ hGetContents outH
  ec <- liftIO $ waitForProcess p
  srcDistfile <- case ec of
    ExitFailure (ec') -> do
      liftIO $ hPutStrLn stderr $ "./[sS]etup sdist failed with exit code " ++ (show ec')
      return $ "failure"
    ExitSuccess -> do
      let pref = "Source tarball created:"
      let distFile = case filter (pref `isPrefixOf`) e of
            [] -> error "expected cabal outputting line starting with \"" ++ pref ++ "\""
            x':xs' -> case drop (length pref) x' of
                       -- dist file next line
                       "" -> -- dist file next line
                             head . drop 1 $ xs'
                       (' ':xs'') -> xs''
                       _ -> error "unexpected in BuildEnvs"
      pwd <- liftIO $ getCurrentDirectory
      return $ pwd ++ "/" ++ distFile

  -- TODO refactor. Add createFetchUrl src func or such 
  nixT <- liftIO $ packageDescriptionToNix (STFetchUrl ("file://"++srcDistfile) (error "unsued")) $ pd
  let pD = packageDescription $ pd
  let (PackageIdentifier (PackageName name) _) = package pD
  let nixFile = "dist/" ++ name ++ ".nix"
  liftIO $ writeFile nixFile  (renderStyle style $ toDoc $ nixT)
  liftIO $ putStrLn nixFile
  return $ nixFile


-- j is concurrency of nix-env command 
buildEnv :: String -> [String] -> ConfigR ()
buildEnv envName nixEnvArgs = do
  let readFlag ('-':envName') = (envName', False)
      readFlag envName' = (envName', True)
      rmComments =  (filter (not . ("#" `isPrefixOf`)))
      splitLine l = case break (== ':') l of
        (envName', (':':options')) ->
              let -- grr 
                  map' = unsafePerformIO $ handle (\(_::SomeException) -> error $ "error parsing options :" ++ options') $ return $ read options'
                  flagsS = fromMaybe "" (lookup "flags" map')
              in Right (envName', \s d-> fromMaybe d (lookup s map'), (map readFlag . words) flagsS, flagsS)
        r -> Left (show r)

  -- if file doesn't exist assume "default: contents
  de <- liftIO $ doesFileExist hackNixCabalConfig
  fc <- if de then
          liftIO $ liftM ( rmComments . lines) $ readFile hackNixCabalConfig
        else return ["default:[(\"haskellPackages\",\"" ++ defaultHaskellPackages ++ "\")]"]

  case filter ( (envName++":") `isPrefixOf`) fc of
    [] -> liftIO $ die $ unlines [
                 "configuration with envName " ++ envName ++ " not found in" ++ hackNixCabalConfig,
                 "I know about these names: " ++ show [ envName' | Right (envName', _, _, _)  <- map splitLine fc ]
                ]
    (_:_:_) -> liftIO $ die $ "envName " ++ envName ++ " is defined multiple times"
    (h:_) -> do
      case  splitLine h of
        Left s -> liftIO $ die $ "can't read config line: " ++ h ++ " result : " ++ show s
        Right (envName', getOpt, flags, flagsStr') -> do

          -- build dist file more important write .cabal file in a nix readable format: 
          thisPkgNixFile <- packageToNix

          let nixFilesDir = (hackNixEnvs </> "nix")
              nixFile = nixFilesDir </> envName' ++ ".nix"
              thisPkgNixFile9 = nixFilesDir </> envName' ++ "9.nix"

          liftIO $ createDirectoryIfMissing True nixFilesDir

          liftIO $ copyFile thisPkgNixFile thisPkgNixFile9

          -- make this package uniq by assigning version 99999 
          liftIO $ run (Just 0) "sed" [ "-i", "s@version = \"[^\"]*\";@version=\"99999\";@", thisPkgNixFile9] Nothing Nothing
          -- I should use regexp package or such - get the job done for now

          overlayRepo <- asks haskellNixOverlay
          
          pd <- parseCabalFileCurrentDir
          
          let PackageIdentifier (PackageName pName) _ = package $ packageDescription pd
              flagsStr = intercalate " " [ "{ n = \"" ++ n ++ "\"; v =" ++ (if set then "true" else "false") ++ ";}" | (n, set) <- flags]
              haskellPackagesToUse = getOpt "haskellPackages" defaultHaskellPackages
              mergeWith = case getOpt "mergeWith" "" of
                "" -> ""
                file -> ".merge (import ../../" ++ file ++ ")"

          cht <- asks createHaskellTags 
          let tagOptions = case cht of
                TTNone -> ["         # no tags"]
                TTVim  -> ["         createHaskellTagsFor = pkg.deps",
                           "                              ++ [ (" ++ haskellPackagesToUse ++ ".ghcReal // { srcDir = \"libraries compiler/main\"; })",
                           "                                   (" ++ haskellPackagesToUse ++ ".ghcReal // { srcDir = \"compiler/main\"; })",
                           "                                 ];"
                         ]
                TTEmacs ->["         # creating tags for Emacs is not supperted yet (FIXME)" ]

          -- I'm too lazy to get all dependencies of this .cabal file as well. 
          -- so built cabal package from dist/full-name.nix and use its buildInputs and propagatedBuildInputs value
          liftIO $ writeFile nixFile $ unlines $ [
               "let nixOverlay = import \"" ++ overlayRepo ++ "\" {};",
               "    lib = nixOverlay.lib;",
               "    pkgs = nixOverlay.pkgs;",
               "    pkgFlags = lib.fold (a: n: a // n) {} (map ({n, v}: lib.attrSingleton n v) [" ++ flagsStr ++ " ]);",
               "    pkg = builtins.getAttr \"" ++ pName ++ "\" ((nixOverlay.haskellOverlayPackagesFun.merge (args: args // {",
               "      targetPackages = [{ n = \"" ++ pName ++ "\"; v = \"99999\"; }];",
               "      packageFlags = args.packageFlags // lib.attrSingleton \"" ++ pName ++ "-99999\" pkgFlags;",
               "      packages = args.packages ++ [ (nixOverlay.libOverlay.pkgFromDb (import ./" ++ takeFileName thisPkgNixFile9 ++ ")) ];",
               "      haskellPackages = " ++ haskellPackagesToUse ++ ";",
               "      debugS = true;",
               "    }))" ++ mergeWith ++ ").result;",
               "in {",
               "      env = nixOverlay.envFromHaskellLibs {"
               ] ++ tagOptions ++ [
               "         buildInputs = [ " ++ haskellPackagesToUse ++ ".ghc ] ++ pkg.buildInputs ++ pkg.deps;",
               "      };",
               "   }"
            ]
          
          nixFlags' <- asks nixFlags
          liftIO $ do
            let envPath = (hackNixEnvs </> envName')
            let buildDir = if envName' == "default" then "" else "--builddir=dist" ++ envName'
            run (Just 0) "nix-env" (["-p", envPath, "-iA", "env", "-f", nixFile, "--show-trace"] ++ nixEnvArgs ++ nixFlags') Nothing Nothing 
            let configureLines = [
                    "[ -e Setup ] || ghc --make Setup.*hs",
                    "./Setup clean " ++ buildDir,
                    "./Setup configure " ++ buildDir ++ " --flags \"" ++ flagsStr' ++ "\" && ./Setup build " ++ buildDir
                  ] 
                quickSourceme = envName' ++ "-env"
            putStrLn $ unlines $ [
                "success:",
                "",
                "# source:",
                "source " ++ envPath ++ "/source-me/haskell-env",
                "# and configure"
              ] ++ configureLines ++ [
                "",
                "you can source this env by running:",
                ". ./" ++ quickSourceme
              ]

            writeFile quickSourceme $ unlines $ [
                    "source " ++ envPath ++ "/source-me/haskell-env",
                    "echo 'You can copy paste and run:'"
                  ] ++ map (\s -> "echo '" ++ s ++ "'") configureLines
