module BuildEnvs where
import Control.Monad.Reader.Class
import System.FilePath
import Text.PrettyPrint
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

writeHackNixCabalConfig :: ConfigR ()
writeHackNixCabalConfig = do
  -- never overwrite a config. Append instead 
  h <- liftIO $ openFile hackNixCabalConfig AppendMode

  -- first is default 
  pd <- parseCabalFileCurrentDir

  let flags = [ (flagName, flagDefault)
             | (MkFlag (FlagName flagName) flagDescription flagDefault flagManual) <- genPackageFlags pd
             ]
  liftIO $ print flags

  -- calculate all flag combinations 
  let combinations =
        foldr (\n e -> e >>= n) [[]] [ (\l -> [ (name, def):l, (name, not def):l ]) | (name, def) <- flags ]

  let names = "default":(map ( ("way" ++ ) . show) [2..])
  let header = "# generated lines:\n"
  liftIO $ do
    hPutStrLn h $
      if null combinations then
        header ++ "default:\n"
      else header ++ (unlines $ zipWith (\n flags -> n ++ ":" ++ flagsToString flags) names combinations)
    hClose h

  where flagsToString list =
          intercalate " " [ (if value then "" else "-") ++ name | (name, value) <- list ]


-- runs ./[Ss]etup dist
-- and creates dist/name.nix 
packageToNix :: ConfigR FilePath
packageToNix = do
  pd <- parseCabalFileCurrentDir
  setupE <- liftIO $ findSetup
  (inH, outH, errH, p) <- liftIO $ runInteractiveProcess ("./"++setupE) ["sdist"] Nothing Nothing
  e <- liftIO $ liftM lines $ hGetContents outH
  ec <- liftIO $ waitForProcess p
  srcDistfile <- case ec of
    ExitFailure (ec) -> do
      liftIO $ hPutStrLn stderr $ "./[sS]etup sdist failed with exit code " ++ (show ec)
      return $ "failure"
    ExitSuccess -> do
      let pref = "Source tarball created: "
      let distFile = drop (length pref) $ head $ filter (pref `isPrefixOf`) e
      pwd <- liftIO $ getCurrentDirectory
      return $ pwd ++ "/" ++ distFile

  nixT <- liftIO $ packageDescriptionToNix (STFile srcDistfile ) $ pd
  let pD = packageDescription $ pd
  let (PackageIdentifier (PackageName name) version) = package pD
  let nixFile = "dist/" ++ name ++ ".nix"
  liftIO $ writeFile nixFile  (renderStyle style $ toDoc $ nixT)
  return $ nixFile


buildEnv :: String -> ConfigR ()
buildEnv envName = do
  let readFlag ('-':envName) = (envName, False)
      readFlag envName = (envName, True)
      rmComments =  (filter (not . ("#" `isPrefixOf`)))
      splitLine l = case break (== ':') l of
        (envName, (':':flags)) -> Right (envName, (map readFlag . words) flags, flags)
        r -> Left r

  -- if file doesn't exist assume "default: contents
  de <- liftIO $ doesFileExist hackNixCabalConfig
  fc <- if de then
          liftIO $ liftM ( rmComments . lines) $ readFile hackNixCabalConfig
        else return ["default:"]

  case filter ( (envName++":") `isPrefixOf`) fc of
    [] -> liftIO $ die $ unlines [
                 "configuration with envName " ++ envName ++ "not found in" ++ hackNixCabalConfig,
                 "I know about these names: " ++ show [ envName | Right (envName, _, _)  <- map splitLine fc ]
                ]
    (h:x:_) -> liftIO $ die $ "envName " ++ envName ++ " is defined multiple times"
    (h:_) -> do
      case  splitLine h of
        Left s -> liftIO $ die $ "can't read config line: " ++ h ++ " result : " ++ show s
        Right (envName, flags, flagsStr') -> do

          -- build dist file more important write .cabal file in a nix readable format: 
          thisPkgNixFile <- packageToNix

          let nixFilesDir = (hackNixEnvs </> "nix")
              nixFile = nixFilesDir </> envName ++ ".nix"
              thisPkgNixFile9 = nixFilesDir </> envName ++ "9.nix"

          liftIO $ createDirectoryIfMissing True nixFilesDir

          liftIO $ copyFile thisPkgNixFile thisPkgNixFile9

          -- make this package uniq by assigning version 99999 
          liftIO $ run (Just 0) "sed" [ "-i", "s@version = \"[^\"]*\";@version=\"99999\";@", thisPkgNixFile9] Nothing Nothing
          -- I should use regexp package or such - get the job done for now

          overlayRepo <- asks haskellNixOverlay
          
          pd <- parseCabalFileCurrentDir
          
          let PackageIdentifier (PackageName pName) version = package $ packageDescription pd
              flagsStr = intercalate " " [ "{ n = \"" ++ n ++ "\"; v =" ++ (if set then "true" else "false") ++ ";}" | (n, set) <- flags]

          cht <- asks createHaskellTags 
          let tagOptions = case cht of
                TTNone -> ["         # no tags"]
                TTVim  -> ["         createHaskellTagsFor = pkg.propagatedBuildInputs",
                           "                              ++ [ (pkgs.haskellPackages.ghcReal // { srcDir = \"libraries compiler/main\"; })",
                           "                                   (pkgs.haskellPackages.ghcReal // { srcDir = \"compiler/main\"; })",
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
               "    pkg = builtins.getAttr \"" ++ pName ++ "\" (nixOverlay.haskellOverlayPackagesFun.merge (args: args // {",
               "      targetPackages = [{ n = \"" ++ pName ++ "\"; v = \"99999\"; }];",
               "      packageFlags = args.packageFlags // lib.attrSingleton \"" ++ pName ++ "-99999\" pkgFlags;",
               "      packages = args.packages ++ [ (nixOverlay.libOverlay.pkgFromDb (import ./" ++ takeFileName thisPkgNixFile9 ++ ")) ];",
               "      debugS = true;",
               "    })).result;",
               "in {",
               "      env = nixOverlay.envFromHaskellLibs {"
               ] ++ tagOptions ++ [
               "         buildInputs = pkg.buildInputs ++ pkg.propagatedBuildInputs;",
               "      };",
               "   }"
            ]
          
          nixFlags <- asks nixFlags
          liftIO $ do
            let envPath = (hackNixEnvs </> envName)
            run (Just 0) "nix-env" (["-p", envPath, "-iA", "env", "-f", nixFile, "--show-trace"] ++ nixFlags) Nothing Nothing 
            putStrLn $ unlines [
                "success:",
                "# source:",
                "source " ++ envPath ++ "/source-me/haskell-env",
                "# and configure",
                "[ -e Setup ] || ghc --make Setup.hs",
                "./Setup configure --flags \"" ++ flagsStr' ++ "\" && ./Setup build "
              ]
