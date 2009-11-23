module Utils where
import Distribution.Package
import Distribution.Version
import System.Exit

matchDepndencyConstraints :: Dependency -> PackageIdentifier -> Bool
matchDepndencyConstraints (Dependency name vRange) (PackageIdentifier pName pVersion) =
  pName == name && withinRange pVersion vRange

die :: String -> IO ()
die msg = do
  putStrLn msg
  exitWith (ExitFailure 1)
