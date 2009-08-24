module Utils where
import Distribution.Package
import Distribution.Version

matchDepndencyConstraints :: Dependency -> PackageIdentifier -> Bool
matchDepndencyConstraints (Dependency name vRange) (PackageIdentifier pName pVersion) =
  pName == name && withinRange pVersion vRange
