{-# OPTIONS_GHC -XFlexibleContexts -XUndecidableInstances -XPatternGuards -XFlexibleInstances -XOverlappingInstances #-}
module NixLangUtil where
import Distribution.Compiler
import Nix
import NixLanguage
import Distribution.PackageDescription
import Distribution.Version
import Distribution.Package
import Distribution.System
import qualified Data.Map as M
import Data.Version
import Data.List (intercalate)
import Data.Maybe (maybeToList,fromMaybe)

--  about 5% size could be saved by looking up package names from a dictionary with int keys

{-
instance TypeToNix Flag where
  toNix (MkFlag (FlagName flagName) flagDescription flagDefault flagManual) = NixAttrs [] $ M.fromList [
      ("name", NixString flagName)
    , ("description", NixString flagDescription) don't serialize the description, it takes up to much space..
    , ("default", NixBool flagDefault)
    -- ("manual", NixBool flagDefault)
    ]
-}

instance TypeToNix OS where
  toNix Linux = NixString "Linux"
  toNix Windows = NixString "Windows"
  toNix OSX= NixString "OSX"
  toNix FreeBSD = NixString "FreeBSD"
  toNix OpenBSD = NixString "OpenBSD"
  toNix NetBSD= NixString "NetBSD"
  toNix Solaris = NixString "Solaris"
  toNix AIX = NixString "AIX"
  toNix HPUX = NixString "HPUX"
  toNix IRIX= NixString "IRIX"
  toNix (OtherOS s) = NixString $ "other:" ++ s

instance TypeToNix CompilerFlavor where
  toNix GHC = NixString "GHC"
  toNix NHC = NixString "NHC"
  toNix YHC = NixString "YHC"
  toNix Hugs = NixString "Hugs"
  toNix HBC = NixString "HBC"
  toNix Helium = NixString "Helium"
  toNix JHC= NixString "JHC"
  toNix (OtherCompiler o) = NixString $ "other:" ++ o

instance TypeToNix Arch where
  toNix I386   = NixString "I386"
  toNix X86_64 = NixString "X86_64"
  toNix PPC    = NixString "PPC"
  toNix PPC64  = NixString "PPC64"
  toNix Sparc  = NixString "Sparc"
  toNix Arm    = NixString "Arm"
  toNix Mips   = NixString "Mips"
  toNix SH     = NixString "SH"
  toNix IA64   = NixString "IA64"
  toNix S390   = NixString "S390"
  toNix Alpha  = NixString "Alpha"
  toNix Hppa   = NixString "Hppa"
  toNix Rs6000 = NixString "Rs6000"
  toNix M68k   = NixString "M68k"
  toNix Vax    = NixString "Vax"
  toNix (OtherArch a) = NixString $ "other:" ++ a

instance TypeToNix FlagName where
  toNix (FlagName f) = NixString f

instance TypeToNix ConfVar where
  toNix (OS os) = keyValue "os" $ toNix os
  toNix (Arch arch) = keyValue "arch" $ toNix arch
  toNix (Flag (FlagName flagName)) = keyValue "flag" $ NixString $ toNixLabel flagName
  toNix (Impl compilerFlavor versionRange) = NixAttrs [] $ M.fromList $ [
        ("compilerFlavor", toNix compilerFlavor)
      , ("versionRange", toNix versionRange)
    ]

nixVersion numbers = NixString $ intercalate "." $ map show numbers

instance TypeToNix Version where
  toNix (Version numbers _) = nixVersion numbers

opVersion :: String -> NixType -> NixType
opVersion op v = NixAttrs [] $ M.fromList [("op", NixString op), ("v", v)]

instance TypeToNix VersionRange where
  toNix AnyVersion = NixAttrs [] M.empty;
  toNix (ThisVersion version) = NixAttrs [] $ M.fromList [("v", toNix version)]
                                -- no version in output means any version, keep
                                -- it simple, there are 1500 pacakges (latest
                                -- version only !)
  toNix (LaterVersion version) =  NixAttrs [] $ M.fromList [("gt", toNix version)]
  toNix (EarlierVersion version) = NixAttrs [] $ M.fromList [("lt", toNix version)]
  -- encode wildarcd as version range
  toNix (WildcardVersion (Version version _)) =
		NixAttrs [] $ M.fromList [("i1", lower), ("i2", upper)]
     where lower = NixAttrs [] $ M.fromList [("le", nixVersion $ init version ++ [1 + last version])]
           upper = NixAttrs [] $ M.fromList [("gte", nixVersion version)]


  --  Build-Depends: base       >= 4 is represented as    (== 4) union (> 4) so rewrite this as greater or equal (gte)
  toNix (UnionVersionRanges v1 v2)
      | ThisVersion va  <- v1
      , LaterVersion vb <- v2
      , va == vb = NixAttrs [] $ M.fromList [("gte", toNix va)]

  -- same for (== 4) union (< 4) 
  toNix (UnionVersionRanges v1 v2)
      | ThisVersion va  <- v1
      , EarlierVersion vb <- v2
      , va == vb = NixAttrs [] $ M.fromList [("lte", toNix va)]

  toNix (UnionVersionRanges v1 v2)
      = NixAttrs [] $ M.fromList [("u1", toNix v1), ("u2", toNix v2)]

  toNix (IntersectVersionRanges v1 v2) = NixAttrs [] $ M.fromList [("i1", toNix v1), ("i2", toNix v2)]


instance (TypeToNix a, TypeToNix b, TypeToNix c) => TypeToNix (a,b,Maybe c) where
  toNix (a,b,c) = NixList $ [ toNix a, toNix b ] ++ case c of
      Just x -> [ toNix x ]
      _ -> []

instance ( TypeToNix v, TypeToNix c, TypeToNix a
         , TypeToNix (Condition v, CondTree v c a, Maybe (CondTree v c a)) )
     => (TypeToNix (CondTree v c a)) where
  toNix (CondNode d c components) = NixAttrs [] $ M.fromList $ [
      -- ("data", toNix d) this only contains stuff such as "exposed modules.." we're not interested in
        ("deps", toNix c) -- deps = constraints. That's shorter 
      , ("cdeps", toNix components) -- (i) cdeps = "conditional deps" - this is shorter 
    ]

gtk2hsMetaPkgName = "gtk2hs-meta-package-hack"
gtk2hsHack "svgcairo" = gtk2hsMetaPkgName
gtk2hsHack "glib" = gtk2hsMetaPkgName
gtk2hsHack "cairo" = gtk2hsMetaPkgName
gtk2hsHack "gtk2hs" = gtk2hsMetaPkgName
gtk2hsHack "soegtk" = gtk2hsMetaPkgName
gtk2hsHack "gio" = gtk2hsMetaPkgName
gtk2hsHack "gtksourceview2" = gtk2hsMetaPkgName
gtk2hsHack "glade" = gtk2hsMetaPkgName
gtk2hsHack "gtk" = gtk2hsMetaPkgName
gtk2hsHack s = s

-- returns
-- { n = "Cabal"; v = [ 1 4 4 0=; }
-- { n = "Cabal; }  = any version
-- ...
instance TypeToNix Dependency where
  toNix (Dependency (PackageName packageName) versionRange) = 
    let (NixAttrs _ map') = toNix versionRange
    in NixAttrs [] $ M.fromList [ ("n", NixString (gtk2hsHack packageName)) ] `M.union` map'
  
instance TypeToNix Executable where
  toNix (Executable name _ _) = NixString name

instance TypeToNix Library where
  toNix _ = NixString "library"

-- special instance for flag names 
instance TypeToNix (Condition String) where
  -- strip this "var" to save some bytes, the "os", "arch", "flag", "compilerFlavor" names are uniq enough
  toNix (Var c) = {- keyValue "bool" $ -} NixString $ toNixLabel c
  toNix (Lit bool) = keyValue "bool" $ toNix bool
  toNix (CNot c) = keyValue "not" $ toNix c
  toNix (COr c1 c2) = keyValue "or" $ toNix [ c1, c2 ]
  toNix (CAnd c1 c2) = keyValue "and" $ toNix [ c1, c2 ]

instance (TypeToNix a) => TypeToNix (Condition a) where
  -- strip this "var" to save some bytes, the "os", "arch", "flag", "compilerFlavor" names are uniq enough
  toNix (Var c) = {- keyValue "bool" $ -} toNix c
  toNix (Lit bool) = keyValue "bool" $ toNix bool
  toNix (CNot c) = keyValue "not" $ toNix c
  toNix (COr c1 c2) = keyValue "or" $ toNix [ c1, c2 ]
  toNix (CAnd c1 c2) = keyValue "and" $ toNix [ c1, c2 ]

hackageSrcUrl name versionStr =
  "http://hackage.haskell.org/packages/archive/" ++ name ++ "/" ++ versionStr ++ "/" ++ name ++ "-" ++ versionStr ++ ".tar.gz"

data SourceType =
    STHackage       -- get source from hackage. url is generated in nix code, only add hash
  | STFile FilePath -- use this file 
  | STFetchUrl FilePath String -- file:// uri and sha256
  | STNone -- for testing only 

packageDescriptionToNix :: SourceType -> GenericPackageDescription -> IO NixType
packageDescriptionToNix st (GenericPackageDescription packageDescription' genPackageFlags' condLibrary' condExecutables') = do
  let PackageIdentifier (PackageName name) version = package packageDescription'
  let versionNumbers = versionBranch version
  let versionStr = intercalate "." (map show versionNumbers)
  let url = hackageSrcUrl name versionStr
  srcAttrs <-
     case st of
        STHackage -> do
          (_, hash) <- downloadCached url False
          return  [("sha256", NixString hash)]

        STFile file ->
          return [("srcFile", NixString file)] -- be careful. Nix does not always recognize that this file has changed!

        STFetchUrl url hash -> do
          (_, hash) <- downloadCached url False
          return [("sha256", NixString hash), ("url", NixString url)]

        STNone -> return []

  return $ NixAttrs ["name", "version"] $ M.fromList $ [
        ("name", NixString name)
      , ("version", nixVersion versionNumbers)
      ] ++ srcAttrs
      ++ (
      -- 450 flags: False
      -- 99  flags: True Okt 2009
      -- thus only store True default values as list
        let list = [ NixString flagName
                   | (MkFlag (FlagName flagName) flagDescription flagDefault flagManual) <- genPackageFlags'
                   , flagDefault -- is True
                   ]
        in if null list then []
            else [ ("tflags", NixList list ) ]
        )
      -- dependencies of library
      ++ (fromMaybe [] (fmap (\x -> [("ldeps", toNix x)]) condLibrary'))
      -- dependencies of executables
      ++ [("edeps", (toNix . map snd) condExecutables')]
