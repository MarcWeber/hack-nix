module Package where
import Data.Vorsion

-- a (cabal) package whose deps are to be resolved before writing it to disk.. 

ata Source = Url { 
              url :: String -- download (fetchurl) 
            , relPath :: Maybe String
            }
            | SourceByName {
              name ::  String -- name 
            , relPath :: Maybe String
            }

data DepP

data compilation = 
  CompiledByCabal {
    flags :: [ String ]
  }
  | Script String -- TODO needed? 

-- information about the ghc to be used
data GHC =
  GHCDist {
      version :: Version
    , coreLibs :: [ Package ]
  }
  | GHCDarcs { -- bleeding edge darcs version of ghc
      version :: Version
    , coreLibs :: [ Package ]
  }


-- a package whose dependencies are to be resolved 
data Package = 
    GHC {
      ghc :: GHC
    }
  | Package {
      name :: String
      corePackage :: Bool -- does this package belong to ghc core (they don't have to be compiled)
      version :: [Int]
      pkgSource :: Source
      depDescription :: DepDescription
      deps :: Maybe [ Package ] -- after they have been resolved 
      compilation :: Compilation
    }

ghcInfoFromDirectory dir = do
  isDarcs <- doesFileExist $ dir </> packages
  if isDarcs then do
      -- TODO read  
    else do
  
