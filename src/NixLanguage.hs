{-# OPTIONS_GHC -XTypeSynonymInstances -XOverlappingInstances #-}
-- quick and dirty way to write nix expressions
module NixLanguage (
  NixType(..)
  , TypeToNix(..)
  , nixEq, nixAnd, nixOr
  , keyValue
  , toNixLabel
  , ToDoc(..)
) where
import qualified Data.Map as M
import Data.List
import Text.PrettyPrint
import Data.Maybe (fromJust)

class TypeToNix a where
  toNix :: a -> NixType

instance (TypeToNix a) => TypeToNix (Maybe a) where
  toNix Nothing = NixNull
  toNix (Just a) = NixAttrs [] $ M.fromList [("J", toNix a)]

instance (TypeToNix a) => TypeToNix [a] where
  toNix = NixList . map toNix

instance TypeToNix Int where toNix = NixInt

instance (TypeToNix a, TypeToNix b) => TypeToNix (a,b) where
  toNix (a,b) = NixList [ toNix a, toNix b]
  -- toNix (a,b) = NixAttrs [] $ M.fromList [ ("t21", toNix a), ("t22", toNix b) ]

instance (TypeToNix a, TypeToNix b, TypeToNix c) => TypeToNix (a,b,c) where
  toNix (a,b,c) = NixList [ toNix a, toNix b, toNix c]
  -- toNix (a,b,c ) = NixAttrs [] $ M.fromList [ ("t31", toNix a), ("t32", toNix b), ("t33", toNix c) ]

instance TypeToNix Bool where
  toNix = NixBool

keyValue :: String -> NixType -> NixType
keyValue a v = NixAttrs [] $ M.singleton a v

data NixType = NixString String
            -- | NixUrl -- no longer used by nix
            | NixList [NixType]
            | NixAttrs [String] (M.Map String NixType) -- ^ 
            | NixAttrsRec [String] (M.Map String NixType) -- ^ (rec {})
            | NixFilePath FilePath -- ^ just a string but not quoted / this should be absolute (?)
            | NixInt Int
            | NixBool Bool
            | NixNull
            | NixOp String NixType NixType -- ^ eg && 
            | NixVerb String
            | NixFunction String NixType -- string : function body
            | NixApply String [NixType] -- name args
  deriving (Eq,Ord)

nixEq, nixAnd, nixOr :: NixType -> NixType -> NixType
nixEq = NixOp "=="
nixAnd = NixOp "&&"
nixOr = NixOp "||"

instance Show NixType where 
  show (NixString s) = show s
  show (NixList l) = "[" ++ concInter " " (map ( (\c -> "(" ++ c ++ ")") . show) l ) ++ "]"
  show (NixAttrs _ set) = "{" ++ concInter " " [ name ++ "=" ++ (show value) ++ ";\n" -- I'm inserting \n here so that we'll get many lines. I hope this enhances the way svn can handle diffs
                                     | (name,value) <- M.toList set ] ++ "}"
  show (NixAttrsRec ordering set) = "rec " ++ show (NixAttrs ordering set)
  show (NixFilePath fp) = fp
  show (NixInt i) = (show i)
  show (NixBool True) = "true"
  show (NixBool False) = "false"
  show NixNull = "null"
  show (NixApply fun args) = "(" ++ fun ++ " " ++ (concat $ intersperse " " (map ((\s -> "(" ++s ++")").show) args)) ++ ")"
  show (NixOp s e1 e2) = "(" ++ (show e1) ++ " " ++ s ++ " " ++ (show e2) ++ ")"
  show (NixFunction var expr) = var ++ " : " ++ show expr
  show (NixVerb s) = s

concInter :: [Char] -> [[Char]] -> [Char]
concInter a = concat . intersperse a

-- same as M.toList but preserves ordering. Not all keys must be present in
-- list. keys which are not present in map are ignored
attrsToList :: NixType -> [(String, NixType)]
attrsToList (NixAttrs ordering map') =
  let validOrderingKeys = filter ((flip M.member) map') ordering
  in [ (k, fromJust (M.lookup k map')) | k <- nub $ validOrderingKeys ++ M.keys map' ]
attrsToList _ = error "invalid argument"

--  ====================================== 
--  pretty printing 
class ToDoc a where
  toDoc :: a -> Doc

needsParens :: NixType -> Bool
needsParens (NixFunction _ _) = True
needsParens (NixApply _ _) = True
needsParens  _ = False

addParensIfNeeded :: NixType -> Doc
addParensIfNeeded i = 
  let d = toDoc i
  in if needsParens i then text "(" <+> d <+> text ")" else d

kV k v = fcat [ (text k), text " = ", (toDoc v) <> text ";" ]

instance ToDoc NixType where
  toDoc (NixString s) = text $ show s
  toDoc (NixList []) = text "[]"
  toDoc (NixList [x]) = char '[' <> addParensIfNeeded x <> char ']'
  toDoc (NixList l) =
              let (i:is) = map addParensIfNeeded l
              in  -- text "[" <+> items <+> text "]"
                  cat [ char '['
                      , fcat (nest 2 i : [text "  " <> d | d <- is])
                      , char ']'
                      ]
  toDoc a@(NixAttrs _ _) = attrsToDoc $ attrsToList a
    where
      attrsToDoc [] = text "{}"
      attrsToDoc [(k,v)] = char '{' <> kV k v <> char '}'
      attrsToDoc l =
                  let (i:is) = map (uncurry kV) l
                  in  -- text "[" <+> items <+> text "]"
                      cat [ char '{'
                          , fcat (nest 2 i : [text "  " <> d | d <- is])
                          , char '}'
                          ]
  {-
  toDoc (NixAttrs set) = let keyVs = (nest 2 . vcat) $ map (\(k, v) -> (text k) <+> text "=" <+> (toDoc v) <> text ";" ) (M.toList set)
                         in  text "{" $+$ keyVs $+$ text "}"
  -}
  toDoc (NixAttrsRec ordering set) = text "rec" <+> toDoc (NixAttrs ordering set)
  toDoc (NixFilePath fp) = text fp
  toDoc (NixInt i) = int i
  toDoc (NixBool True) = text "true"
  toDoc (NixBool False) = text "false"
  toDoc NixNull = text "null"
  -- TODO: 
  toDoc (NixApply fun args) = (text fun) <+> vcat (map addParensIfNeeded args)
  toDoc (NixOp s e1 e2) = text $ "(" ++ (show e1) ++ " " ++ s ++ " " ++ (show e2) ++ ")"
  toDoc (NixFunction var expr) = text var <+> char ':' <+> toDoc expr
  toDoc (NixVerb s) = text $ s
  -- TODO end 

toNixLabel :: String -> String
toNixLabel = map (\c -> if c `elem` "-" then '_' else c)
