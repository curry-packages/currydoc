{- |
    Description: Functions to generate documentation in "CDoc" format.
    Author: Sandra Dylus
    Version: August 2025

    Converts the CurryDoc representation of a module into a
    slim CDoc representation which can be used to generate
    documentation based on the CurryDoc format (see currygle).
    The CDoc format is a simplified version of the original format
    used in CurryDoc, which is more suitable for documentation
    generation tools.
-}

module CurryDoc.Generators.CDoc where

import CurryDoc.Info.Header
import CurryDoc.Data.CurryDoc
import CurryDoc.Data.AnaInfo
import CurryDoc.Info.Comments

import qualified AbstractCurry.Types  as AC
import qualified AbstractCurry.Select as ACS
import           FlatCurry.Types
import           FlatCurry.Files
import           FlatCurry.FlexRigid
import           FlatCurry.Goodies   ( progFuncs )

import Data.List
import Data.Maybe ( catMaybes, fromMaybe )
import ReadShowTerm

import Data.Trie as T

-- | Generates the documentation of a module in "CDoc" format.
generateCDoc :: CurryDoc -> IO String
generateCDoc cd@(CurryDoc mname mhead _ _) = do
  putStrLn $ "Reading flat curry for module \"" ++ mname ++ "\"..."
  fcyProg <- getFlatCurryFileInLoadPath mname
              >>= readFlatCurryFile
  let frMap
        = buildFRMap (progFuncs fcyProg)
      modInfo
        = ModuleInfo
            mname
            (cleanupBorder $ removeNewlines $ author mhead)
            (cleanupBorder $ description mhead)
      decls
        = allCurryDocDecls cd
      curryInfo
        = buildCurryInfo frMap modInfo decls

  putStrLn $ "Writing " ++ mname ++ ".cdoc file..."
  return $ showTerm curryInfo
 where
  buildCurryInfo :: FRMap -> ModuleInfo -> [CurryDocDecl] -> CurryInfo
  buildCurryInfo frMap modInfo =
    foldr (flip (addDecl frMap)) (CurryInfo modInfo [] [])

  addDecl :: FRMap -> CurryInfo -> CurryDocDecl -> CurryInfo
  addDecl frMap ci decl = case decl of
    CurryDocFunctionDecl qn t msig anaInfo cs ->
      addFunctionInfo ci $ FunctionInfo
        (snd qn)
        (ctypeExpr2typeExpr $ ACS.typeOfQualType t)
        (fst qn)
        (funDescription msig cs)
        (nondet anaInfo)
        (lookupFR frMap qn)
    CurryDocDataDecl qn vs _ _ cons cs ->
      addTypeInfo ci $ TypeInfo
        (snd qn)
        (map consInfo cons)
        (map fst vs)
        (fst qn)
        (declDescription cs)
        False
    CurryDocNewtypeDecl qName vs _ (Just cons) cs ->
      addTypeInfo ci $ TypeInfo
        (snd qName)
        [consInfo cons]
        (map fst vs)
        (fst qName)
        (declDescription cs)
        False
    CurryDocNewtypeDecl qName vs@(v:_) _ Nothing cs ->
      addTypeInfo ci $ TypeInfo
        (snd qName)
        [(qName, [TVar $ fst v])]
        (map fst vs)
        (fst qName)
        (declDescription cs)
        False
    CurryDocTypeDecl qName vs tExpr cs ->
      addTypeInfo ci $ TypeInfo
        (snd qName)
        [(qName, [ctypeExpr2typeExpr tExpr])]
        (map fst vs)
        (fst qName)
        (declDescription cs)
        True
    CurryDocClassDecl qName _ vs _ _ cs ->
      addTypeInfo ci $ TypeInfo
        ("_Dict#" ++ snd qName)
        []
        (map fst vs)
        (fst qName)
        (declDescription cs)
        False
    _ -> ci

  lookupFR :: FRMap -> QName -> FlexRigidResult
  lookupFR frMap qn =
    fromMaybe UnknownFR $ T.lookup (stringifyQName qn) frMap

-- | The information about a Curry module.
data CurryInfo =
  CurryInfo ModuleInfo     -- ^ The module information
            [FunctionInfo] -- ^ Information about the functions
                           --   defined in the module
            [TypeInfo]     -- ^ Information about types (also newtypes
                           --   and classes) defined in the module
 deriving (Read, Show)

-- | The base information about some module.
data ModuleInfo =
  ModuleInfo String -- ^ The name
             String -- ^ The author
             String -- ^ The description
 deriving (Read, Show)

-- | The information about functions defined in a Curry module.
data FunctionInfo
  = FunctionInfo String          -- ^ The name.
                 TypeExpr        -- ^ The signature.
                 String          -- ^ The corresponding module.
                 String          -- ^ The description.
                 Bool            -- ^ `True` if the function is non-deterministically defined.
                 FlexRigidResult -- ^ The flex/rigid status of the function.
  deriving (Read, Show)

-- | The information about data and type declarations in a Curry module.
data TypeInfo
  = TypeInfo String                -- ^ The name (which has `_Dict#` prefix
                                   --   if it is a type class).
             [(QName, [TypeExpr])] -- ^ A list of constructor names and their
                                   --   argument types (or the type name
                                   --   and the type expression in case
                                   --   of type synonyms).
             [TVarIndex]           -- ^ A list of type variables (which is
                                   --   non-empty for a polymorphic type).
             String                -- ^ The corresponding module.
             String                -- ^ The description.
             Bool                  -- ^ A flag which is `True` if it is
                                   --   a type synonym.
 deriving (Read, Show)

-- Conversions --------------------------------------------------------

-- | Converts a CurryDocCons to a tuple containing the constructor's
--   qualified name and a list of its argument types.
consInfo :: CurryDocCons -> (QName, [TypeExpr])
consInfo (CurryDocConstr qn tExprs _ _) =
  (qn, map ctypeExpr2typeExpr tExprs)
consInfo (CurryDocConsOp qn tExpr1 tExpr2 _ _) =
  (qn, [ctypeExpr2typeExpr tExpr1, ctypeExpr2typeExpr tExpr2])
consInfo (CurryDocRecord qn tExprs _ _ _) =
  (qn, map ctypeExpr2typeExpr tExprs)

type FRMap = T.Trie FlexRigidResult

-- | Converts a list of fcy `FuncDecl`s to a mapping from
--   qualified names to `FlexRigidResult`s.
buildFRMap :: [FuncDecl] -> FRMap
buildFRMap = T.fromList . map buildFR
  where
    buildFR (Func qn _ _ _ rule) = (stringifyQName qn, flexRigid rule)

-- | Converts a qualified name to a flat `String`
--   by prepending the module name to the function name.
stringifyQName :: QName -> String
stringifyQName (m, n) = m ++ "." ++ n

-- | Computes the flex/rigid status of a fcy `Rule` (i.e., of a function).
flexRigid :: Rule -> FlexRigidResult
flexRigid (Rule _ expr) = getFlexRigid expr
flexRigid (External  _) = UnknownFR

-- auxiliaties --------------------------------------------------------

-- | Prepends a `FunctionInfo` to the list of function information.
addFunctionInfo :: CurryInfo -> FunctionInfo -> CurryInfo
addFunctionInfo (CurryInfo mi fis tis) fi =
  CurryInfo mi (fi : fis) tis

-- | Prepends a `TypeInfo` to the list of type information.
addTypeInfo :: CurryInfo -> TypeInfo -> CurryInfo
addTypeInfo (CurryInfo mi fis tis) ti =
  CurryInfo mi fis (ti : tis)

-- | Retrieves the author from the module header.
author :: ModuleHeader -> String
author = getFieldWithDefault "" Author

-- | Retrieves the (short) description from the module header.
--
--   If the description is not set, the full (long) description
--   is used instead.
description :: ModuleHeader -> String
description mh@(ModuleHeader _ fd) = getFieldWithDefault fd Description mh

-- | Generates data and type constructors.
consSignature :: ConsDecl -> (QName, [TypeExpr])
consSignature (Cons (mName, cName) _ _ tExprList) =
  ((mName, cName), tExprList)

-- | Generates data and type constructors.
newconsSignature :: NewConsDecl -> (QName, [TypeExpr])
newconsSignature (NewCons (mName, cName) _ tExpr) =
  ((mName, cName), [tExpr])

-- | Converts a `CTypeExpr` to a flat `TypeExpr`.
--
--   Because CurryDoc works with AbstractCurry types and Currygle
--   works with FlatCurry types, the types need to be converted.
ctypeExpr2typeExpr :: AC.CTypeExpr -> TypeExpr
ctypeExpr2typeExpr (AC.CTVar i) =
  TVar $ fst i
ctypeExpr2typeExpr (AC.CFuncType t1 t2) =
  FuncType (ctypeExpr2typeExpr t1) (ctypeExpr2typeExpr t2)
ctypeExpr2typeExpr (AC.CTCons qn) =
  TCons qn []
ctypeExpr2typeExpr (AC.CTApply t1 t2) =
  let l = ctypeExpr2typeExpr t1
      r = ctypeExpr2typeExpr t2
  in case (l, r) of
    (TCons qn args, _) -> TCons qn (args ++ [r])
    _                  -> TCons ("Prelude", "Apply") [l, r]

-- | Collects all exported CurryDoc declarations from a CurryDoc.
allCurryDocDecls :: CurryDoc -> [CurryDocDecl]
allCurryDocDecls (CurryDoc _ _ ex _) =
  concatMap collectExports ex
  where
    collectExports (ExportEntry e)        = [e]
    collectExports (ExportEntryModule _)  = []
    collectExports (ExportSection _ _ es) = concatMap collectExports es

allCurryDocTypes :: CurryDoc -> [CurryDocDecl]
allCurryDocTypes = filter (liftM2 (||) isCurryDocTypeDecl isCurryDocClassDecl)
                 . allCurryDocDecls

allCurryDocFuncs :: CurryDoc -> [CurryDocDecl]
allCurryDocFuncs = filter isCurryDocFuncDecl . allCurryDocDecls

-- | Cleans up a multi-line comment string by replacing
--   newlines with spaces. Because all information regarding
--   indentation and structure is lost, this should only be
--   used for header field comments without any markdown content.
removeNewlines :: String -> String
removeNewlines = unwords . lines

-- | Removes newlines and empty lines from the beginning and end
--   of a string.
cleanupBorder :: String -> String
cleanupBorder = removeWhitespace . reverse . removeWhitespace . reverse
  where
    removeWhitespace = dropWhile (`elem` [' ', '\n', '\t'])

-- | Generates the declaration description from a list of comments.
declDescription :: [Comment] -> String
declDescription = unwords . map commentString

-- | Generates the function description from a potentially
-- | missing type signature and comments.
funDescription :: Maybe CurryDocTypeSig -> [Comment] -> String
funDescription msig cs = case msig of
  Just (CurryDocTypeSig _ _ _ cs')
    -> declDescription (cs' ++ cs)
  Nothing
    -> declDescription cs
