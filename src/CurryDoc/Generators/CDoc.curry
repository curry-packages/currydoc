{- |

    Description: Functions to generate documentation in "CDoc" format.
    Author: Sandra Dylus
    Version: May 2025

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

import Data.List
import Data.Maybe (catMaybes)
import ReadShowTerm

--- Generates the documentation of a module in "CDoc" format.
generateCDoc :: CurryDoc -> IO String
generateCDoc cd@(CurryDoc mname mhead _ _) = do
  let modInfo = ModuleInfo mname (author mhead) (description mhead)
      decls     = allCurryDocDecls cd
      (funcInfos, typeInfos) = partitionDecls $ translateDecls decls

  putStrLn $ "Writing " ++ mname ++ ".cdoc file..."
  return $ showTerm (CurryInfo modInfo funcInfos typeInfos)
 where
  translateDecls :: [CurryDocDecl] -> [DeclInfo]
  translateDecls = catMaybes . map translateDecl

  translateDecl :: CurryDocDecl -> Maybe DeclInfo
  translateDecl decl = case decl of
    CurryDocFunctionDecl qn t _ anaInfo cs -> 
      Just $ FunctionInfo 
        (snd qn) 
        (ctypeExpr2typeExpr $ ACS.typeOfQualType t)
        (fst qn)
        (declDescription cs) 
        (nondet anaInfo)
        UnknownFR -- TODO: flexRigid
    CurryDocDataDecl qn vs _ _ cons cs -> 
      Just $ TypeInfo 
        (snd qn)
        (map consInfo cons)
        (map fst vs)
        (fst qn)
        (declDescription cs)
        False
    CurryDocNewtypeDecl qName vs _ (Just cons) cs ->
      Just $ TypeInfo 
        (snd qName)
        [consInfo cons]
        (map fst vs)
        (fst qName)
        (declDescription cs)
        False
    CurryDocNewtypeDecl qName vs@(v:_) _ Nothing cs ->
      Just $ TypeInfo 
        (snd qName)
        [(qName, [TVar $ fst v])]
        (map fst vs)
        (fst qName)
        (declDescription cs)
        False
    CurryDocTypeDecl qName vs tExpr cs ->
      Just $ TypeInfo 
        (snd qName)
        [(qName, [ctypeExpr2typeExpr tExpr])]
        (map fst vs)
        (fst qName)
        (declDescription cs)
        True
    _ -> Nothing -- TODO: We ignore class declarations for now

--- The information about a Curry module.
data CurryInfo = 
  CurryInfo ModuleInfo -- ^ the module information 
            [DeclInfo] -- ^ the corresponding functions
            [DeclInfo] -- ^ the corresponding data and type declaration
 deriving (Read, Show)

--- The basic information about some module.
data ModuleInfo = 
  ModuleInfo String -- ^ the name 
             String -- ^ the author
             String -- ^ the description
 deriving (Read, Show)

-- | The information about functions defined in a Curry module.
data DeclInfo
  = FunctionInfo String          -- ^ the name
                 TypeExpr        -- ^ the signature
                 String          -- ^ the corresponding module
                 String          -- ^ the description
                 Bool            -- ^ True if property is defined non-deterministically
                 FlexRigidResult -- ^ the flex/rigid status
  | TypeInfo String                -- ^ the name
             [(QName, [TypeExpr])] -- ^ a list of constructors and their argument types (or the type name
                                   --   and the type expression in case of type synonyms)
             [TVarIndex]           -- ^ a list of type variables (i.e., non-empty for a polymoprhic type)
             String                -- ^ the corresponding module 
             String                -- ^ the description
             Bool                  -- ^ a flag which is `True` if it is a type synonym
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

-- auxiliaties --------------------------------------------------------

-- | Partitions a list of `DeclInfo` into function declarations
--   and type declarations.
partitionDecls :: [DeclInfo] -> ([DeclInfo], [DeclInfo])
partitionDecls decls = 
  (filter isFunctionDecl decls, filter isTypeDecl decls)
  where
    isFunctionDecl decl = case decl of
      FunctionInfo {} -> True
      _               -> False

    isTypeDecl decl = case decl of
      TypeInfo {} -> True
      _           -> False 

-- | Retrieves the author from the module header.
author :: ModuleHeader -> String
author = getFieldWithDefault "" Author

-- | Retrieves the (short) description from the module header.
--
--   If the description is not set, the full (long) description 
--   is used instead.
description :: ModuleHeader -> String
description mh@(ModuleHeader _ fd) = getFieldWithDefault fd Description mh

-- generate data and type constructors
consSignature :: ConsDecl -> (QName, [TypeExpr])
consSignature (Cons (mName, cName) _ _ tExprList) =
  ((mName, cName), tExprList)

-- generate data and type constructors
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
allCurryDocTypes = filter isCurryDocTypeDecl . allCurryDocDecls

allCurryDocFuncs :: CurryDoc -> [CurryDocDecl]
allCurryDocFuncs = filter isCurryDocFuncDecl . allCurryDocDecls

declDescription :: [Comment] -> String
declDescription = unwords . map commentString