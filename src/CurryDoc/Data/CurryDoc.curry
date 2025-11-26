{- |
     Author  : Kai-Oliver Prott
     Version : March 2025

     Datatype and operations for Abstract CurryDoc
-}
module CurryDoc.Data.CurryDoc where

import CurryDoc.Info.Header
import CurryDoc.Info.Comments
import CurryDoc.Data.AnaInfo

import AbstractCurry.Types
import AbstractCurry.Select ( tconsArgsOfType )

import Data.Maybe ( catMaybes )

-- | CurryDoc mName mhead exports imports
data CurryDoc = CurryDoc MName ModuleHeader [ExportEntry CurryDocDecl]
                         [MName]
  deriving (Show)

-- | Documented Curry declarations
data CurryDocDecl
  = CurryDocTypeDecl     QName [CTVarIName] CTypeExpr [Comment]
  | CurryDocDataDecl     QName [CTVarIName] [CurryDocInstanceDecl]
                         Bool [CurryDocCons] [Comment]
  | CurryDocNewtypeDecl  QName [CTVarIName] [CurryDocInstanceDecl]
                         (Maybe CurryDocCons) [Comment]
  | CurryDocClassDecl    QName CContext [CTVarIName] [CurryDocFunDep] [CurryDocDecl] [Comment]
  | CurryDocFunctionDecl QName CQualTypeExpr (Maybe CurryDocTypeSig)
                         AnalysisInfo [Comment]
  deriving (Show)

-- | Documented Curry constructors
data CurryDocCons
  = CurryDocConstr QName [CTypeExpr]                 AnalysisInfo [Comment]
  | CurryDocConsOp QName CTypeExpr CTypeExpr         AnalysisInfo [Comment]
  | CurryDocRecord QName [CTypeExpr] [CurryDocField] AnalysisInfo [Comment]
  deriving (Show)

-- | Documented Curry type signatures
data CurryDocTypeSig = CurryDocTypeSig QName CContext
                                       [(CTypeExpr, [Comment])] [Comment]
  deriving (Show)

-- | Documented Curry instances
data CurryDocInstanceDecl = CurryDocInstanceDecl QName CContext [CTypeExpr]
                                                 [CurryDocDecl] [Comment]
  deriving (Show)

-- | Documented Curry Record fields
data CurryDocField = CurryDocField QName CTypeExpr AnalysisInfo [Comment]
  deriving (Show)

-- | Documented Curry functional dependencies
data CurryDocFunDep = CurryDocFunDep CFunDep
  deriving (Show)

isCurryDocFuncDecl :: CurryDocDecl -> Bool
isCurryDocFuncDecl d = case d of
  CurryDocFunctionDecl _ _ _ _ _ -> True
  _                              -> False

isCurryDocClassDecl :: CurryDocDecl -> Bool
isCurryDocClassDecl d = case d of
  CurryDocClassDecl _ _ _ _ _ _ -> True
  _                             -> False

isCurryDocTypeDecl :: CurryDocDecl -> Bool
isCurryDocTypeDecl d = case d of
  CurryDocDataDecl    _ _ _ _ _ _ -> True
  CurryDocNewtypeDecl _ _ _ _ _   -> True
  CurryDocTypeDecl    _ _ _ _     -> True
  _                               -> False

getTypesigComments :: CurryDocTypeSig -> [Comment]
getTypesigComments (CurryDocTypeSig _ _ _ cs) = cs

instTypeNames :: CurryDocInstanceDecl -> [QName]
instTypeNames (CurryDocInstanceDecl _ _ ts _ _)  
 = map fst $ catMaybes $ map tconsArgsOfType ts
 
curryDocDeclName :: CurryDocDecl -> QName
curryDocDeclName (CurryDocTypeDecl     n _ _ _    ) = n
curryDocDeclName (CurryDocDataDecl     n _ _ _ _ _) = n
curryDocDeclName (CurryDocNewtypeDecl  n _ _ _ _  ) = n
curryDocDeclName (CurryDocClassDecl    n _ _ _ _ _) = n
curryDocDeclName (CurryDocFunctionDecl n _ _ _ _  ) = n
