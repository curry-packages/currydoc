module CurryDoc.Data.CurryDoc where

import CurryDoc.Info.Header
import CurryDoc.Info.Comments
import CurryDoc.Data.AnaInfo

import AbstractCurry.Types
import AbstractCurry.Select (tconsArgsOfType)


--- CurryDoc mName mhead exports imports
data CurryDoc = CurryDoc String ModuleHeader [ExportEntry CurryDocDecl] [MName]
  deriving Show

data CurryDocDecl
  = CurryDocTypeDecl     QName [CTVarIName] CTypeExpr [Comment]
  | CurryDocDataDecl     QName [CTVarIName] [CurryDocInstanceDecl] Bool [CurryDocCons] [Comment]
  | CurryDocNewtypeDecl  QName [CTVarIName] [CurryDocInstanceDecl] (Maybe CurryDocCons) [Comment]
  | CurryDocClassDecl    QName CContext CTVarIName [CurryDocDecl] [Comment]
  | CurryDocFunctionDecl QName CQualTypeExpr (Maybe CurryDocTypeSig) AnalysisInfo [Comment]
  deriving Show

data CurryDocCons
  = CurryDocConstr QName [CTypeExpr]                 AnalysisInfo [Comment]
  | CurryDocConsOp QName CTypeExpr CTypeExpr         AnalysisInfo [Comment]
  | CurryDocRecord QName [CTypeExpr] [CurryDocField] AnalysisInfo [Comment]
  deriving Show

data CurryDocTypeSig = CurryDocTypeSig QName CContext [(CTypeExpr, [Comment])] [Comment]
  deriving Show

data CurryDocInstanceDecl = CurryDocInstanceDecl QName CContext CTypeExpr [CurryDocDecl] [Comment]
  deriving Show

data CurryDocField = CurryDocField QName CTypeExpr AnalysisInfo [Comment]
  deriving Show

isCurryDocFuncDecl :: CurryDocDecl -> Bool
isCurryDocFuncDecl d = case d of
  CurryDocFunctionDecl _ _ _ _ _ -> True
  _                              -> False

isCurryDocClassDecl :: CurryDocDecl -> Bool
isCurryDocClassDecl d = case d of
  CurryDocClassDecl _ _ _ _ _ -> True
  _                           -> False

getTypesigComments :: CurryDocTypeSig -> [Comment]
getTypesigComments (CurryDocTypeSig _ _ _ cs) = cs


instTypeName :: CurryDocInstanceDecl -> QName
instTypeName (CurryDocInstanceDecl _ _ ty _ _) = q
  where Just (q,_) = tconsArgsOfType ty
