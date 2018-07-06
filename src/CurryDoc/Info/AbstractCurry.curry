module CurryDoc.Info.AbstractCurry (addAbstractCurryProg) where

import AbstractCurry.Types
import AbstractCurry.Select

import CurryDoc.Data.AnaInfo
import CurryDoc.Data.CurryDoc
import CurryDoc.Info.Comments
import CurryDoc.Info.Goodies

import List  (last)
import Maybe (listToMaybe)

--- Remove unexported entities and
--- add exported entities that did not have any comments.
--- Also translates into CurryDoc representations and sets a flag for
--- ExternalDataDecls
addAbstractCurryProg :: CurryProg -> [CommentedDecl] -> [CurryDocDecl]
addAbstractCurryProg (CurryProg _ _ _ cls inst typ func _) ds =
  let withins  = addAbstractCurryInstInfo inst ds ++
                 concatMap generateDerivingInstances typ
      withcls  = addAbstractCurryClassesInfo cls ds
      withtyp  = addAbstractCurryDataInfo typ ds withins
      withfun  = addAbstractCurryFunInfo func ds
  in withcls ++ withtyp ++ withfun

addAbstractCurryInstInfo :: [CInstanceDecl] -> [CommentedDecl] -> [CurryDocInstanceDecl]
addAbstractCurryInstInfo []                          _   = []
addAbstractCurryInstInfo (CInstance n cx ty ds : is) cds =
  maybe (CurryDocInstanceDecl n cx ty (addAbstractCurryFunInfo ds []) [])
    (\(CommentedInstanceDecl _ _ cs ds') -> CurryDocInstanceDecl n cx ty
        (addAbstractCurryFunInfo ds ds') cs) (lookupInstance n ty cds)
    : addAbstractCurryInstInfo is cds

addAbstractCurryClassesInfo :: [CClassDecl] -> [CommentedDecl] -> [CurryDocDecl]
addAbstractCurryClassesInfo []                               _   = []
addAbstractCurryClassesInfo (CClass n Public  cx vn ds : cs) cds =
  maybe (CurryDocClassDecl n cx vn (addAbstractCurryFunInfo ds []) [])
    (\(CommentedClassDecl _ cs' ds') -> CurryDocClassDecl n cx vn
        (addAbstractCurryFunInfo ds ds') cs') (lookupClass n cds)
    : addAbstractCurryClassesInfo cs cds
addAbstractCurryClassesInfo (CClass _ Private _  _  _  : cs) cds =
  addAbstractCurryClassesInfo cs cds

addAbstractCurryFunInfo :: [CFuncDecl] -> [CommentedDecl] -> [CurryDocDecl]
addAbstractCurryFunInfo []                             _   = []
addAbstractCurryFunInfo (CFunc n _ Public  qty _ : ds) cds =
  maybe (CurryDocFunctionDecl n qty Nothing NoAnalysisInfo [])
    (\(CommentedFunctionDecl _ cs) -> CurryDocFunctionDecl n qty
        (transformTypesig qty (lookupTypeSig n cds)) NoAnalysisInfo cs)
    (lookupFunc n cds)
    : addAbstractCurryFunInfo ds cds
addAbstractCurryFunInfo (CFunc _ _ Private _   _ : ds) cds =
  addAbstractCurryFunInfo ds cds
addAbstractCurryFunInfo (CmtFunc _ a b c d e : ds) cds =
  addAbstractCurryFunInfo (CFunc a b c d e : ds) cds

transformTypesig :: CQualTypeExpr -> Maybe CommentedDecl -> Maybe CurryDocTypeSig
transformTypesig (CQualType cx _) d = case d of
  Just (CommentedTypeSig [n] cs ps) -> Just (CurryDocTypeSig n cx ps cs)
  _                                 -> Nothing

addAbstractCurryDataInfo :: [CTypeDecl] -> [CommentedDecl]
                         -> [CurryDocInstanceDecl] -> [CurryDocDecl]
addAbstractCurryDataInfo []                                _   _   = []
addAbstractCurryDataInfo (CTypeSyn n Public vs ty    : ds) cds ins =
  maybe (CurryDocTypeDecl n vs ty [])
    (\(CommentedTypeDecl _ cs) -> CurryDocTypeDecl n vs ty cs)
    (lookupTypeDecl n cds)
    : addAbstractCurryDataInfo ds cds ins
addAbstractCurryDataInfo (CNewType n Public vs con _ : ds) cds ins =
  maybe (CurryDocNewtypeDecl n vs (getInstances n ins)
          (listToMaybe (addAbstractCurryConsInfo [con] [])) [])
    (\(CommentedNewtypeDecl _ cs cn) -> CurryDocNewtypeDecl n vs
        (getInstances n ins) (listToMaybe (addAbstractCurryConsInfo [con] [cn])) cs)
    (lookupNewDecl n cds)
    : addAbstractCurryDataInfo ds cds ins
addAbstractCurryDataInfo (CType n Public vs cons _   : ds) cds ins =
  maybe (CurryDocDataDecl n vs (getInstances n ins)
          (null cons) (addAbstractCurryConsInfo cons []) [])
    (\(CommentedDataDecl _ cs cns) -> CurryDocDataDecl n vs (getInstances n ins)
          (null cons) (addAbstractCurryConsInfo cons cns) cs)
    (lookupDataDecl n cds)
    : addAbstractCurryDataInfo ds cds ins
addAbstractCurryDataInfo (CTypeSyn _ Private _ _     : ds) cds ins =
  addAbstractCurryDataInfo ds cds ins
addAbstractCurryDataInfo (CNewType _ Private _ _ _   : ds) cds ins =
  addAbstractCurryDataInfo ds cds ins
addAbstractCurryDataInfo (CType _ Private _ _ _      : ds) cds ins =
  addAbstractCurryDataInfo ds cds ins

addAbstractCurryConsInfo :: [CConsDecl] -> [CommentedConstr] -> [CurryDocCons]
addAbstractCurryConsInfo []                              _   = []
addAbstractCurryConsInfo (CCons   _ _ n Public  tys : cs) cds =
  maybe (createConsInfo n tys) (transformConstructor n tys)
    (lookupCons n cds)
    : addAbstractCurryConsInfo cs cds
addAbstractCurryConsInfo (CRecord _ _ n Public  fs  : cs) cds =
  maybe (createRecordInfo n fs) (transformRecord n fs)
    (lookupRecord n cds)
    : addAbstractCurryConsInfo cs cds
addAbstractCurryConsInfo (CCons   _ _ _ Private _   : cs) cds =
  addAbstractCurryConsInfo cs cds
addAbstractCurryConsInfo (CRecord _ _ _ Private _   : cs) cds =
  addAbstractCurryConsInfo cs cds

createConsInfo :: QName -> [CTypeExpr] -> CurryDocCons
createConsInfo n tys
  | isOperator && length tys == 2 =
            let [ty1, ty2] = tys
            in  CurryDocConsOp n ty1 ty2 NoAnalysisInfo []
  | otherwise = CurryDocConstr n tys     NoAnalysisInfo []
  where isOperator = all (`elem` "~!@#$%^&*+-=<>:?./|\\") (snd n)

createRecordInfo :: QName -> [CFieldDecl] -> CurryDocCons
createRecordInfo n fs =
  CurryDocRecord n (map cFieldType fs) (addAbstractCurryField fs [])
                 NoAnalysisInfo []

transformConstructor :: QName -> [CTypeExpr] -> CommentedConstr -> CurryDocCons
transformConstructor n tys c = case c of
  CommentedConstr _ cs -> CurryDocConstr n tys                   NoAnalysisInfo cs
  CommentedConsOp _ cs -> CurryDocConsOp n (head tys) (last tys) NoAnalysisInfo cs
  _                    -> error "CurryDoc.Info.AbstractCurry. transformConstructor"

transformRecord :: QName -> [CFieldDecl] -> CommentedConstr -> CurryDocCons
transformRecord n fs c = case c of
  CommentedRecord _ cs fs' -> CurryDocRecord n (map cFieldType fs)
                                             (addAbstractCurryField fs fs')
                                             NoAnalysisInfo cs
  _                        -> error "CurryDoc.Info.AbstractCurry. transformRecord"

addAbstractCurryField :: [CFieldDecl] -> [CommentedField] -> [CurryDocField]
addAbstractCurryField []                         _   =  []
addAbstractCurryField (CField n Public  ty : fs) cfs =
  maybe (CurryDocField n ty NoAnalysisInfo [])
    (\(_,cs) -> CurryDocField n ty NoAnalysisInfo cs) (lookupField n cfs)
    : addAbstractCurryField fs cfs
addAbstractCurryField (CField _ Private _  : fs) cfs =
  addAbstractCurryField fs cfs

-------------------------------------------------------------------------------
-- Collecting instance informations

generateDerivingInstances :: CTypeDecl -> [CurryDocInstanceDecl]
generateDerivingInstances (CType    n Public vs _ der) =
  map (generateDerivingInstanceFor n vs) der
generateDerivingInstances (CNewType n Public vs _ der) =
  map (generateDerivingInstanceFor n vs) der
generateDerivingInstances (CTypeSyn _ _       _ _  ) = []
generateDerivingInstances (CNewType _ Private _ _ _) = []
generateDerivingInstances (CType    _ Private _ _ _) = []

generateDerivingInstanceFor :: QName -> [CTVarIName] -> QName -> CurryDocInstanceDecl
generateDerivingInstanceFor t vs d =
  CurryDocInstanceDecl d (CContext (map (generateConstraintFor d) vs))
    (generateType t vs) [] []

generateConstraintFor :: QName -> CTVarIName -> CConstraint
generateConstraintFor n v = (n, CTVar v)

generateType :: QName -> [CTVarIName] -> CTypeExpr
generateType n = foldl (\t v -> CTApply t (CTVar v)) (CTCons n)

-------------------------------------------------------------------------------
-- lookup entries

lookupClass :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupClass _ []     = Nothing
lookupClass n (d:ds) = case d of
  CommentedClassDecl n' _ _
    | n =~= n' -> Just d
  _            -> lookupClass n ds

lookupInstance :: QName -> CTypeExpr -> [CommentedDecl] -> Maybe CommentedDecl
lookupInstance _ _  []     = Nothing
lookupInstance n ty (d:ds) = case d of
  CommentedInstanceDecl n' ty' _ _
    | n =~= n' && ty =~~= ty' -> Just d
  _                           -> lookupInstance n ty ds

lookupFunc :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupFunc _ []     = Nothing
lookupFunc n (d:ds) = case d of
  CommentedFunctionDecl n' _
    | n =~= n' -> Just d
  _            -> lookupFunc n ds

lookupTypeDecl :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupTypeDecl _ []     = Nothing
lookupTypeDecl n (d:ds) = case d of
  CommentedTypeDecl n' _
    | n =~= n' -> Just d
  _            -> lookupTypeDecl n ds

lookupDataDecl :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupDataDecl _ []     = Nothing
lookupDataDecl n (d:ds) = case d of
  CommentedDataDecl n' _ _
    | n =~= n' -> Just d
  _            -> lookupDataDecl n ds

lookupNewDecl :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupNewDecl _ []     = Nothing
lookupNewDecl n (d:ds) = case d of
  CommentedNewtypeDecl n' _ _
    | n =~= n' -> Just d
  _            -> lookupNewDecl n ds

lookupTypeSig :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupTypeSig _ []     = Nothing
lookupTypeSig n (d:ds) = case d of
  CommentedTypeSig [n'] _ _
    | n =~= n' -> Just d
  _            -> lookupTypeSig n ds

lookupField :: QName -> [CommentedField] -> Maybe CommentedField
lookupField _ []     = Nothing
lookupField n (f:fs) = case f of
  ([n'], _)
    | n =~= n' -> Just f
  _            -> lookupField n fs

lookupRecord :: QName -> [CommentedConstr] -> Maybe CommentedConstr
lookupRecord _ []     = Nothing
lookupRecord n (c:cs) = case c of
  CommentedRecord n' _ _
    | n =~= n' -> Just c
  _            -> lookupRecord n cs

lookupCons :: QName -> [CommentedConstr] -> Maybe CommentedConstr
lookupCons _ []     = Nothing
lookupCons n (c:cs) = case c of
  CommentedConstr n'  _
    | n =~= n' -> Just c
  CommentedConsOp n'  _
    | n =~= n' -> Just c
  _            -> lookupCons n cs

getInstances :: QName -> [CurryDocInstanceDecl] -> [CurryDocInstanceDecl]
getInstances n = filter (\(CurryDocInstanceDecl n' _ _ _ _) -> n =~= n')
