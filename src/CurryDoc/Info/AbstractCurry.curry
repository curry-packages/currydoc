{- |
     Author  : Kai-Oliver Prott
     Version : August 2018

     Operations to add information from AbstactCurry to the
     commented declarations.
-}
module CurryDoc.Info.AbstractCurry (addAbstractCurryProg) where

import AbstractCurry.Types
import AbstractCurry.Select
import qualified FlatCurry.Types as FC
import FlatCurry.Goodies

import CurryDoc.Data.AnaInfo
import CurryDoc.Data.CurryDoc
import CurryDoc.Info.Comments
import CurryDoc.Info.Goodies

import List  (last)
import Maybe (listToMaybe)

-- HACK due to external data decls not being in AbstractCurry, we augment the
-- AbstractCurry with the nullary types from FlatCurry(those are external ones)
-- and add a dummy-constructor to every empty data decl in AbstractCurry
-- | Remove unexported entities and
--   add exported entities that did not have any comments.
--   All entities get a lot more information about thei type or ...
--   Also translates into CurryDoc representations and sets a flag for
--   ExternalDataDecls
addAbstractCurryProg :: CurryProg -> FC.Prog -> [CommentedDecl] -> [CurryDocDecl]
addAbstractCurryProg (CurryProg _ _ _ cls inst typ func _) fprog ds =
  let typ' = map augmentEmptyCons typ ++ (map flatToAbstract
                                                $ filter externalType
                                                $ progTypes fprog)
      withins  = addAbstractCurryInstInfo inst ds ++
                 concatMap generateDerivingInstances typ
      withcls  = addAbstractCurryClassesInfo cls ds
      withtyp  = addAbstractCurryDataInfo typ' ds withins
      withfun  = addAbstractCurryFunInfo func ds
  in withcls ++ withtyp ++ withfun
  where
    typeConstr = trType (\_ _ _ cs -> cs) (\_ _ _ _ -> [])
    externalType ty = not (isTypeSyn ty) && null (typeConstr ty)

    augmentEmptyCons   (CType n vis vs cons deriv)
      | null cons = CType n vis vs [CCons [] (CContext []) ("","dummy") Private []] deriv
      | otherwise = CType n vis vs cons                                             deriv
    augmentEmptyCons t@(CNewType _ _ _ _ _) = t
    augmentEmptyCons t@(CTypeSyn _ _ _ _  ) = t

    flatToAbstract (FC.Type n vis vs _) =
      CType n (trVis vis) (map (\i -> (i, varName i)) vs) [] []
    flatToAbstract (FC.TypeSyn _ _ _ _) =
      error $ "CurryDoc.Info.AbstractCurry.addAbstractCurryProg.flatToAbstract:"
           ++ " unexpected TypeSyn"
    trVis FC.Public  = Public
    trVis FC.Private = Private

    varName i = (['a'..'z'] !! i) : if i < 26 then "" else show i

-- All addXInfo function are mostly the same:
--   look for the CommentedDecl that matches the AbstractCurry decl
--   When it exists, then add some additional information to the decl
--   Otherwise create one with those infos
-- All this is only done, when the AbstractCurry decl is public (exported)


-- Currently ignores comments and declarations and just adds context and type
addAbstractCurryInstInfo :: [CInstanceDecl] -> [CommentedDecl] -> [CurryDocInstanceDecl]
addAbstractCurryInstInfo []                          _   = []
addAbstractCurryInstInfo (CInstance n cx ty ds : is) cds =
  maybe (CurryDocInstanceDecl n cx ty (addAbstractCurryFunInfo ds []) [])
    (\(CommentedInstanceDecl _ _ cs ds') -> CurryDocInstanceDecl n cx ty
        (addAbstractCurryFunInfo ds ds') cs) (lookupInstance n ty cds)
    : addAbstractCurryInstInfo is cds

-- Adds superclass and type variable
addAbstractCurryClassesInfo :: [CClassDecl] -> [CommentedDecl] -> [CurryDocDecl]
addAbstractCurryClassesInfo []                               _   = []
addAbstractCurryClassesInfo (CClass n Public  cx vn ds : cs) cds =
  maybe (CurryDocClassDecl n cx vn (addAbstractCurryFunInfo ds []) [])
    (\(CommentedClassDecl _ cs' ds') -> CurryDocClassDecl n cx vn
        (addAbstractCurryFunInfo ds ds') cs') (lookupClass n cds)
    : addAbstractCurryClassesInfo cs cds
addAbstractCurryClassesInfo (CClass _ Private _  _  _  : cs) cds =
  addAbstractCurryClassesInfo cs cds

-- Adds empty Analysis, qualified type and typesig (if it exists)
addAbstractCurryFunInfo :: [CFuncDecl] -> [CommentedDecl] -> [CurryDocDecl]
addAbstractCurryFunInfo []                             _   = []
addAbstractCurryFunInfo (CFunc n _ Public  qty _ : ds) cds =
  maybe (CurryDocFunctionDecl n qty typesig NoAnalysisInfo [])
    (\(CommentedFunctionDecl _ cs) -> CurryDocFunctionDecl n qty typesig
        NoAnalysisInfo cs)
    (lookupFunc n cds)
    : addAbstractCurryFunInfo ds cds
  where typesig = transformTypesig qty (lookupTypeSig [n] cds)
addAbstractCurryFunInfo (CFunc _ _ Private _   _ : ds) cds =
  addAbstractCurryFunInfo ds cds
addAbstractCurryFunInfo (CmtFunc _ a b c d e : ds) cds =
  addAbstractCurryFunInfo (CFunc a b c d e : ds) cds

-- transform the content of a typesig to CurryDocTypeSig
transformTypesig :: CQualTypeExpr -> Maybe CommentedDecl -> Maybe CurryDocTypeSig
transformTypesig (CQualType cx _) d = case d of
  Just (CommentedTypeSig [n] cs ps) -> Just (CurryDocTypeSig n cx ps cs)
  _                                 -> Nothing

-- For data: Adds external info, typevars, instances,
--           and modifies constructors (see below)
-- For new: Adds typevars, instances and modifies constructor
-- For syn: Adds type and typevars
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

-- Adds type and analysis info and modifies fields for recordss (see below)
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

-- create constructor info
createConsInfo :: QName -> [CTypeExpr] -> CurryDocCons
createConsInfo n tys
  | isOperator && length tys == 2 =
            let [ty1, ty2] = tys
            in  CurryDocConsOp n ty1 ty2 NoAnalysisInfo []
  | otherwise = CurryDocConstr n tys     NoAnalysisInfo []
  where isOperator = all (`elem` "~!@#$%^&*+-=<>:?./|\\") (snd n)

-- create constructor info for records
createRecordInfo :: QName -> [CFieldDecl] -> CurryDocCons
createRecordInfo n fs =
  CurryDocRecord n (map cFieldType fs) (addAbstractCurryField fs [])
                 NoAnalysisInfo []

-- add type to constructor
transformConstructor :: QName -> [CTypeExpr] -> CommentedConstr -> CurryDocCons
transformConstructor n tys c = case c of
  CommentedConstr _ cs -> CurryDocConstr n tys                   NoAnalysisInfo cs
  CommentedConsOp _ cs -> CurryDocConsOp n (head tys) (last tys) NoAnalysisInfo cs
  _                    -> error "CurryDoc.Info.AbstractCurry. transformConstructor"

-- adds type to record constructor and modifies fields (see below)
transformRecord :: QName -> [CFieldDecl] -> CommentedConstr -> CurryDocCons
transformRecord n fs c = case c of
  CommentedRecord _ cs fs' -> CurryDocRecord n (map cFieldType fs)
                                             (addAbstractCurryField fs fs')
                                             NoAnalysisInfo cs
  _                        -> error "CurryDoc.Info.AbstractCurry. transformRecord"

-- adds empty AnalysisInfo and type to record fields
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

getInstances :: QName -> [CurryDocInstanceDecl] -> [CurryDocInstanceDecl]
getInstances n = filter ((=~= n) . instTypeName)
