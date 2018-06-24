module CurryDoc.Info.AbstractCurry
  (addAbstractCurryProg, collectInstanceInfo) where

import AbstractCurry.Types
import AbstractCurry.Select

import CurryDoc.Data.AnaInfo
import CurryDoc.Info.Comments
import CurryDoc.Info.Goodies

import Maybe (listToMaybe, mapMaybe)

--- Remove unexported entities and
--- add exported entities that did not have any comments
addAbstractCurryProg :: CurryProg -> [CommentedDecl] -> [CommentedDecl]
addAbstractCurryProg (CurryProg _ _ _ cls inst typ func _) ds =
  let withcls  = addAbstractCurryClassesInfo cls ds
      withins  = addAbstractCurryInstInfo inst ds
      withtyp  = addAbstractCurryDataInfo typ ds
      withfun  = addAbstractCurryFunInfo func ds
  in withcls ++ withins ++ withtyp ++ withfun

addAbstractCurryClassesInfo :: [CClassDecl] -> [CommentedDecl] -> [CommentedDecl]
addAbstractCurryClassesInfo []                               _   = []
addAbstractCurryClassesInfo (CClass n Public  cx vn ds : cs) cds =
  maybe (CommentedClassDecl n cx vn [] (addAbstractCurryFunInfo ds []))
    id (lookupClass n cds) : addAbstractCurryClassesInfo cs cds
addAbstractCurryClassesInfo (CClass _ Private _  _  _  : cs) cds =
  addAbstractCurryClassesInfo cs cds

lookupClass :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupClass _ []     = Nothing
lookupClass n (d:ds) = case d of
  CommentedClassDecl n' _ _ _ _
    | n =~= n' -> Just d
  _            -> lookupClass n ds

addAbstractCurryInstInfo :: [CInstanceDecl] -> [CommentedDecl] -> [CommentedDecl]
addAbstractCurryInstInfo []                          _   = []
addAbstractCurryInstInfo (CInstance n cx ty ds : is) cds =
  maybe (CommentedInstanceDecl n cx ty [] (addAbstractCurryFunInfo ds []))
    id (lookupInstance n ty cds) : addAbstractCurryInstInfo is cds

lookupInstance :: QName -> CTypeExpr -> [CommentedDecl] -> Maybe CommentedDecl
lookupInstance _ _  []     = Nothing
lookupInstance n ty (d:ds) = case d of
  CommentedInstanceDecl n' _ ty' _ _
    | n =~= n' && ty =~~= ty' -> Just d
  _                           -> lookupInstance n ty ds

addAbstractCurryFunInfo :: [CFuncDecl] -> [CommentedDecl] -> [CommentedDecl]
addAbstractCurryFunInfo []                                 _   = []
addAbstractCurryFunInfo (CFunc     n _ Public  qty _ : ds) cds =
  maybe (CommentedFunctionDecl n [] (Just qty) NoAnalysisInfo)
    (setType qty) (lookupFunc n cds) : addAbstractCurryFunInfo ds cds
addAbstractCurryFunInfo (CmtFunc _ n _ Public  qty _ : ds) cds =
  maybe (CommentedFunctionDecl n [] (Just qty) NoAnalysisInfo)
    (setType qty) (lookupFunc n cds) : addAbstractCurryFunInfo ds cds
addAbstractCurryFunInfo (CFunc     _ _ Private _   _ : ds) cds =
  addAbstractCurryFunInfo ds cds
addAbstractCurryFunInfo (CmtFunc _ _ _ Private _   _ : ds) cds =
  addAbstractCurryFunInfo ds cds

setType :: CQualTypeExpr -> CommentedDecl -> CommentedDecl
setType qty f = case f of
  (CommentedFunctionDecl n cs _ ai) -> CommentedFunctionDecl n cs (Just qty) ai
  _                                 -> f

lookupFunc :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupFunc _ []     = Nothing
lookupFunc n (d:ds) = case d of
  CommentedFunctionDecl n' _ _ _
    | n =~= n' -> Just d
  _            -> lookupFunc n ds

addAbstractCurryDataInfo :: [CTypeDecl] -> [CommentedDecl] -> [CommentedDecl]
addAbstractCurryDataInfo []                               _   = []
addAbstractCurryDataInfo (CTypeSyn n Public vs ty   : ds) cds =
  maybe (CommentedTypeDecl n vs ty []) id (lookupTypeDecl n cds)
    : addAbstractCurryDataInfo ds cds
addAbstractCurryDataInfo (CNewType n Public vs con _ : ds) cds =
  maybe (CommentedNewtypeDecl n vs [] (createNewConsInfos con))
    (\(CommentedNewtypeDecl a b c d)
      -> CommentedNewtypeDecl a b c (filterNewCons con d))
    (lookupNewDecl n cds) : addAbstractCurryDataInfo ds cds
addAbstractCurryDataInfo (CType n Public vs cons _   : ds) cds =
  maybe (CommentedDataDecl n vs [] (createConsInfos cons))
    (\(CommentedDataDecl a b c d) -> CommentedDataDecl a b c
       (mapMaybe (filterCons cons) d))
    (lookupDataDecl n cds) : addAbstractCurryDataInfo ds cds
addAbstractCurryDataInfo (CTypeSyn _ Private _ _     : ds) cds =
  addAbstractCurryDataInfo ds cds
addAbstractCurryDataInfo (CNewType _ Private _ _ _   : ds) cds =
  addAbstractCurryDataInfo ds cds
addAbstractCurryDataInfo (CType _ Private _ _ _      : ds) cds =
  addAbstractCurryDataInfo ds cds

filterCons :: [CConsDecl] -> CommentedConstr -> Maybe CommentedConstr
filterCons [] _ = Nothing
filterCons (CCons _ _ n v _    : cs) c@(CommentedConstr n' _ _ _)
  | n =~= n' && v == Public = Just c
  | otherwise               = filterCons cs c
filterCons (CCons _ _ n v _    : cs) c@(CommentedConsOp n' _ _ _ _)
  | n =~= n' && v == Public = Just c
  | otherwise               = filterCons cs c
filterCons (CRecord _ _ n v fs : cs) c@(CommentedRecord n' cms tys fs' ai)
  | n =~= n' && v == Public =
    Just (CommentedRecord n' cms tys (filter (filterFields fs) fs') ai)
  | otherwise               = filterCons cs c
filterCons (CCons _ _ _ _ _    : cs) c@(CommentedRecord _ _ _ _ _)
  = filterCons cs c
filterCons (CRecord _ _ _ _ _  : cs) c@(CommentedConsOp _ _ _ _ _)
  = filterCons cs c
filterCons (CRecord _ _ _ _ _  : cs) c@(CommentedConstr   _ _ _ _)
  = filterCons cs c

filterFields :: [CFieldDecl] -> CommentedField -> Bool
filterFields [] _ = False
filterFields (CField n Public  _ : fs) f@([n'], _, _)
  = n =~= n' || filterFields fs f
filterFields (CField _ Public  _ : _) ([], _, _)
  = error "CurryDoc.filterFields: field with no qname"
filterFields (CField _ Public  _ : _) ((_:_:_), _, _)
  = error "CurryDoc.filterFields: field with more than one qname"
filterFields (CField _ Private _ : fs) f
  = filterFields fs f

filterNewCons :: CConsDecl -> Maybe CommentedNewtypeConstr -> Maybe CommentedNewtypeConstr
filterNewCons _ Nothing  = Nothing
filterNewCons c (Just c') =  case (c, c') of
  (CCons   _ _ _ Public _ , CommentedNewConstr _ _ _ _)
      -> Just c'
  (CRecord _ _ _ Public fs, CommentedNewRecord n' a ty f b)
      -> Just (CommentedNewRecord n' a ty f' b)
    where f' = case f of
                 Nothing -> Nothing
                 Just x | any isPublicField fs -> Just x
                        | otherwise            -> Nothing
  _ -> Nothing

lookupTypeDecl :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupTypeDecl _ []     = Nothing
lookupTypeDecl n (d:ds) = case d of
  CommentedTypeDecl n' _ _ _
    | n =~= n' -> Just d
  _            -> lookupTypeDecl n ds

lookupDataDecl :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupDataDecl _ []     = Nothing
lookupDataDecl n (d:ds) = case d of
  CommentedDataDecl n' _ _ _
    | n =~= n' -> Just d
  _            -> lookupDataDecl n ds

lookupNewDecl :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupNewDecl _ []     = Nothing
lookupNewDecl n (d:ds) = case d of
  CommentedNewtypeDecl n' _ _ _
    | n =~= n' -> Just d
  _            -> lookupNewDecl n ds

createConsInfos :: [CConsDecl] -> [CommentedConstr]
createConsInfos [] = []
createConsInfos (CCons _ _ n Public tys : cs) =
  CommentedConstr n [] tys NoAnalysisInfo : createConsInfos cs
createConsInfos (CRecord _ _ n Public fs : cs) =
  CommentedRecord n [] (map cFieldType fs)
    (map createFieldInfo (filter isPublicField fs))
    NoAnalysisInfo : createConsInfos cs
createConsInfos (CCons _ _ _ Private _ : cs) =
  createConsInfos cs
createConsInfos (CRecord _ _ _ Private _ : cs) =
  createConsInfos cs

createFieldInfo :: CFieldDecl -> CommentedField
createFieldInfo (CField n _ ty) = ([n], [], ty)

createNewConsInfos :: CConsDecl -> Maybe CommentedNewtypeConstr
createNewConsInfos (CCons _ _ n Public tys) =
  Just $ CommentedNewConstr n [] (head tys) NoAnalysisInfo
createNewConsInfos (CRecord _ _ n Public fs) =
  Just $ CommentedNewRecord n [] (head (map cFieldType fs))
           (listToMaybe (map createFieldInfo (filter isPublicField fs)))
           NoAnalysisInfo
createNewConsInfos (CCons _ _ _ Private _) = Nothing
createNewConsInfos (CRecord _ _ _ Private _) = Nothing

-------------------------------------------------------------------------------
-- Collecting instance informations

collectInstanceInfo :: [CTypeDecl] -> [CommentedDecl] -> [CommentedDecl]
collectInstanceInfo tydecl ds = concatMap generateDerivingInstances tydecl ++
                                filter isCommentedInstanceDecl ds

generateDerivingInstances :: CTypeDecl -> [CommentedDecl]
generateDerivingInstances (CType    n Public vs _ der) =
  map (generateDerivingInstanceFor n vs) der
generateDerivingInstances (CNewType n Public vs _ der) =
  map (generateDerivingInstanceFor n vs) der
generateDerivingInstances (CTypeSyn _ _       _ _  ) = []
generateDerivingInstances (CNewType _ Private _ _ _) = []
generateDerivingInstances (CType    _ Private _ _ _) = []

generateDerivingInstanceFor :: QName -> [CTVarIName] -> QName -> CommentedDecl
generateDerivingInstanceFor t vs d =
  CommentedInstanceDecl d (CContext (map (generateConstraintFor d) vs))
    (generateType t vs) [] []

generateConstraintFor :: QName -> CTVarIName -> CConstraint
generateConstraintFor n v = (n, CTVar v)

generateType :: QName -> [CTVarIName] -> CTypeExpr
generateType n = foldl (\t v -> CTApply t (CTVar v)) (CTCons n)
