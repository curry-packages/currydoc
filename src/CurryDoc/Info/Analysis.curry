{- |
     Author  : Kai-Oliver Prott
     Version : March 2025

     Operations to add analysis information
     to the CurryDoc declarations.
-}
module CurryDoc.Info.Analysis
  ( addAnaInfoToCurryDocDecls, addShortAnaInfoToCurryDocDecls ) where

import CurryDoc.Data.AnaInfo
import CurryDoc.Data.CurryDoc
import CurryDoc.Info.Goodies

import AbstractCurry.Types
import AbstractCurry.Select

import Data.List ( find, partition, isPrefixOf )

-- | Adds analysis information to suitable `CurryDocDecls`.
addAnaInfoToCurryDocDecls :: AnaInfo -> [COpDecl] -> [CFuncDecl]
                          -> [CurryDocDecl] -> [CurryDocDecl]
addAnaInfoToCurryDocDecls ai cop funs =
  map (addAnaInfoToCurryDocDecl ai cop funs)

-- | Adds short analysis information to suitable `CurryDocDecls`.
addShortAnaInfoToCurryDocDecls :: [COpDecl] -> [CFuncDecl]
                               -> [CurryDocDecl] -> [CurryDocDecl]
addShortAnaInfoToCurryDocDecls cop funs =
  map (addShortAnaInfoToCurryDocDecl cop funs)

-- | Recursively descends the declarations and fills in any `AnalysisInfo`.

addAnaInfoToCurryDocDecl :: AnaInfo -> [COpDecl] -> [CFuncDecl] -> CurryDocDecl
                        -> CurryDocDecl
addAnaInfoToCurryDocDecl _  _   _ d@(CurryDocTypeDecl          _ _ _ _) = d
addAnaInfoToCurryDocDecl _  cop _ (CurryDocClassDecl         a b c d e f) =
  CurryDocClassDecl a b c d (map (addPrecedenceInfoToCurryDocDecl cop) e) f
addAnaInfoToCurryDocDecl ai cop funs (CurryDocFunctionDecl n qty sig _ cs) =
  CurryDocFunctionDecl n qty sig (createAnalysisInfoFun ai cop funs n) cs
addAnaInfoToCurryDocDecl _  cop _ (CurryDocDataDecl  idt vs ins ex cns cs) =
  CurryDocDataDecl idt vs ins ex
                   (map (addPrecedenceInfoToCurryDocCons cop) cns) cs
addAnaInfoToCurryDocDecl _  cop _ (CurryDocNewtypeDecl idt vs ins cns cs) =
  CurryDocNewtypeDecl idt vs ins
                      (fmapMaybe (addPrecedenceInfoToCurryDocCons cop) cns) cs
  where fmapMaybe _ Nothing  = Nothing
        fmapMaybe f (Just x) = Just  (f x)

addShortAnaInfoToCurryDocDecl :: [COpDecl] -> [CFuncDecl] -> CurryDocDecl
                              -> CurryDocDecl
addShortAnaInfoToCurryDocDecl _   _ d@(CurryDocTypeDecl          _ _ _ _) = d
addShortAnaInfoToCurryDocDecl cop _ (CurryDocClassDecl         a b c d e f) =
  CurryDocClassDecl a b c d (map (addPrecedenceInfoToCurryDocDecl cop) e) f
addShortAnaInfoToCurryDocDecl cop funs (CurryDocFunctionDecl n qty sig _ cs) =
  CurryDocFunctionDecl n qty sig (createShortAnalysisInfoFun cop funs n) cs
addShortAnaInfoToCurryDocDecl cop _ (CurryDocDataDecl  idt vs ins ex cns cs) =
  CurryDocDataDecl idt vs ins ex
                   (map (addPrecedenceInfoToCurryDocCons cop) cns) cs
addShortAnaInfoToCurryDocDecl cop _ (CurryDocNewtypeDecl idt vs ins cns cs) =
  CurryDocNewtypeDecl idt vs ins
                      (fmapMaybe (addPrecedenceInfoToCurryDocCons cop) cns) cs
  where fmapMaybe _ Nothing  = Nothing
        fmapMaybe f (Just x) = Just  (f x)

addPrecedenceInfoToCurryDocCons :: [COpDecl] -> CurryDocCons -> CurryDocCons
addPrecedenceInfoToCurryDocCons cop (CurryDocConstr c tys     _ cs) =
  CurryDocConstr c tys     (createPrecInfo cop c) cs
addPrecedenceInfoToCurryDocCons cop (CurryDocConsOp c ty1 ty2 _ cs) =
  CurryDocConsOp c ty1 ty2 (createPrecInfo cop c) cs
addPrecedenceInfoToCurryDocCons cop (CurryDocRecord c tys fs  _ cs) =
  CurryDocRecord c tys fs' (createPrecInfo cop c) cs
  where fs' = map (addPrecedenceInfoToField cop) fs

addPrecedenceInfoToField :: [COpDecl] -> CurryDocField -> CurryDocField
addPrecedenceInfoToField cop (CurryDocField n ty _ cs) =
  CurryDocField n ty (createPrecInfo cop n) cs

addPrecedenceInfoToCurryDocDecl :: [COpDecl] -> CurryDocDecl -> CurryDocDecl
addPrecedenceInfoToCurryDocDecl cop d = case d of
  CurryDocFunctionDecl n qty sig _ cs ->
    CurryDocFunctionDecl n qty sig (createPrecInfo cop n) cs
  _                                   -> d

-- | Full analysis for functions.
createAnalysisInfoFun :: AnaInfo -> [COpDecl] -> [CFuncDecl] -> QName
                      -> AnalysisInfo
createAnalysisInfoFun ai cop funs n = AnalysisInfo
    (getNondetInfo ai n)
    (getIndetInfo ai n)
    (getOpCompleteInfo ai n)
    (getExternalInfo funs n)
    (getCompleteInfo ai n)
    (genPrecedenceInfo cop n)
    (genPropertyInfo funs n)

-- | Short analysis for functions.
createShortAnalysisInfoFun :: [COpDecl] -> [CFuncDecl] -> QName -> AnalysisInfo
createShortAnalysisInfoFun cop funs n = ShortAnalysisInfo
    (getExternalInfo funs n)
    (genPrecedenceInfo cop n)
    (genPropertyInfo funs n)

-- | Gets external status of a function by checking the number of rules.
getExternalInfo :: [CFuncDecl] -> QName -> Bool
getExternalInfo []                             _
  = error "CurryDoc.Comment.getExternalInfo: Function not found!"
getExternalInfo (CFunc     n _ _ _ []    : fs) n'
  | n =~= n'  = True
  | otherwise = getExternalInfo fs n'
getExternalInfo (CmtFunc _ n _ _ _ []    : fs) n'
  | n =~= n'  = True
  | otherwise = getExternalInfo fs n'
getExternalInfo (CFunc     n _ _ _ (_:_) : fs) n'
  | n =~= n'  = False
  | otherwise = getExternalInfo fs n'
getExternalInfo (CmtFunc _ n _ _ _ (_:_) : fs) n'
  | n =~= n'  = False
  | otherwise = getExternalInfo fs n'

-- | Only creates a precedence info (e.g. for fields and constructors).
createPrecInfo :: [COpDecl] -> QName -> AnalysisInfo
createPrecInfo cop n = PrecedenceInfo (genPrecedenceInfo cop n)

-- | Looks up the precedence for a given name.
genPrecedenceInfo :: [COpDecl] -> QName -> Maybe (CFixity, Int)
genPrecedenceInfo []                     _ = Nothing
genPrecedenceInfo (COp m fix prec : cop) n
  | n =~= m   = Just (fix, prec)
  | otherwise = genPrecedenceInfo cop n

-- | Checks for properties of a function.
--   (Code from Michael Hanus, modified by Kai Prott)
genPropertyInfo :: [CFuncDecl] -> QName -> [(Property, CRule)]
genPropertyInfo funs n = getContracts ++ concatMap getProp props
  where
    fprops = takeWhile isPropSpecFun $
             tail $ dropWhile (not . (=~=n) . funcName) funs
    (specs, props) = partition isSpecFun fprops

    getContracts = getContract (snd n ++ "'pre")  PreSpec  ++
                   getContract (snd n ++ "'post") PostSpec ++
                   getContract (snd n ++ "'spec") Spec

    getContract fn typ =
      maybe [] (getRule typ)
        (find (\fd -> snd (funcName fd) == fn) specs)

    getRule typ (CFunc     _ ar _ (CQualType _ ftype) rules) =
      map (\rule -> (typ, etaExpand ar (length (argTypes ftype)) rule)) rules
    getRule typ (CmtFunc _ _ ar _ (CQualType _ ftype) rules) =
      map (\rule -> (typ, etaExpand ar (length (argTypes ftype)) rule)) rules

    etaExpand arity tarity rule = case rule of
      CRule ps (CSimpleRhs exp ldecls) ->
        if arity == tarity
          then rule
          else let evars = map (\i -> (i,"x"++show i)) [(arity+1) .. tarity]
                in CRule (ps ++ map CPVar evars)
                         (CSimpleRhs (foldl CApply exp (map CVar evars)) ldecls)
      _ -> rule -- don't do it for complex rules

    getProp propdecl = map (\rule -> (Prop, rule)) (funcRules propdecl)

isPropSpecFun :: CFuncDecl -> Bool
isPropSpecFun fdecl = isPropFun fdecl || isSpecFun fdecl

-- | Is a function definition a property function?
isPropFun :: CFuncDecl -> Bool
isPropFun fdecl = fst (funcName fdecl) /= easyCheckModule
               && isPropType ty && funcVis fdecl == Private
 where
    isPropType :: CTypeExpr -> Bool
    isPropType ct =  ct =~~= CTCons (easyCheckModule,"Prop") -- I/O test?
                  || ct =~~= CTCons (propModule,"Prop")      -- I/O test?
                  || resultType ct =~~= CTCons (easyCheckModule,"Prop")
                  || resultType ct =~~= CTCons (propModule,"Prop")
    easyCheckModule = "Test.EasyCheck"
    propModule      = "Test.Prop"
    CQualType _ ty = funcType fdecl

-- | Is a function definition part of a specification, i.e.,
--   a full specification (suffix 'spec), a precondition (suffix 'pre),
--   or a postcondition (suffix 'post)?
isSpecFun :: CFuncDecl -> Bool
isSpecFun fdecl =
  let rfname = reverse (snd (funcName fdecl))
   in any (`isPrefixOf` rfname) ["ceps'","erp'","tsop'"]
      && funcVis fdecl == Private
