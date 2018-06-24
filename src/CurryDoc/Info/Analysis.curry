module CurryDoc.Info.Analysis (addAnaInfoToCommentDecls) where

import CurryDoc.Data.AnaInfo
import CurryDoc.Info.Comments
import CurryDoc.Info.Goodies

import AbstractCurry.Types
import AbstractCurry.Select

import List (find, partition, isPrefixOf)

--- Adds analysis information to suitable CommentedDecls
addAnaInfoToCommentDecls :: AnaInfo -> [COpDecl] -> [CFuncDecl]
                         -> [CommentedDecl] -> [CommentedDecl]
addAnaInfoToCommentDecls ai cop funs = map (addAnaInfoToCommentDecl ai cop funs)

addAnaInfoToCommentDecl :: AnaInfo -> [COpDecl] -> [CFuncDecl] -> CommentedDecl
                        -> CommentedDecl
addAnaInfoToCommentDecl _  _   _    d@(UnsupportedDecl                _) = d
addAnaInfoToCommentDecl _  _   _    d@(CommentedExternalDecl    _ _ _ _) = d
addAnaInfoToCommentDecl _  _   _    d@(CommentedTypeDecl        _ _ _ _) = d
addAnaInfoToCommentDecl _  _   _    d@(CommentedTypeSig         _ _ _ _) = d
addAnaInfoToCommentDecl _  _   _    d@(CommentedInstanceDecl  _ _ _ _ _) = d
addAnaInfoToCommentDecl _  cop _      (CommentedClassDecl     a b c d e) =
  CommentedClassDecl a b c d (map (addPrecedenceInfoToCommentDecl cop) e)
  -- CommentedClassDecl a b (addAnaInfoToCommentDecls ai cop c) -- not reasonably possible for classes
addAnaInfoToCommentDecl ai cop funs    (CommentedFunctionDecl n qty cs _) =
  CommentedFunctionDecl n qty cs (createAnalysisInfoFun ai cop funs n)
addAnaInfoToCommentDecl _  cop _       (CommentedDataDecl     idt vs cs cns) =
  CommentedDataDecl idt vs cs (map add cns)
  where add (CommentedConstr c ccs tys _) =
             CommentedConstr c ccs tys (createPrecInfo cop c)
        add (CommentedRecord c ccs tys fs  _) =
             CommentedRecord c ccs tys fs (createPrecInfo cop c)
        add (CommentedConsOp c ccs ty1 ty2 _) =
             CommentedConsOp c ccs ty1 ty2 (createPrecInfo cop c)
addAnaInfoToCommentDecl _  cop _       (CommentedNewtypeDecl idt vs cs cns) =
  CommentedNewtypeDecl idt vs cs (fmapMaybe add cns)
  where add (CommentedNewConstr c ccs ty _) =
             CommentedNewConstr c ccs ty (createPrecInfo cop c)
        add (CommentedNewRecord c ccs ty f  _) =
             CommentedNewRecord c ccs ty f (createPrecInfo cop c)
        fmapMaybe _ Nothing  = Nothing
        fmapMaybe f (Just x) = Just  (f x)

addPrecedenceInfoToCommentDecl :: [COpDecl] -> CommentedDecl -> CommentedDecl
addPrecedenceInfoToCommentDecl cop d = case d of
  CommentedFunctionDecl n qty cs _ ->
    CommentedFunctionDecl n qty cs (createPrecInfo cop n)
  _                                -> d

createAnalysisInfoFun :: AnaInfo -> [COpDecl] -> [CFuncDecl] -> QName
                      -> AnalysisInfo
createAnalysisInfoFun ai cop funs n = AnalysisInfo {
    nondet = getNondetInfo ai n,
    indet  = getIndetInfo ai n,
    opComplete = getOpCompleteInfo ai n,
    complete = getCompleteInfo ai n,
    ext = getExternalInfo funs n,
    precedence = genPrecedenceInfo cop n,
    property = genPropertyInfo funs n
  }

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

createPrecInfo :: [COpDecl] -> QName -> AnalysisInfo
createPrecInfo cop n = PrecedenceInfo {
    precedence = genPrecedenceInfo cop n
  }

genPrecedenceInfo :: [COpDecl] -> QName -> Maybe (CFixity, Int)
genPrecedenceInfo []                     _ = Nothing
genPrecedenceInfo (COp m fix prec : cop) n
  | n =~= m   = Just (fix, prec)
  | otherwise = genPrecedenceInfo cop n

genPropertyInfo :: [CFuncDecl] -> QName -> [(Property, CRule)]
genPropertyInfo funs n = getContracts ++ concatMap getProp props
  where
    fprops = takeWhile isPropSpecFun $ tail $ dropWhile ((=~=n) . funcName) funs
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

isPropFun :: CFuncDecl -> Bool
isPropFun fdecl = fst (funcName fdecl) /= easyCheckModule
               && isPropType ty && funcVis fdecl == Private
 where
    isPropType :: CTypeExpr -> Bool
    isPropType ct =  ct =~~= CTCons (easyCheckModule,"Prop") -- I/O test?
                  || ct =~~= CTCons (propModule,"Prop") -- I/O test?
                  || resultType ct =~~= CTCons (easyCheckModule,"Prop")
                  || resultType ct =~~= CTCons (propModule,"Prop")
    easyCheckModule = "Test.EasyCheck"
    propModule      = "Test.Prop"
    CQualType _ ty = funcType fdecl

-- Is a function definition part of a specification, i.e.,
-- a full specification (suffix 'spec), a precondition (suffix 'pre),
-- or a postcondition (suffix 'post)?
isSpecFun :: CFuncDecl -> Bool
isSpecFun fdecl =
  let rfname = reverse (snd (funcName fdecl))
   in any (`isPrefixOf` rfname) ["ceps'","erp'","tsop'"]
      && funcVis fdecl == Private
