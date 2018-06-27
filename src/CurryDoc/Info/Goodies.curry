module CurryDoc.Info.Goodies where

import AbstractCurry.Types
import AbstractCurry.Select

import CurryDoc.Data.Type
import CurryDoc.Data.Ident

import List (intercalate)
import Char (isSpace)

identToQName :: Ident ->  QName
identToQName (Ident _ s _) = ("", s)

qIdentToQName :: QualIdent -> QName
qIdentToQName (QualIdent _ Nothing   idt) = identToQName idt
qIdentToQName (QualIdent _ (Just mi) idt) = (intercalate "." ms, n)
  where (_, n) = identToQName idt
        ModuleIdent _ ms = mi

(=~=) :: QName -> QName -> Bool
(   ""   , x) =~= (   ""   , y) = x == y
(   ""   , x) =~= (   (_:_), y) = x == y
(   (_:_), x) =~= (   ""   , y) = x == y
(xs@(_:_), x) =~= (ys@(_:_), y) = (xs, x) == (ys, y)

typeExprToCType :: TypeExpr -> CTypeExpr
typeExprToCType (ParenType       _ t1   ) = typeExprToCType t1
typeExprToCType (VariableType    _ n    ) = CTVar (tvIName n)
typeExprToCType (ApplyType       _ t1 t2) =
  CTApply (typeExprToCType t1) (typeExprToCType t2)
typeExprToCType (ArrowType       _ t1 t2) =
  CFuncType (typeExprToCType t1) (typeExprToCType t2)
typeExprToCType (ConstructorType _ qid  ) = CTCons (qIdentToQName qid)
typeExprToCType (ListType        _ t1   ) =
  CTApply (CTCons ("", "[]")) (typeExprToCType t1)
typeExprToCType (TupleType       _ tys  ) =
  foldl (\b a -> CTApply b (typeExprToCType a))
        (CTCons ("", "(" ++ replicate (length tys - 1) ',' ++ ")")) tys

tvIName :: Ident -> CTVarIName
tvIName n = (0, snd $ identToQName n)

(=~~=) :: CTypeExpr -> CTypeExpr -> Bool
a =~~= b = case (a,b) of
  (CTVar (_, n1), CTVar (_, n2)) -> n1 == n2
  _                              -> a  == b

contextToCContext :: Context -> CContext
contextToCContext cs = CContext (map constraintToCConstraint cs)

constraintToCConstraint :: Constraint -> CConstraint
constraintToCConstraint (Constraint _ qid ty) =
  (qIdentToQName qid, typeExprToCType ty)

getConstrName   :: ConstrDecl -- ^ ConstrDecl
  {- | Ident -} -> QName
getConstrName (ConstrDecl _ _ _   idt _) = identToQName idt
getConstrName (ConOpDecl  _ _ _ _ idt _) = identToQName idt
getConstrName (RecordDecl _ _ _   idt _) = identToQName idt

getNewtypeConstrName :: NewConstrDecl -- ^ NewConstrDecl
       {- | Ident -} -> QName
getNewtypeConstrName (NewConstrDecl _ idt _) = identToQName idt
getNewtypeConstrName (NewRecordDecl _ idt _) = identToQName idt

cFieldType :: CFieldDecl -> CTypeExpr
cFieldType (CField _ _ ty) = ty

fieldType :: FieldDecl -> CTypeExpr
fieldType (FieldDecl _ _ ty) = typeExprToCType ty

isPublicField :: CFieldDecl -> Bool
isPublicField (CField _ Public  _ ) = True
isPublicField (CField _ Private _ ) = False

publicFieldNames :: CurryProg -> [QName]
publicFieldNames = concatMap publicFieldNames' . constructors
  where publicFieldNames' (CCons   _ _ _ _ _ ) = []
        publicFieldNames' (CRecord _ _ _ _ fs) =
          map (\(CField n _ _) -> n) $ filter isPublicField fs

publicClassNames :: CurryProg -> [QName]
publicClassNames (CurryProg _ _ _ cls _ _ _ _) =
  map publicClassName cls
  where publicClassName (CClass n _ _ _ _) = n

isApplyType :: CTypeExpr -> Bool
isApplyType t = case t of
  CTApply _ _ -> True
  _           -> False

isFunctionType :: CTypeExpr -> Bool
isFunctionType t = case t of
  CFuncType _ _ -> True
  _             -> False

concatCommentStrings :: [String] -> String
concatCommentStrings ss = unwords (map replaceEmptyLine ss) -- TODO: improve unwording and line replacement

replaceEmptyLine :: String -> String
replaceEmptyLine ss | all isSpace ss = "\n"
                  | otherwise      = trimSpace ss

trimSpace :: String -> String
trimSpace = reverse . dropWhile isSpace . reverse . dropWhile isSpace
