{- |
     Author  : Kai-Oliver Prott
     Version : August 2018

     Some useful Operations on all kinds of datastructures that
     are needed in CurryDoc.
-}
module CurryDoc.Info.Goodies where

import AbstractCurry.Types
import AbstractCurry.Select

import CurryDoc.Data.Type
import CurryDoc.Data.Ident

import List (intercalate)
import Char (isSpace)

-- | Transform identifier to QName with empty module qualifier
identToQName :: Ident ->  QName
identToQName (Ident _ s _) = ("", s)

-- | Transform qualified identifier to QName
qIdentToQName :: QualIdent -> QName
qIdentToQName (QualIdent _ Nothing   idt) = identToQName idt
qIdentToQName (QualIdent _ (Just mi) idt) = (mIdentToMName mi, n)
  where (_, n) = identToQName idt

-- | Transform module identifier to MName
mIdentToMName :: ModuleIdent -> MName
mIdentToMName (ModuleIdent _ ms) = intercalate "." ms

-- | Relaxed equality on QNames.
--   Only compares qualifier if present in both arguments.
(=~=) :: QName -> QName -> Bool
(   ""   , x) =~= (   ""   , y) = x == y
(   ""   , x) =~= (   (_:_), y) = x == y
(   (_:_), x) =~= (   ""   , y) = x == y
(xs@(_:_), x) =~= (ys@(_:_), y) = (xs, x) == (ys, y)

-- | Trans from AST-TypeExpr to FlatCurry/AbstractCurry TypeExpr
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

-- | Transform an identifier to the name of a type variable
tvIName :: Ident -> CTVarIName
tvIName n = (0, snd $ identToQName n)

-- | Relaxed equality on CTypeExpr.
(=~~=) :: CTypeExpr -> CTypeExpr -> Bool
a =~~= b = case (a,b) of
  (CTVar (_, n1), CTVar (_, n2)) -> n1 == n2
  _                              -> a  == b

-- | Trans from AST-Context to FlatCurry/AbstractCurry Context
contextToCContext :: Context -> CContext
contextToCContext cs = CContext (map constraintToCConstraint cs)

-- | Trans from AST-Constraint to FlatCurry/AbstractCurry Constraint
constraintToCConstraint :: Constraint -> CConstraint
constraintToCConstraint (Constraint _ qid ty) =
  (qIdentToQName qid, typeExprToCType ty)

-- | Get all imports of a module
getImports :: Module a -> [ImportDecl]
getImports (Module _ _ _ _ im _) = im

-- | Get the QName of a constructor
getConstrName   :: ConstrDecl -> QName
getConstrName (ConstrDecl _ _ _   idt _) = identToQName idt
getConstrName (ConOpDecl  _ _ _ _ idt _) = identToQName idt
getConstrName (RecordDecl _ _ _   idt _) = identToQName idt

-- | Get the QName of a newtype constructor
getNewtypeConstrName :: NewConstrDecl -> QName
getNewtypeConstrName (NewConstrDecl _ idt _) = identToQName idt
getNewtypeConstrName (NewRecordDecl _ idt _) = identToQName idt

-- | Get the Type of a Field in FlatCurry/AbstractCurry
cFieldType :: CFieldDecl -> CTypeExpr
cFieldType (CField _ _ ty) = ty

-- | Get the Type of a Field
fieldType :: FieldDecl -> CTypeExpr
fieldType (FieldDecl _ _ ty) = typeExprToCType ty

isPublicField :: CFieldDecl -> Bool
isPublicField (CField _ Public  _ ) = True
isPublicField (CField _ Private _ ) = False

-- | Get the QNames of all public fields
publicFieldNames :: CurryProg -> [QName]
publicFieldNames = concatMap publicFieldNames' . constructors
  where publicFieldNames' (CCons   _ _ _ _ _ ) = []
        publicFieldNames' (CRecord _ _ _ _ fs) =
          map (\(CField n _ _) -> n) $ filter isPublicField fs

-- | Get the QNames of all public classes
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

-- | Combine multiple comment strings to a single string
concatCommentStrings :: [String] -> String
concatCommentStrings ss = unwords (map replaceEmptyLine ss)
 where  replaceEmptyLine ss | all isSpace ss = "\n"
                            | otherwise      = trimSpace ss

trimSpace :: String -> String
trimSpace = reverse . dropWhile isSpace . reverse . dropWhile isSpace
