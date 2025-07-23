----------------------------------------------------------------------
--- Functions to generate documentation in "CDoc" format.
---
--- @author Sandra Dylus
--- @version July 2025
----------------------------------------------------------------------

module CurryDoc.CDoc where

import Data.List

import FlatCurry.Types
import FlatCurry.Files
import FlatCurry.FlexRigid
import ReadShowTerm

import CurryDoc.AnaInfo
import CurryDoc.Read

--- Generates the documentation of a module in "CDoc" format.
generateCDoc :: String  -> String -> [(SourceLine,String)] -> AnaInfo
             -> IO String
generateCDoc modName modCmts progCmts anaInfo = do
  fcyName <- getFlatCurryFileInLoadPath modName
  Prog _ _ types functions _ <- readFlatCurryFile fcyName
  let modInfo = ModuleInfo modName (author avCmts) mCmts

      funcInfo (Func qName@(mName, fName) _ _ tExpr rule) =
        FunctionInfo fName
        (removeForall tExpr)
        mName
        (funcComment fName progCmts)
        (getNondetInfo anaInfo qName)
        (flexRigid rule)

      typeInfo (Type (mName, tName) _ vars consDecl) =
        TypeInfo tName
        (map consSignature
             (filter (\ (Cons _ _ vis _) -> vis == Public) consDecl))
        (map fst vars)
        mName
        (dataComment tName progCmts)
        False
      typeInfo (TypeNew (mName, tName) _ vars newconsDecl) =
        TypeInfo tName
        (map newconsSignature
             (filter (\ (NewCons _ vis _) -> vis == Public) [newconsDecl]))
        (map fst vars)
        mName
        (dataComment tName progCmts)
        False
      typeInfo (TypeSyn qName@(mName, tName) _ vars tExpr) =
        TypeInfo tName
        [(qName, [removeForall tExpr])]
        (map fst vars)
        mName
        (dataComment tName progCmts)
        True

      (mCmts, avCmts) = splitComment modCmts

      funcInfos = map funcInfo (filter isPublicFunc functions)

      typeInfos = map typeInfo (concatMap filterT types)

  putStrLn $ "Writing " ++ modName ++ ".cdoc file"
  return $ showTerm (CurryInfo modInfo funcInfos typeInfos)
 where
  -- is the function public and not a class function?
  isPublicFunc (Func (_,f) _ vis _ _) = vis == Public && '#' `notElem` f

  filterT f@(Type _    vis _ _) = if vis == Public then [f] else []
  filterT f@(TypeSyn _ vis _ _) = if vis == Public then [f] else []
  filterT f@(TypeNew _ vis _ _) = if vis == Public then [f] else []

-- Strip forall type quantifiers in order to keep compatibility
-- with Currygle 0.3.0:
removeForall :: TypeExpr -> TypeExpr
removeForall texp = case texp of
  ForallType _ te  -> removeForall te
  FuncType te1 te2 -> FuncType (removeForall te1) (removeForall te2)
  TCons qn tes     -> TCons qn (map removeForall tes)
  TVar _           -> texp

funcComment :: String -> [(SourceLine,String)] -> String
funcComment str = fst . splitComment . getFuncComment str

dataComment :: String -> [(SourceLine,String)] -> String
dataComment str = fst . splitComment . getDataComment str

flexRigid :: Rule -> FlexRigidResult
flexRigid (Rule _ expr) = getFlexRigid expr
flexRigid (External _)  = UnknownFR

--- The information about a Curry module contains
--- * the module information
--- * information about the function defined in the module
--- * information about types (also newtypes and classes) defined in the module
data CurryInfo = CurryInfo ModuleInfo [FunctionInfo] [TypeInfo]
  deriving (Read, Show)

--- The base information about some module contains
--- * the name
--- * the author
--- * the description
data ModuleInfo = ModuleInfo String String String
  deriving (Read, Show)

--- The information about functions defined in a Curry module contains
--- * the name
--- * the signature
--- * the corresponding module
--- * the description
--- * `True` if the function is non-deterministically defined
--- * the flex/rigid status of the function
data FunctionInfo =
  FunctionInfo String TypeExpr String String Bool FlexRigidResult
  deriving (Read, Show)

--- The information about types defined in a Curry module contains
--- * the name (which has `_Dict#` as a prefix if it is a class)
--- * a list of constructor names and their argument types (or the type name
---   and the type expression in case of type synonyms)
--- * a list of type variables (which is non-empty for a polymorphic type)
--- * the corresponding module
--- * the description
--- * a flag which is `True` if it is a type synonym
data TypeInfo =
  TypeInfo String [(QName, [TypeExpr])] [TVarIndex] String String Bool
  deriving (Read,Show)

-- auxiliaties --------------------------------------------------------

author :: [(String, String)] -> String
author av = concat $ getCommentType "author" av

-- generate data and type constructors
consSignature :: ConsDecl -> (QName, [TypeExpr])
consSignature (Cons (mName, cName) _ _ tExprList) =
  ((mName, cName), map removeForall tExprList)

-- generate data and type constructors
newconsSignature :: NewConsDecl -> (QName, [TypeExpr])
newconsSignature (NewCons (mName, cName) _ tExpr) =
  ((mName, cName), map removeForall [tExpr])
