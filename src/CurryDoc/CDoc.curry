----------------------------------------------------------------------
--- Functions to generate documentation in "CDoc" format.
---
--- @author Sandra Dylus
--- @version December 2024
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

      funcInfos = map funcInfo
                      (filter (\ (Func _ _ vis _ _) -> vis == Public) functions)

      typeInfos = map typeInfo (concatMap filterT types)

  putStrLn $ "Writing " ++ modName ++ ".cdoc file"
  return $ showTerm (CurryInfo modInfo funcInfos typeInfos)
 where
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
--- * the corresponding functions
--- * the corresponding data and type declaration
data CurryInfo = CurryInfo ModuleInfo [FunctionInfo] [TypeInfo]
  deriving (Read, Show)

--- The basic information about some module contains
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
--- * True if property ist defined non-deterministically
--- * the flex/rigid status
data FunctionInfo =
  FunctionInfo String TypeExpr String String Bool FlexRigidResult
  deriving (Read, Show)

--- The information about types defined in a Curry module contains
--- * the name
--- * the signature (true indicates a type synonym, false a data type)
--- * a list of constructors and their argument types (or the type name
---   and the type expression in case of type synonyms)
--- * a list of type variables (i.e., non-empty for a polymoprhic type)
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
