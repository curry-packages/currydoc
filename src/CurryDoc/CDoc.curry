----------------------------------------------------------------------
--- Functions to generate documentation in "CDoc" format.
---
--- @author Sandra Dylus
--- @version November 2015
----------------------------------------------------------------------

module CurryDoc.CDoc where

import CurryDoc.AnaInfo
import CurryDoc.Params
import CurryDoc.Read
import FlatCurry.Types
import FlatCurry.Files
import FlatCurry.FlexRigid
import List
import ReadShowTerm

generateCDoc :: String  -> String -> [(SourceLine,String)] -> AnaInfo
             -> IO String
generateCDoc modName modCmts progCmts anaInfo = do
  fcyName <- getFlatCurryFileInLoadPath modName
  Prog _ _ types functions _ <- readFlatCurryFile fcyName
  let modInfo = ModuleInfo modName (author avCmts) mCmts
      funcInfo (Func qName@(mName, fName) _ _ tExpr rule) =
        FunctionInfo fName
        tExpr
        mName
        (funcComment fName progCmts)
        (getNondetInfo anaInfo qName)
        (flexRigid rule)
      typeInfo (Type (mName, tName) _ vars consDecl) =
        TypeInfo tName
        (map consSignature (filter (\(Cons _ _ vis _) -> vis == Public) consDecl))
        vars
        mName
        (dataComment tName progCmts)
        False
      typeInfo (TypeSyn qName@(mName, tName) _ vars tExpr) =
        TypeInfo tName
        [(qName, [tExpr])]
        vars
        mName
        (dataComment tName progCmts)
        True
      (mCmts, avCmts) = splitComment modCmts
      funcInfos = map funcInfo (filter (\(Func _ _ vis _ _) -> vis == Public) functions)
      typeInfos = map typeInfo (concatMap filterT types)
  putStrLn $ "Writing " ++ modName ++ ".cdoc file"
  return $ showTerm (CurryInfo modInfo funcInfos typeInfos)
 where
   filterT f@(Type _ vis _ _) = if vis == Public then [f] else []
   filterT f@(TypeSyn _ vis _ _) = if vis == Public then [f] else []

funcComment :: String -> [(SourceLine,String)] -> String
funcComment str = fst . splitComment . getFuncComment str

dataComment :: String -> [(SourceLine,String)] -> String
dataComment str = fst . splitComment . getDataComment str

flexRigid :: Rule -> FlexRigidResult
flexRigid (Rule _ expr) = getFlexRigid expr
flexRigid (External _)  = UnknownFR

-- the name
-- the author
-- the description
data ModuleInfo = ModuleInfo String String String

-- the module
-- the corresponding functions
-- the corresponding data and type declaration
data CurryInfo = CurryInfo ModuleInfo [FunctionInfo] [TypeInfo]

-- the name
-- the signature
-- the corresponding module
-- the description
-- True if property ist defined non-deterministically
-- the flex/rigid status
data FunctionInfo = FunctionInfo String TypeExpr String String Bool FlexRigidResult

-- the name
-- the signature (true indicates a type synonym, false a data type)
-- the corresponding module
-- the description
data TypeInfo = TypeInfo String [(QName, [TypeExpr])] [TVarIndex] String String Bool

-- auxilieres --------------------------------------------------------

author :: [(String, String)] -> String
author av = concat $ getCommentType "author" av

-- generate data and type constructors
consSignature :: ConsDecl -> (QName, [TypeExpr])
consSignature (Cons (mName, cName) _ _ tExprList) = ((mName, cName), tExprList)
