----------------------------------------------------------------------
--- Functions to generate documentation in TeX format.
---
--- @author Michael Hanus
--- @version October 2017
----------------------------------------------------------------------

module CurryDoc.Generators.TeX (generateTexDocs) where

import Char
import Distribution
import List
import Maybe

import CurryDoc.Options
import CurryDoc.Info
import AbstractCurry.Types
import AbstractCurry.Select
import HTML.Base
import HTML.LaTeX  ( showLatexExps )
import HTML.Parser
import Markdown

--------------------------------------------------------------------------
-- Generates the documentation of a module in HTML format where the comments
-- are already analyzed.
generateTexDocs :: DocOptions -> CurryDoc -> IO String
generateTexDocs docopts (CurryDoc mname mhead insts typesigs decls _ _) =
  let textypes   = concatMap (genHtmlTexType  docopts insts) decls
      texfuncs   = concatMap (genHtmlTexFunc  docopts typesigs) decls
      texclasses = concatMap (genHtmlTexClass docopts) decls
  in return $
    "\\currymodule{"++mname++"}\n" ++
    genHtmlTexModule docopts mhead ++ "\n" ++
    (if null textypes   then ""
     else "\\currytypesstart\n"   ++ textypes   ++ "\\currytypesstop\n") ++
    (if null texfuncs   then ""
     else "\\curryfuncstart\n"    ++ texfuncs   ++ "\\curryfuncstop\n") ++
    (if null texclasses then ""
     else "\\curryclassesstart\n" ++ texclasses ++ "\\curryclassesstop\n")

--- Translate a documentation comment to LaTeX and use markdown translation
--- if necessary. If the string contains HTML tags, these are also
--- translated into LaTeX.
htmlString2Tex :: DocOptions -> String -> String
htmlString2Tex docopts cmt =
  if withMarkdown docopts
  then markdownText2LaTeX (replaceIdLinks cmt)
  else showLatexExps (parseHtmlString (replaceIdLinks cmt))

-- replace identifier hyperlinks in a string (i.e., enclosed in single quotes)
-- by code markdown:
replaceIdLinks :: String -> String
replaceIdLinks str = case str of
  [] -> []
  ('\\':'\'':cs) -> '\'' : replaceIdLinks cs
  (c:cs) -> if c=='\'' then tryReplaceIdLink [] cs
                       else c : replaceIdLinks cs
 where
  tryReplaceIdLink ltxt [] = '\'' : reverse ltxt
  tryReplaceIdLink ltxt (c:cs)
   | isSpace c = '\'' : reverse ltxt ++ c : replaceIdLinks cs -- no space in id
   | c == '\'' = checkId (reverse ltxt) ++ replaceIdLinks cs
   | otherwise = tryReplaceIdLink (c:ltxt) cs

  checkId s = if ' ' `elem` s
              then '\'' : s ++ ['\'']
              else "<code>"++s++"</code>"

-- generate TeX documentation for a function
genHtmlTexFunc :: DocOptions -> [CommentedDecl] -> CommentedDecl -> String
genHtmlTexFunc docopts sigs d = case d of
  CommentedFunctionDecl (fmod, fname) cs (Just ty) _
    -> "\\curryfunctionstart{" ++ string2tex fname ++ "}{" ++
       "\\curryfuncsig{" ++ string2tex (showId fname) ++ "}{" ++
         showQualTexType docopts fmod ty ++ "}}\n" ++
         htmlString2Tex docopts(
           concatCommentStrings (map commentString
             (getTypesigComment (fmod, fname) sigs))) ++
         htmlString2Tex docopts
           (concatCommentStrings (map commentString cs)) ++
       "\\curryfunctionstop\n"
  _ -> ""

getTypesigComment :: QName -> [CommentedDecl] -> [Comment]
getTypesigComment _ []     = []
getTypesigComment n (d:ds) = case d of
  CommentedTypeSig [n'] cs _ _
    | n' =~= n -> cs
  _            -> getTypesigComment n ds

--- generate TeX documentation for a datatype
genHtmlTexType :: DocOptions -> [CommentedDecl] -> CommentedDecl -> String
genHtmlTexType docopts insts d = case d of
  CommentedDataDecl (tcmod,tcons) vs cs constrs ->
    "\\currydatastart{" ++ tcons ++ "}\n" ++
    htmlString2Tex docopts
      (concatCommentStrings (map commentString cs)) ++
    "\n\\currydatacons\n" ++
    concatMap (genHtmlTexCons docopts tcons vs) constrs ++
    "\n\\currydatainsts\n" ++
    concatMap (genHtmlTexInst docopts tcmod)
       (filter (((tcmod,tcons) =~=) . instTypeName) insts) ++
    "\\currydatastop\n"
  CommentedNewtypeDecl (tcmod,tcons) vs cs cons ->
    "\\currydatastart{" ++ tcons ++ "}\n" ++
    htmlString2Tex docopts
      (concatCommentStrings (map commentString cs)) ++
    "\n\\currydatacons\n" ++
    genHtmlTexNewCons docopts tcons vs cons ++
    "\n\\currydatainsts\n" ++
    concatMap (genHtmlTexInst docopts tcmod)
       (filter (((tcmod,tcons) =~=) . instTypeName) insts) ++
    "\\currydatastop\n"
  CommentedTypeDecl (tcmod,tcons) vs ty cs ->
    "\\currytypesynstart{" ++ tcons ++ "}{" ++
    (if tcons=="String" && tcmod=="Prelude"
     then "String = [Char]"
     else tcons ++ " " ++ unwords (map snd vs) ++ "="
           ++ showTexType docopts tcmod False ty) ++ "}\n" ++
    htmlString2Tex docopts
       (concatCommentStrings (map commentString cs)) ++
    "\\currytypesynstop\n\n"
  _ -> ""

genHtmlTexInst :: DocOptions -> String -> CommentedDecl -> String
genHtmlTexInst docopts modname d = case d of
  CommentedInstanceDecl (cmod, cname) cx ty _ _ ->
    "\n" ++
    (if null cxString then "" else cxString ++ " ") ++
    "\\textbf{" ++ cname ++ "} " ++
    showTexType docopts modname (isApplyType ty || isFunctionType ty) ty ++ "\n\n"
    where cxString = showTexContext docopts cmod cx
  _ -> ""

--- generate HTML documentation for a constructor if it is exported:
genHtmlTexCons :: DocOptions -> String -> [CTVarIName] -> CommentedConstr -> String
genHtmlTexCons docopts vs ds (CommentedConsOp (cmod, cname) cs ty1 ty2 ai) =
  genHtmlTexCons docopts vs ds (CommentedConstr (cmod, "(" ++ cname ++ ")")
                            cs [ty1, ty2] ai) -- TODO: maybe different?
genHtmlTexCons docopts tcons vs (CommentedConstr (cmod, cname) cs tys _) =
 "\\curryconsstart{" ++ cname ++ "}{" ++
      concatMap (\t-> showTexType docopts cmod True t ++ " $\\to$ ") tys ++
      tcons ++ " " ++ unwords (map snd vs) ++ "}\n" ++
      htmlString2Tex docopts (unwords (map commentString cs))
genHtmlTexCons docopts tcons vs (CommentedRecord (cmod,cname) cs tys fs _) =
 "\\curryconsstart{" ++ cname ++ "}{" ++
      concatMap (\t-> showTexType docopts cmod True t ++ " $\\to$ ") tys ++
      tcons ++ " " ++ unwords (map snd vs) ++ "}\n" ++
      htmlString2Tex docopts (unwords (map commentString cs)) ++
      intercalate "\n" (map (genHtmlTexField docopts) fs)

-- generate HTML documentation for record fields
genHtmlTexField :: DocOptions -> CommentedField -> String
genHtmlTexField _       ([]            , _ , _ ) = []
genHtmlTexField _       ((_ : _ : _)   , _ , _ ) = []
genHtmlTexField docopts ([(fmod,fname)], cs, ty) =
  "\\curryfieldstart{" ++ fname ++ "}{" ++
    fname ++ " :: " ++ showTexType docopts fmod False ty ++ "}" ++
    if null txt then [] else
      " : " ++ htmlString2Tex docopts txt
  where txt = unwords (map commentString cs)

genHtmlTexNewCons :: DocOptions -> String -> [CTVarIName]
                  -> Maybe CommentedNewtypeConstr -> String
genHtmlTexNewCons _       _  _  Nothing = []
genHtmlTexNewCons docopts dn vs (Just (CommentedNewConstr cn cs ty ai)) =
  genHtmlTexCons docopts dn vs (CommentedConstr cn cs [ty] ai)
genHtmlTexNewCons docopts dn vs (Just (CommentedNewRecord cn cs ty f ai)) =
  genHtmlTexCons docopts dn vs (CommentedRecord cn cs [ty] fs ai)
  where fs = case f of
               Just f' -> [f']
               Nothing -> []

-- generate HTML documentation for a function:
genHtmlTexClass :: DocOptions -> CommentedDecl -> String
genHtmlTexClass docopts d = case d of
  CommentedClassDecl (cmod, cname) cx v cs ds
    -> "\\curryclassstart{" ++ cname ++ " " ++
       (if null cxString then "" else cxString ++ " ") ++
       snd v ++ "}{" ++
       htmlString2Tex docopts
         (concatCommentStrings (map commentString cs)) ++ "\n" ++
       concatMap (genHtmlTexFunc docopts sigs) other ++
       "\\curryclassstop\n"
    where cxString = showTexContext docopts cmod cx
          (sigs, other) = partition isCommentedTypeSig ds
  _ -> ""

--- generate Tex documentation for a module:
genHtmlTexModule :: DocOptions -> ModuleHeader -> String
genHtmlTexModule docopts (ModuleHeader fields maincmt) =
  htmlString2Tex docopts maincmt ++
  concatMap fieldHtml fields
  where fieldHtml (typ, value) =
          "\\textbf{" ++ show typ ++ ": } " ++ value ++ "\n\n"

-- Pretty printer for qualified types in Curry syntax:
showQualTexType :: DocOptions -> String -> CQualTypeExpr -> String
showQualTexType opts mod (CQualType ctxt texp) =
  unwords [showTexContext opts mod ctxt, showTexType opts mod False texp]

showTexContext :: DocOptions -> String -> CContext -> String
showTexContext _ _ (CContext []) = ""
showTexContext opts mod (CContext [clscon]) =
  showTexConstraint opts mod clscon ++ " =>"
showTexContext opts mod (CContext ctxt@(_:_:_)) =
  bracketsIf True (intercalate ", " (map (showTexConstraint opts mod) ctxt))
    ++ " =>"

--- Pretty-print a single class constraint.
showTexConstraint :: DocOptions -> String -> CConstraint -> String
showTexConstraint opts mod (cn,texp) =
  "\\textbf{" ++ snd cn ++ "} " ++ showTexType opts mod True texp

-- Pretty printer for types in Curry syntax as TeX string.
-- first argument is True iff brackets must be written around complex types
showTexType :: DocOptions -> String -> Bool -> CTypeExpr -> String
showTexType opts mod nested texp = case texp of
  CTVar (_,n) -> n -- TODO: use name given in source program instead?
  CFuncType t1 t2 ->
    bracketsIf nested (showTexType opts mod (isFunctionalType t1) t1++" $\\to$ "++
                     showTexType opts mod False t2)
  CTCons tc -> showTexTConsType opts mod nested tc []
  CTApply t1 t2 ->
       maybe (bracketsIf nested $
                showTexType opts mod True t1 ++ " " ++ showTexType opts mod True t2)
             (\ (tc,ts) -> showTexTConsType opts mod nested tc ts)
             (tconsArgsOfType texp)

showTexTConsType :: DocOptions -> String -> Bool -> QName -> [CTypeExpr] -> String
showTexTConsType opts mod nested tc ts
 | ts==[]  = snd tc
 | tc==("Prelude","[]") && (head ts == CTCons ("Prelude","Char"))
   = "String"
 | tc==("Prelude","[]")
   = "[" ++ showTexType opts mod False (head ts) ++ "]" -- list type
 | take 2 (snd tc) == "(,"                      -- tuple type
   = "(" ++ concat (intersperse "," (map (showTexType opts mod False) ts)) ++ ")"
 | otherwise
   = bracketsIf nested
      (snd tc ++ " " ++
       concat (intersperse " " (map (showTexType opts mod True) ts)))

-- convert string into TeX:
string2tex :: String -> String
string2tex = concatMap char2tex
 where
  char2tex c | c==chr 228  = "\\\"a"
             | c==chr 246  = "\\\"o"
             | c==chr 252  = "\\\"u"
             | c==chr 196  = "\\\"A"
             | c==chr 214  = "\\\"O"
             | c==chr 220  = "\\\"U"
             | c==chr 223  = "\\ss{}"
             | c=='\\'     = "{\\symbol{92}}"
             | c=='^'      = "{\\symbol{94}}"
             | c=='~'      = "{\\symbol{126}}"
             | c=='<'      = "{$<$}"
             | c=='>'      = "{$>$}"
             | c=='_'      = "\\_"
             | c=='#'      = "\\#"
             | c=='$'      = "\\$"
             | c=='%'      = "\\%"
             | c=='{'      = "\\{"
             | c=='}'      = "\\}"
             | c=='&'      = "\\&"
             | otherwise   = [c]

bracketsIf :: Bool -> String -> String
bracketsIf False s = s
bracketsIf True  s = "("++s++")"

-- enclose a non-letter identifier in brackets:
showId :: String -> String
showId name = if isAlpha (head name) then name
                                     else ('(':name)++")"
