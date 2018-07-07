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

 -- TODO: The generated .tex does not have all the information that .html has
 -- This has also been the case in prior versions

--------------------------------------------------------------------------
-- | Generates the documentation of a module in Tex format
--   from CurryDoc datastructure
generateTexDocs :: DocOptions -> CurryDoc -> IO String
generateTexDocs docopts (CurryDoc mname mhead ex _) =
  return $ "\\currymodule{"++mname++"}\n" ++
           genTexModule docopts mhead ++ "\n" ++
           genTexForExport docopts ex

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

genTexForExport :: DocOptions -> [ExportEntry CurryDocDecl] -> String
genTexForExport _   []                                  = ""
genTexForExport doc (ExportSection c nesting ex : rest) =
  let innerTex = genTexForExport doc ex
      outerTex = genTexForExport doc rest
  in getSubsectionCommand nesting ++
     htmlString2Tex doc (commentString c) ++ "\n\n" ++
     innerTex ++ "\n\n" ++ outerTex
  where getSubsectionCommand n = case n of
                    2 -> "\\paragraph*"
                    1 -> "\\subsubsection*"
                    3 -> "\\subparagraph*"
genTexForExport doc (ExportEntryModule _ : rest) =
  genTexForExport doc rest -- TODO: show export of modules
genTexForExport doc (ExportEntry decl : rest)
  | isCurryDocFuncDecl  decl = genTexFunc  doc decl ++ restTex
  | isCurryDocClassDecl decl = genTexClass doc decl ++ restTex
  | otherwise                = genTexType  doc decl ++ restTex
  where restTex = genTexForExport doc rest

-- generate TeX documentation for a function
genTexFunc :: DocOptions -> CurryDocDecl -> String
genTexFunc docopts d = case d of
  CurryDocFunctionDecl (fmod, fname) ty sig _ cs
    -> "\\curryfunctionstart{" ++ string2tex fname ++ "}{" ++
       "\\curryfuncsig{" ++ string2tex (showId fname) ++ "}{" ++
         showQualTexType docopts fmod ty ++ "}}\n" ++
         htmlString2Tex docopts(
           concatCommentStrings (map commentString
             (maybe [] getTypesigComments sig))) ++
         htmlString2Tex docopts
           (concatCommentStrings (map commentString cs)) ++
       "\\curryfunctionstop\n"
  _ -> ""

--- generate TeX documentation for a datatype
genTexType :: DocOptions -> CurryDocDecl -> String
genTexType docopts d = case d of
  CurryDocDataDecl (tcmod,tcons) vs insts _ constrs cs ->
    "\\currydatastart{" ++ tcons ++ "}\n" ++
    htmlString2Tex docopts
      (concatCommentStrings (map commentString cs)) ++
    "\n\\currydatacons\n" ++
    concatMap (genTexCons docopts tcons vs) constrs ++
    "\n\\currydatainsts\n" ++
    concatMap (genTexInst docopts tcmod) insts ++
    "\\currydatastop\n"
  CurryDocNewtypeDecl (tcmod,tcons) vs insts cons cs ->
  -- TODO: distinguish from data
    "\\currydatastart{" ++ tcons ++ "}\n" ++
    htmlString2Tex docopts
      (concatCommentStrings (map commentString cs)) ++
    "\n\\currydatacons\n" ++
    (maybe [] (genTexCons docopts tcons vs) cons) ++
    "\n\\currydatainsts\n" ++
    concatMap (genTexInst docopts tcmod) insts ++
    "\\currydatastop\n"
  CurryDocTypeDecl (tcmod,tcons) vs ty cs ->
    "\\currytypesynstart{" ++ tcons ++ "}{" ++
    (if tcons=="String" && tcmod=="Prelude"
     then "String = [Char]"
     else tcons ++ " " ++ unwords (map snd vs) ++ "="
           ++ showTexType docopts tcmod False ty) ++ "}\n" ++
    htmlString2Tex docopts
       (concatCommentStrings (map commentString cs)) ++
    "\\currytypesynstop\n\n"
  _ -> ""

genTexInst :: DocOptions -> String -> CurryDocInstanceDecl -> String
genTexInst docopts modname d = case d of
  CurryDocInstanceDecl (cmod, cname) cx ty _ _ ->
    "\n" ++
    (if null cxString then "" else cxString ++ " ") ++
    "\\textbf{" ++ cname ++ "} " ++
    showTexType docopts modname (isApplyType ty || isFunctionType ty) ty ++ "\n\n"
    where cxString = showTexContext docopts cmod cx

--- generate Tex documentation for a constructor if it is exported:
genTexCons :: DocOptions -> String -> [CTVarIName] -> CurryDocCons -> String
genTexCons docopts vs ds (CurryDocConsOp (cmod, cname) ty1 ty2 ai cs) =
  genTexCons docopts vs ds (CurryDocConstr (cmod, "(" ++ cname ++ ")")
                             [ty1, ty2] ai cs) -- TODO: maybe different?
genTexCons docopts tcons vs (CurryDocConstr (cmod, cname) tys _ cs) =
 "\\curryconsstart{" ++ cname ++ "}{" ++
      concatMap (\t-> showTexType docopts cmod True t ++ " $\\to$ ") tys ++
      tcons ++ " " ++ unwords (map snd vs) ++ "}\n" ++
      htmlString2Tex docopts (unwords (map commentString cs))
genTexCons docopts tcons vs (CurryDocRecord (cmod,cname) tys fs _ cs) =
 "\\curryconsstart{" ++ cname ++ "}{" ++
      concatMap (\t-> showTexType docopts cmod True t ++ " $\\to$ ") tys ++
      tcons ++ " " ++ unwords (map snd vs) ++ "}\n" ++
      htmlString2Tex docopts (unwords (map commentString cs)) ++
      intercalate "\n" (map (genTexField docopts) fs)

-- generate Tex documentation for record fields
genTexField :: DocOptions -> CurryDocField -> String
genTexField docopts (CurryDocField (fmod,fname) ty _ cs) =
  "\\curryfieldstart{" ++ fname ++ "}{" ++
    fname ++ " :: " ++ showTexType docopts fmod False ty ++ "}" ++
    if null txt then [] else
      " : " ++ htmlString2Tex docopts txt
  where txt = unwords (map commentString cs)

-- generate Tex documentation for a function:
genTexClass :: DocOptions -> CurryDocDecl -> String
genTexClass docopts d = case d of
  CurryDocClassDecl (cmod, cname) cx v ds cs
    -> "\\curryclassstart{" ++ cname ++ " " ++
       (if null cxString then "" else cxString ++ " ") ++
       snd v ++ "}{" ++
       htmlString2Tex docopts
         (concatCommentStrings (map commentString cs)) ++ "\n" ++
       concatMap (genTexFunc docopts) ds ++
       "\\curryclassstop\n"
    where cxString = showTexContext docopts cmod cx
  _ -> ""

--- generate Tex documentation for a module:
genTexModule :: DocOptions -> ModuleHeader -> String
genTexModule docopts (ModuleHeader fields maincmt) =
  htmlString2Tex docopts maincmt ++
  concatMap fieldTex fields
  where fieldTex (typ, value) =
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
  CTVar (_,n) -> n
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
