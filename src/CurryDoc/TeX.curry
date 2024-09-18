----------------------------------------------------------------------
--- Functions to generate documentation in TeX format.
---
--- @author Michael Hanus
--- @version March 2021
----------------------------------------------------------------------

module CurryDoc.TeX where

import Data.Char
import Data.List
import Data.Maybe

import FlatCurry.Types
import FlatCurry.Files
import FlatCurry.Show   ( isClassContext )
import HTML.Base
import HTML.LaTeX       ( showLatexExps )
import HTML.Parser
import Text.Markdown

import CurryDoc.AnaInfo
import CurryDoc.Options
import CurryDoc.Read

--------------------------------------------------------------------------
-- Generates the documentation of a module in HTML format where the comments
-- are already analyzed.
generateTexDocs :: DocOptions -> AnaInfo -> String -> String
                -> [(SourceLine,String)] -> IO String
generateTexDocs docopts anainfo modname modcmts progcmts = do
  fcyname <- getFlatCurryFileInLoadPath modname
  putStrLn $ "Reading FlatCurry program \""++fcyname++"\"..."
  (Prog _ _ types functions _) <- readFlatCurryFile fcyname
  let textypes = concatMap (genTexType docopts progcmts) types
      texfuncs = concatMap (genTexFunc docopts progcmts anainfo) functions
      modcmt   = fst (splitComment modcmts)
  return $
     "\\currymodule{"++modname++"}\n" ++
     htmlString2Tex docopts modcmt ++ "\n" ++
     (if null textypes then ""
      else "\\currytypesstart\n" ++ textypes ++ "\\currytypesstop\n") ++
     (if null texfuncs then ""
      else "\\curryfuncstart\n" ++ texfuncs ++ "\\curryfuncstop\n")

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
  []             -> []
  ('\\':'\'':cs) -> '\'' : replaceIdLinks cs
  (c:cs)         -> if c=='\'' then tryReplaceIdLink [] cs
                               else c : replaceIdLinks cs
 where
  tryReplaceIdLink ltxt [] = '\'' : reverse ltxt
  tryReplaceIdLink ltxt (c:cs)
   | isSpace c = '\'' : reverse ltxt ++ c : replaceIdLinks cs -- no space in id
   | c == '\'' = checkId (reverse ltxt) ++ replaceIdLinks cs
   | otherwise = tryReplaceIdLink (c:ltxt) cs

  checkId s = if ' ' `elem` s
                then '\'' : s ++ ['\'']
                else "`" ++ s ++ "`"

-- generate short HTML documentation for a function if it is exported
-- and not an internal operation to implement type classes:
genTexFunc :: DocOptions -> [(SourceLine,String)] -> _ -> FuncDecl -> String
genTexFunc docopts progcmts _ (Func (_,fname) _ fvis ftype _) =
  if fvis==Public && not (classOperations fname)
  then "\\curryfunctionstart{" ++ string2tex fname ++ "}{" ++
       "\\curryfuncsig{" ++ string2tex (showId fname) ++ "}{" ++
         showTexType False (stripForall ftype) ++ "}}\n" ++
         htmlString2Tex docopts
               (fst (splitComment (getFuncComment fname progcmts))) ++
       "\\curryfunctionstop\n"
  else ""
 where
  -- strip initial forall type quantifiers:
  stripForall texp = case texp of
    ForallType _ te -> te
    _               -> texp

  classOperations fn = take 6 fn `elem` ["_impl#","_inst#"]
                    || take 5 fn == "_def#" || take 7 fn == "_super#"

--- generate TeX documentation for a datatype if it is exported and
--- not a dictionary:
genTexType :: DocOptions -> [(SourceLine,String)] -> TypeDecl -> String
genTexType docopts progcmts (Type (_,tcons) tvis tvars constrs)
  | tvis==Public && not (isDict tcons)
  = let (datacmt,conscmts) = splitComment (getDataComment tcons progcmts)
    in "\\currydatastart{" ++ tcons ++ "}\n" ++
       htmlString2Tex docopts datacmt ++
       "\n\\currydatacons\n" ++
       concatMap (genHtmlCons docopts (getCommentType "cons" conscmts)
                              tcons tvars) constrs ++
       "\\currydatastop\n"
  | otherwise = ""
 where
  isDict fn = take 6 fn == "_Dict#"

genTexType docopts progcmts (TypeSyn (tcmod,tcons) tvis tvars texp)
  | tvis==Public
  = let (typecmt,_) = splitComment (getDataComment tcons progcmts)
    in "\\currytypesynstart{" ++ tcons ++ "}{" ++
       (if tcons=="String" && tcmod=="Prelude"
        then "String = [Char]"
        else tcons ++ concatMap (\(i,_) -> [' ', chr (97 + i)]) tvars ++
             " = " ++ showTexType False texp ) ++ "}\n" ++
       htmlString2Tex docopts typecmt ++ "\\currytypesynstop\n\n"
  | otherwise = ""

genTexType docopts progcmts
           (TypeNew (_,tcons) tvis tvars (NewCons cn cvis cte))
  | tvis==Public
  = let (datacmt,conscmts) = splitComment (getDataComment tcons progcmts)
    in "\\currynewtypestart{" ++ tcons ++ "}\n" ++
       htmlString2Tex docopts datacmt ++
       "\n\\currydatacons\n" ++
       genHtmlCons docopts (getCommentType "cons" conscmts) tcons tvars
                   (Cons cn 1 cvis [cte]) ++
       "\\currydatastop\n"
  | otherwise = ""

genHtmlCons :: DocOptions -> [String] -> String -> [(Int, a)] -> ConsDecl
            -> String
genHtmlCons docopts conscmts tcons tvars (Cons (_,cname) _ cvis argtypes) =
  if cvis==Public
  then "\\curryconsstart{" ++ cname ++ "}{" ++
       concatMap (\t->showTexType True t++" $\\to$ ") argtypes ++
                 tcons ++ concatMap (\(i,_) -> [' ', chr (97 + i)]) tvars ++
       "}\n" ++
       (maybe ""
              (\ (call,cmt) -> "{\\tt " ++ call ++ "}" ++
                               htmlString2Tex docopts cmt)
              (getConsComment conscmts cname))
       ++ "\n"
  else ""


-- Pretty printer for types in Curry syntax as TeX string.
-- first argument is True iff brackets must be written around complex types
showTexType :: Bool -> TypeExpr -> String
showTexType _ (TVar i) = [chr (97+i)]
showTexType nested (FuncType t1 t2) = brackets nested $
  maybe (showTexType (isFunctionType t1) t1 ++ " $\\to$ " ++
         showTexType False t2)
        (\ (cn,cts) -> unwords (cn : map (showTexType True) cts) ++
                       " $\\Rightarrow$ " ++ showTexType False t2)
        (isClassContext t1)
showTexType nested (TCons tc ts)
 | ts==[]  = snd tc
 | tc==("Prelude","[]") && (head ts == TCons ("Prelude","Char") [])
   = "String"
 | tc==("Prelude","[]")
   = "[" ++ showTexType False (head ts) ++ "]" -- list type
 | take 2 (snd tc) == "(,"                     -- tuple type
   = "(" ++ concat (intersperse "," (map (showTexType False) ts)) ++ ")"
 | otherwise
   = brackets nested
      (snd tc ++ " " ++ concat (intersperse " " (map (showTexType True) ts)))
showTexType nested (ForallType tvs te)
 | null tvs  = showTexType nested te
 | otherwise = brackets nested
                 (unwords ("forall" : map (showTexType False . TVar . fst) tvs)
                  ++ "." ++ showTexType False te)

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
