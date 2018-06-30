----------------------------------------------------------------------
--- Operations to generate documentation in HTML format.
---
--- @author Michael Hanus, Jan Tikovsky
--- @version December 2017
----------------------------------------------------------------------

module CurryDoc.Generators.Html
  (generateHtmlDocs,
   genMainIndexPage, genFunctionIndexPage, genConsIndexPage, genSystemLibsPage,
   genClassesIndexPage,
   translateSource2ColoredHtml)
   where

import FilePath
import FileGoodies     (getFileInPath)
import List
import Char
import Sort
import Time
import Distribution
import Markdown
import Maybe

import AbstractCurry.Types
import AbstractCurry.Files
import AbstractCurry.Select
import AbstractCurry.Build
import AbstractCurry.Pretty
import Analysis.TotallyDefined(Completeness(..))
import HTML.Base
import HTML.Styles.Bootstrap3 (bootstrapPage, glyphicon, homeIcon)
import HTML.CategorizedList
import Text.Pretty            (showWidth, empty)

import CurryDoc.Data.AnaInfo
import CurryDoc.Info
import CurryDoc.Options
import CurryDoc.Config

infixl 0 `withTitle`

--------------------------------------------------------------------------
--- Generates the documentation of a module in HTML format where the comments
--- are already analyzed.
generateHtmlDocs :: DocOptions -> CurryDoc -> IO String
generateHtmlDocs opts (CurryDoc mname mhead insts typesigs decls ex im) = do
  let
    (exptypes, expcons, expfields, expfuncs, expclasses) = ex
    navigation =
      [ bold [htxt "Exported names:"]
      , genHtmlExportIndex exptypes expcons expfields expfuncs expclasses
      , anchored "imported_modules" [bold [htxt "Imported modules:"]]
      , ulist (map (\i -> [href (docURL opts i ++ ".html") [htxt i]]) im)
        `addClass` "nav nav-sidebar"
      ]
    content =
      genHtmlModule opts mhead ++
      (if null exptypes then [] else
        [anchoredSection "exported_datatypes"
          ((h2 [htxt "Exported datatypes:"]) : hrule :
          concatMap (genHtmlType opts insts) decls)]) ++
      (if null expfuncs then [] else
        [anchoredSection "exported_operations"
          ((h2 [htxt "Exported operations:"]) : hrule :
          concatMap (mkFunctionBorder . genHtmlFunc opts typesigs) decls)]) ++
      (if null expclasses then [] else
        [anchoredSection "exported_classes"
          ((h2 [htxt "Exported Typeclasses:"]) : hrule :
          concatMap (genHtmlClass opts) decls)])

    leftTopMenu = lefttopmenu (not $ null exptypes)
                              (not $ null expfuncs)
                              (not $ null expclasses)
  mainPage title [htmltitle] leftTopMenu rightTopMenu navigation content
 where
  title = "Module " ++ mname

  htmltitle = h1 [ htxt "Module "
                 , href (mname ++ "_curry.html") [htxt mname]
                 ]

  mkFunctionBorder []        = []
  mkFunctionBorder fun@(_:_) = [borderedTable [[fun]]]

  lefttopmenu tb fb cb
    = [[href "?" [htxt title]], [href "#imported_modules" [htxt "Imports"]]]
   ++ if tb then [[href "#exported_datatypes"  [htxt "Datatypes"  ]]] else []
   ++ if fb then [[href "#exported_operations" [htxt "Operations" ]]] else []
   ++ if cb then [[href "#exported_classes"    [htxt "Typeclasses"]]] else []


--- Translate a documentation comment to HTML and use markdown translation
--- if necessary
--- @return: either a paragraph (`<p>`) element or an empty list.
docComment2HTML :: DocOptions -> String -> [HtmlExp]
docComment2HTML opts cmt
  | null cmt          = []
  | withMarkdown opts = markdownText2HTML (replaceIdLinks opts cmt)
  | otherwise         = [par [HtmlText (replaceIdLinks opts cmt)]]

-- replace identifier hyperlinks in a string (i.e., enclosed in single quotes)
-- by HTML hyperrefences:
replaceIdLinks :: DocOptions -> String -> String
replaceIdLinks opts str = case str of
  [] -> []
  ('\\':'\'':cs) -> '\'' : replaceIdLinks opts cs
  (c:cs) -> if c=='\'' then tryReplaceIdLink [] cs
                       else c : replaceIdLinks opts cs
 where
  tryReplaceIdLink ltxt [] = '\'' : reverse ltxt
  tryReplaceIdLink ltxt (c:cs)
   | isSpace c
   = '\'' : reverse ltxt ++ c : replaceIdLinks opts cs -- no space in id
   | c == '\''
   = checkId (reverse ltxt) ++ replaceIdLinks opts cs
   | otherwise
   = tryReplaceIdLink (c:ltxt) cs

  checkId s =
    if ' ' `elem` s
    then '\'' : s ++ ['\'']
    else let (md,dotfun) = break (=='.') s
          in "<code><a href=\"" ++
             (if null dotfun then '#':s
                             else docURL opts md ++ ".html#" ++ tail dotfun) ++
             "\">"++s++"</a></code>"

genHtmlExportIndex :: [QName] -> [QName] -> [QName] -> [QName] -> [QName]
                  -> HtmlExp
genHtmlExportIndex exptypes expcons expfields expfuns expcls =
  HtmlStruct "ul" [("class","nav nav-sidebar")]
    (concatMap (\ (htmlnames,cattitle) ->
                 if null htmlnames
                 then []
                 else HtmlStruct "li" [("class","nav-header")] [htxt cattitle] :
                      map (HtmlStruct "li" []) htmlnames)
            [(htmltypes,"Datatypes:"),
             (htmlcons ,"Constructors:"),
             (htmlfields,"Fields:"),
             (htmlfuns ,"Operations:"),
             (htmlcls ,"Typeclasses:")])
 where
  htmltypes  = map (\n->[href ('#':n++"_TYPE" ) [htxt n]])
                   (nub (sortStrings $ unquals exptypes))
  htmlcons   = map (\n->[href ('#':n++"_CONS" ) [htxt n]])
                   (nub (sortStrings $ unquals expcons))
  htmlfields = map (\n->[href ('#':n++"_FIELD") [htxt n]])
                  (nub (sortStrings $ unquals expfields))
  htmlfuns   = map (\n->[href ('#':n++"_FUNC" ) [htxt n]])
                   (nub (sortStrings $ unquals expfuns))
  htmlcls    = map (\n->[href ('#':n++"_CLS"  ) [htxt n]])
                   (nub (sortStrings $ unquals expcls))
  unquals = map snd

--- generate HTML documentation for a module:
genHtmlModule :: DocOptions -> ModuleHeader -> [HtmlExp]
genHtmlModule docopts (ModuleHeader fields maincmt) =
  docComment2HTML docopts maincmt ++
  map fieldHtml fields
  where fieldHtml (typ, value) =
          par [bold [htxt (show typ ++ ": ")], htxt value]

--- generate HTML documentation for a datatype if it is exported:
genHtmlType :: DocOptions -> [CommentedDecl] -> CommentedDecl -> [HtmlExp]
genHtmlType docopts inst d = case d of
  CommentedDataDecl n@(tmod,tcons) vs cs cns ->
       [anchored (tcons++"_TYPE") [style "typeheader" [htxt tcons]]]
    ++ docComment2HTML docopts (concatCommentStrings (map commentString cs))
    ++ [par [explainCat "Constructors: "]]
    ++ ulistOrEmpty (map (genHtmlCons docopts n vs) cns)
    ++ [par [explainCat "Known instances: "]]
    ++ ulistOrEmpty (map (genHtmlInst docopts tmod)
                         (filter ((n =~=) . instTypeName) inst))
    ++ [hrule]
  CommentedNewtypeDecl n@(tmod,tcons) vs cs cn ->
    [anchored (tcons++"_TYPE") [style "typeheader" [htxt tcons]]]
    ++ docComment2HTML docopts (concatCommentStrings (map commentString cs))
    ++ [par [explainCat "Constructors: "]]
    ++ genHtmlNewCons docopts n vs cn
    ++ [par [explainCat "Known instances: "]]
    ++ ulistOrEmpty (map (genHtmlInst docopts tmod)
                         (filter ((n =~=) . instTypeName) inst))
    ++ [hrule]
  CommentedTypeDecl (tmod,tcons) vs ty cs ->
       [anchored (tcons++"_TYPE") [style "typeheader" [htxt tcons]]]
    ++ docComment2HTML docopts (concatCommentStrings (map commentString cs))
    ++ [ par [explainCat "Type synonym:"
       , nbsp
       , code [HtmlText (tcons ++ " " ++ unwords (map snd vs) ++ " = " ++
            case (tmod,tcons) of
              ("Prelude", "String")
                -> "[" ++ showTypeCons docopts tmod (tmod,tcons) ++ "]"
              _ -> showType docopts tmod False ty)]]
       , hrule
       ]
  _ -> []

--- generate HTML documentation for a constructor if it is exported:
genHtmlCons :: DocOptions -> QName -> [CTVarIName] -> CommentedConstr
            -> [HtmlExp]
genHtmlCons docopts dn vs (CommentedConsOp cn cs ty1 ty2 ai) =
  genHtmlCons docopts dn vs (CommentedConstr cn cs [ty1, ty2] ai) -- TODO: maybe different?
genHtmlCons docopts (_, tcons) vs (CommentedConstr (cmod, cname) cs tys ai) =
  anchored (cname ++ "_CONS")
    [code [opnameDoc [htxt cname],
           HtmlText (" :: " ++
                    concatMap (\t -> " "++showType docopts cmod True t++" -> ")
                              tys ++
                    tcons ++ " " ++ unwords (map snd vs))]] :
    (if null txt then [] else removeTopPar (docComment2HTML docopts txt)) ++
    maybe []
      (\(fixity, prec) -> [par [htxt ("defined as " ++ showFixity fixity ++
                                      " infix operator with precedence " ++
                                      show prec)]]) fix
  where txt = unwords (map commentString cs)
        fix = case ai of
          NoAnalysisInfo -> Nothing
          _              -> precedence ai
genHtmlCons docopts (_, tcons) vs (CommentedRecord (cmod,cname) cs tys fs ai) =
  anchored (cname ++ "_CONS")
    [code [opnameDoc [htxt cname],
          HtmlText (" :: " ++
                    concatMap (\t -> " "++showType docopts cmod True t++" -> ")
                              tys ++
                    tcons ++ " " ++ unwords (map snd vs))]] :
    (if null txt then [] else removeTopPar (docComment2HTML docopts txt)) ++
    par [explainCat "Fields:"] :
    ulistOrEmpty (map (genHtmlField docopts) fs) ++
    maybe []
      (\p -> genPrecedenceText p) fix
  where txt = unwords (map commentString cs)
        fix = case ai of
          NoAnalysisInfo -> Nothing
          _              -> precedence ai

-- generate HTML documentation for record fields
genHtmlField :: DocOptions -> CommentedField -> [HtmlExp]
genHtmlField _       ([]            , _ , _ ) = []
genHtmlField _       ((_ : _ : _)   , _ , _ ) = []
genHtmlField docopts ([(fmod,fname)], cs, ty) =
  [anchored (fname ++ "_FIELD")
    ([ code [opnameDoc [htxt fname]]
     , HtmlText (" :: " ++ showType docopts fmod True ty)
     ] ++ if null txt then [] else
            htxt " : " : removeTopPar (docComment2HTML docopts txt))]
  where txt = unwords (map commentString cs)

genHtmlNewCons :: DocOptions -> QName -> [CTVarIName]
               -> Maybe CommentedNewtypeConstr -> [HtmlExp]
genHtmlNewCons _       _  _  Nothing = []
genHtmlNewCons docopts dn vs (Just (CommentedNewConstr cn cs ty ai)) =
  genHtmlCons docopts dn vs (CommentedConstr cn cs [ty] ai)
genHtmlNewCons docopts dn vs (Just (CommentedNewRecord cn cs ty f ai)) =
  genHtmlCons docopts dn vs (CommentedRecord cn cs [ty] fs ai)
  where fs = case f of
               Just f' -> [f']
               Nothing -> []

genHtmlInst :: DocOptions -> String -> CommentedDecl -> [HtmlExp]
genHtmlInst docopts dn d = case d of
  CommentedInstanceDecl (cmod, cname) cx ty _ _ ->
    [code [htxt (if null cxString then [] else cxString ++ " "),
           href (docURL docopts cmod++".html#"++cname++"_CLASS")
                 [htxt cname]]] ++
    [code [HtmlText (showType docopts dn
                       (isApplyType ty || isFunctionType ty) ty)]]
    where cxString = showContext docopts cmod cx
  _ -> []

-- generate HTML documentation for a function:
genHtmlClass :: DocOptions -> CommentedDecl -> [HtmlExp]
genHtmlClass docopts d = case d of
  CommentedClassDecl (cmod, cname) cx v cs ds ->
       [anchored (cname ++ "_CLASS")
         [(style "classheader"
            [code [(htxt "class "),
                htxt (if null cxString then [] else cxString ++ " "),
                classnameDoc [htxt cname],
                htxt (' ' : snd v)]])]]
    ++ docComment2HTML docopts (concatCommentStrings (map commentString cs))
    ++ [par [explainCat "Methods: "]]
    ++ [borderedTable (map ((:[]) . genHtmlFunc docopts typsigs) methods)]
    where cxString = showContext docopts cmod cx
          (typsigs, methods) = partition isCommentedTypeSig ds
  _ -> []

-- generate HTML documentation for a function:
genHtmlFunc :: DocOptions -> [CommentedDecl] -> CommentedDecl -> [HtmlExp]
genHtmlFunc docopts types d = case d of
  CommentedFunctionDecl (fmod,fname) cs (Just ty) ai ->
     [anchoredDiv (fname ++ "_FUNC")
      [par $
        [code [opnameDoc [showCodeHRef fname],
               HtmlText (" :: "++ showQualType docopts fmod ty)],
         nbsp, nbsp] ++
        genFuncPropIcons ai]] ++
        genSigComment docopts (lookupTypeSig (fmod, fname) types) ++
        docComment2HTML docopts (concatCommentStrings (map commentString cs)) ++
        genFurtherInfos (fmod, fname) ai
    where showCodeHRef fn = href (fmod++"_curry.html#"++fn) [htxt (showId fn)]
  _ -> []

genSigComment :: DocOptions -> Maybe CommentedDecl -> [HtmlExp]
genSigComment _       Nothing  = []
genSigComment docopts (Just d) = case d of
  CommentedTypeSig [(fmod, _)] cs cx ps ->
    [ par (docComment2HTML docopts (concatCommentStrings (map commentString cs)))
    , par [code [HtmlText (showContext docopts fmod cx)]]
    ] ++ [table (genParamComments docopts fmod ps)]
  _ -> []

genParamComments :: DocOptions -> String -> [(CTypeExpr, [Comment])] -> [[[HtmlExp]]]
genParamComments _       _    []         =
  error "Html.genParamComment: empty list"
genParamComments docopts fmod [(ty, cs)] =
  [[[code [HtmlText (showType docopts fmod False ty)]],
     [par $ docComment2HTML docopts (unwords (map commentString cs))]]]
genParamComments docopts fmod ((ty, cs) : xs@(_:_)) =
  [[code [HtmlText (showType docopts fmod False ty ++ " ->")]],
   [par $ docComment2HTML docopts (unwords (map commentString cs))]]
    : genParamComments docopts fmod xs

genFurtherInfos :: QName -> AnalysisInfo -> [HtmlExp]
genFurtherInfos _  NoAnalysisInfo            = []
genFurtherInfos _  (PrecedenceInfo Nothing)  = []
genFurtherInfos _  (PrecedenceInfo (Just p)) =
  [dlist [([explainCat "Further infos:"],
          genPrecedenceText p)]]
genFurtherInfos qn ai@(AnalysisInfo {}) =
  concatMap (showProperty qn) (property ai) ++
  [dlist [([explainCat "Further infos:"],
            (maybe [] (\p -> genPrecedenceText p) (precedence ai)
              ++ catMaybes
                  [completenessInfo,
                   indeterminismInfo,
                   opcompleteInfo,
                   externalInfo]))]]
  where
    completenessInfo = let ci = complete ai in
      if ci == Complete
       then Nothing
       else Just (htxt
         (if ci == InComplete
            then "partially defined"
            else
              "partially defined in each disjunction (but might be complete)"))

    -- comment about the indeterminism of a function:
    indeterminismInfo =
      if indet ai
        then Just (htxt "might behave indeterministically")
        else Nothing

    -- comment about the indeterminism of a function:
    opcompleteInfo =
       if opComplete ai
         then Just (htxt "solution complete, i.e., able to compute all solutions")
         else Nothing

    -- comment about the external definition of a function:
    externalInfo  =
      if ext ai
        then Just (htxt "externally defined")
        else Nothing

genPrecedenceText :: (CFixity, Int) -> [HtmlExp]
genPrecedenceText (fixity, prec) =
  [par [htxt ("defined as " ++ showFixity fixity ++
              " infix operator with precedence " ++
              show prec)]]

showProperty :: QName -> (Property, CRule) -> [HtmlExp]
showProperty qn (sp, rule) = case (sp, rule) of
  (PreSpec, CRule _ (CSimpleRhs _ _)) ->
     let (lhs,rhs) = break (=='=') prettyRule
     in [code [htxt $ "(" ++ trimSpace lhs ++ ")"],
         italic [htxt " requires "],
         code [htxt (safeTail rhs)]]
  (PreSpec, _) -> -- we don't put much effort to format complex preconditions:
    [code [htxt prettyRule]]
  (PostSpec, CRule ps (CSimpleRhs _ _)) ->
    let (_,rhs) = break (=='=') prettyRule
    in [code [htxt $ prettyWith ppCPattern (last ps) ++ " = " ++
                     prettyWith ppCPattern
                                 (CPComb qn (take (length ps - 1) ps)) ],
        italic [htxt " satisfies "],
        code [htxt (safeTail rhs)]]
  (PostSpec, _) -> -- we don't put much effort to format complex postconditions:
    [code [htxt prettyRule]]
  (Spec, CRule _ (CSimpleRhs _ _)) ->
    let (lhs,rhs) = break (=='=') prettyRule
    in [code [htxt $ "(" ++ trimSpace lhs ++ ")"],
        italic [htxt " is equivalent to "],
        code [htxt (safeTail rhs)]]
  (Spec, _) -> -- we don't put much effort to format complex specifications:
    [code [htxt prettyRule]]
  (Prop, _) ->
    [code [htxt $ prettyWith (ppCRhs empty) (ruleRHS rule)]]
 where
   safeTail xs      = if null xs then xs else tail xs
   prettyRule       = showWidth 78 (ppCRule prettyOpts qn rule)
   prettyWith ppfun = showWidth 78 . ppfun prettyOpts
   prettyOpts       = setNoQualification defaultOptions

--- Generates icons for particular properties of functions.
genFuncPropIcons :: AnalysisInfo -> [HtmlExp]
genFuncPropIcons    NoAnalysisInfo    = []
genFuncPropIcons    PrecedenceInfo {} = []
genFuncPropIcons ai@AnalysisInfo   {} =
   [detPropIcon, nbsp]
 where
   --(non)deterministically defined property:
   detPropIcon =
    if nondet ai
    then href "index.html#nondet_explain" [nondetIcon]
    else href "index.html#det_explain"    [detIcon]

--------------------------------------------------------------------------
-- Pretty printer for qualified types in Curry syntax:
showQualType :: DocOptions -> String -> CQualTypeExpr -> String
showQualType opts mod (CQualType ctxt texp) =
  unwords [showContext opts mod ctxt, showType opts mod False texp]

showContext :: DocOptions -> String -> CContext -> String
showContext _ _ (CContext []) = ""
showContext opts mod (CContext [clscon]) =
  showConstraint opts mod clscon ++ " =>"
showContext opts mod (CContext ctxt@(_:_:_)) =
  bracketsIf True (intercalate ", " (map (showConstraint opts mod) ctxt)) ++ " =>"

--- Pretty-print a single class constraint.
showConstraint :: DocOptions -> String -> CConstraint -> String
showConstraint opts mod (cn,texp) =
  showTypeCons opts mod cn ++ " " ++ showType opts mod True texp

-- Pretty printer for type expressions in Curry syntax:
-- second argument is True iff brackets must be written around complex types
showType :: DocOptions -> String -> Bool -> CTypeExpr -> String
showType opts mod nested texp = case texp of
  CTVar (_,n) -> n -- TODO: use name given in source program instead?
  CFuncType t1 t2 ->
    bracketsIf nested (showType opts mod (isFunctionalType t1) t1++" -&gt; "++
                     showType opts mod False t2)
  CTCons tc -> showTConsType opts mod nested tc []
  CTApply t1 t2 ->
       maybe (bracketsIf nested $
                showType opts mod True t1 ++ " " ++ showType opts mod True t2)
             (\ (tc,ts) -> showTConsType opts mod nested tc ts)
             (tconsArgsOfType texp)

showTConsType :: DocOptions -> String -> Bool -> QName -> [CTypeExpr] -> String
showTConsType opts mod nested tc ts
 | ts==[]  = showTypeCons opts mod tc
 | tc==("Prelude","[]") && (head ts == CTCons ("Prelude","Char"))
   = "String"
 | tc==("Prelude","[]")
   = "[" ++ showType opts mod False (head ts) ++ "]" -- list type
 | take 2 (snd tc) == "(,"                      -- tuple type
   = "(" ++ concat (intersperse "," (map (showType opts mod False) ts)) ++ ")"
 | otherwise
   = bracketsIf nested
      (showTypeCons opts mod tc ++ " " ++
       concat (intersperse " " (map (showType opts mod True) ts)))

showTypeCons :: DocOptions -> String -> QName -> String
showTypeCons opts mod (mtc,tc) =
  if mtc == "Prelude"
  then tc --"<a href=\"Prelude.html#"++tc++"\">"++tc++"</a>"
  else
    if mod == mtc
    then "<a href=\"#"++tc++"\">"++tc++"</a>"
    else "<a href=\""++docURL opts mtc++".html#"++tc++"\">"++tc++"</a>"

showFixity :: CFixity -> String
showFixity CInfixOp  = "non-associative"
showFixity CInfixlOp = "left-associative"
showFixity CInfixrOp = "right-associative"

--------------------------------------------------------------------------
-- translate source file into HTML file with syntax coloring
translateSource2ColoredHtml :: String -> String -> IO ()
translateSource2ColoredHtml docdir modname = do
    let output = docdir </> modname++"_curry.html"
    putStrLn ("Writing source file as HTML to \""++output++"\"...")
    callFrontendWithParams HTML
      (setQuiet True (setHtmlDir docdir defaultParams)) modname

-- translate source file into HTML file with anchors for each function:
translateSource2AnchoredHtml :: String -> String -> IO ()
translateSource2AnchoredHtml docdir modname =
 do putStrLn ("Writing source file as HTML to \""++docdir++"/"++modname++"_curry.html\"...")
    prog <- readFile (modname++".curry")
    writeFile (docdir </> modname++"_curry.html")
              (showPageWithDocStyle (modname++".curry")
                  [HtmlStruct "pre" []
                     [HtmlText (addFuncAnchors [] (lines prog))]])

-- add the anchors to the classified lines and translate back:
-- first argument: list of already added anchors
-- second argument: list of source lines
addFuncAnchors :: [String] -> [String] -> String
addFuncAnchors _ [] = ""
addFuncAnchors ancs (sl : sls) = let id1 = getFirstId sl in
  if id1=="" ||
     id1 `elem` ["data","type","import","module","infix","infixl","infixr"]
  then htmlQuote (sl++"\n") ++ addFuncAnchors ancs sls
  else if id1 `elem` ancs
       then (sl++"\n") ++ addFuncAnchors ancs sls
       else "<a name=\""++id1++"\"></a>"
            ++ htmlQuote (sl++"\n")
            ++ addFuncAnchors (id1:ancs) sls


--------------------------------------------------------------------------
-- generate the index page for the documentation directory:
genMainIndexPage :: DocOptions -> String -> [String] -> IO ()
genMainIndexPage docopts docdir modnames =
 do putStrLn ("Writing index page to \""++docdir++"/index.html\"...")
    simplePage "Documentation of Curry modules"
               (Just pageTitle)
               allConsFuncsClassesMenu (indexPage modnames)
     >>= writeFile (docdir++"/index.html")
 where
  pageTitle = if not (null (mainTitle docopts))
                then [htxt (mainTitle docopts)]
                else if length modnames == 1
                      then [htxt "Documentation of the Curry program ",
                            href (head modnames ++ ".html")
                                 [htxt (head modnames)]]
                      else [htxt "Documentation of Curry programs"]

allConsFuncsClassesMenu :: [[HtmlExp]]
allConsFuncsClassesMenu =
  [[href "findex.html"   [htxt "All operations"]],
   [href "cindex.html"   [htxt "All constructors"]],
   [href "clsindex.html" [htxt "All typeclasses"]]]

indexPage :: [String] -> [HtmlExp]
indexPage modnames =
  (if null modnames
   then []
   else [h2 [htxt "Modules:"],
         ulist (map (\m->[href (m++".html") [htxt m]])
                    (mergeSortBy leqStringIgnoreCase modnames))])
  ++ [explainIcons]

-- Paragraph to explain the meaning of the icons:
explainIcons :: HtmlExp
explainIcons =
  anchoredSection "explain_icons"
    [h2 [htxt "Explanations of the icons used in the documentation:"],
     table
       [[[anchor "det_explain" [detIcon]],[nbsp],
         [htxt " Operation is deterministic, i.e., defined by exclusive rules",
          htxt " and depend only on deterministic operations"]]
       ,[[anchor "nondet_explain" [nondetIcon]],[nbsp],
         [htxt " Operation might be non-deterministic, i.e., it is defined by",
          htxt " overlapping rules or depend on non-deterministic operations"]]
--        ,[[anchor "rigid_explain" [rigidIcon]],[nbsp],
--          [htxt " Operation is rigid"]]
--        ,[[anchor "flex_explain" [flexibleIcon]],[nbsp],
--          [htxt " Operation is flexible"]]
--        ,[[anchor "flexrigid_explain" [flexrigidIcon]],[nbsp],
--          [htxt " Operation is partially flexible and partially rigid"]]
       ]
    ]

--------------------------------------------------------------------------
-- generate the function index page for the documentation directory:
genFunctionIndexPage :: DocOptions -> String -> [CFuncDecl] -> IO ()
genFunctionIndexPage opts docdir funs = do
  putStrLn ("Writing operation index page to \""++docdir++"/findex.html\"...")
  simplePage "Index to all operations" Nothing allConsFuncsClassesMenu
             (htmlIndex opts (sortNames expfuns))
    >>= writeFile (docdir++"/findex.html")
 where
   expfuns = map funcName $ filter ((== Public) . funcVis) funs

htmlIndex :: DocOptions -> [QName] -> [HtmlExp]
htmlIndex opts = categorizeByItemKey . map (showModNameRef opts)

showModNameRef :: DocOptions -> QName -> (String,[HtmlExp])
showModNameRef opts (modname,name) =
  (name,
   [href (docURL opts modname ++ ".html#"++name) [htxt name], nbsp, nbsp,
    htxt "(", href (docURL opts modname ++ ".html") [htxt modname], htxt ")"]
  )

sortNames :: [(a,String)] -> [(a,String)]
sortNames names = mergeSortBy (\(_,n1) (_,n2)->leqStringIgnoreCase n1 n2) names


--------------------------------------------------------------------------
-- generate the constructor index page for the documentation directory:
genConsIndexPage :: DocOptions ->  String -> [CTypeDecl] -> IO ()
genConsIndexPage opts docdir types = do
  putStrLn ("Writing constructor index page to \""++docdir++"/cindex.html\"...")
  simplePage "Index to all constructors" Nothing allConsFuncsClassesMenu
             (htmlIndex opts (sortNames expcons))
    >>= writeFile (docdir++"/cindex.html")
 where
   consDecls (CType    _ _ _ cs _) = cs
   consDecls (CNewType _ _ _ c  _) = [c]
   consDecls (CTypeSyn _ _ _ _   ) = []
   expcons = map consName $ filter ((== Public) . consVis) $
     concatMap consDecls types

--------------------------------------------------------------------------
-- generate the typeclasses index page for the documentation directory:
genClassesIndexPage :: DocOptions ->  String -> [CClassDecl] -> IO ()
genClassesIndexPage opts docdir cls = do
  putStrLn ("Writing typeclasses index page to \""++docdir++"/clsindex.html\"...")
  simplePage "Index to all typeclasses" Nothing allConsFuncsClassesMenu
             (htmlIndex opts (sortNames expclasses))
    >>= writeFile (docdir++"/clsindex.html")
 where
   expclasses = map    (\(CClass n _   _ _ _) -> n) $
                filter (\(CClass _ vis _ _ _) -> vis == Public) cls

--------------------------------------------------------------------------
-- generate the index page categorizing all system libraries of PAKCS/KICS2
genSystemLibsPage :: String -> [String] -> [[(String, ModuleHeader)]] -> IO ()
genSystemLibsPage docdir cats modInfos = do
  putStrLn $ "Writing main index page for " ++ currySystem ++
             " to \"" ++ fname ++ "\"..."
  mainPage (currySystem ++ " Libraries")
           [h1 [htxt $ currySystem ++ ": System Libraries"]]
           syslibsLeftTopMenu
           syslibsRightTopMenu
           (syslibsSideMenu cats)
           ([infoTxt, hrule] ++ genHtmlLibCats modInfos ++ [hrule, explainIcons])
   >>= writeFile fname
 where
  fname = docdir ++ "/" ++ currySystem ++ "_libs.html"

syslibsLeftTopMenu :: [[HtmlExp]]
syslibsLeftTopMenu =
  [ [href (currySystemURL ++ "/Manual.pdf") [htxt "Manual (PDF)"]]
  , [href (currySystemURL ++ "/lib/") [htxt "Libraries"]]
  , [ehref currygleURL [extLinkIcon, htxt " API Search"]]
  , [href (currySystemURL ++ "/download.html") [htxt "Download"]]
  ]

syslibsRightTopMenu :: [[HtmlExp]]
syslibsRightTopMenu =
  [ curryHomeItem
  , [ehref (curryHomeURL ++ "/documentation/report")
           [extLinkIcon, htxt " Curry Report"]]
  ]

syslibsSideMenu :: [String] -> [HtmlExp]
syslibsSideMenu cats = map par $
     [[ehref currygleURL [extLinkIcon, htxt " Search with Curr(y)gle"]]]
  ++ [[href ("#" ++ c) [ htxt (genCatExplain c)]] | c <- cats]
  ++ [ [href "findex.html" [htxt "Index to all library functions"]]
     , [href "cindex.html" [htxt "Index to all library constructors"]]
     , [href "#explain_icons" [htxt "Icons used in the documentation"]]
     ]

infoTxt :: HtmlExp
infoTxt = par
  [ htxt "Here is the collection of libraries contained in the distribution of "
  , href currySystemURL [htxt currySystem]
  , htxt $ ". Most of these libraries have been implemented during the "
        ++ "development of larger Curry applications. If you have suggestions "
        ++ "for changes/improvements or if you want to contribute your own "
        ++ "library, please contact "
  , href "http://www.informatik.uni-kiel.de/~mh/" [htxt "Michael Hanus"]
  , htxt "."
  ]

genCatExplain :: String -> String
genCatExplain c = case c of
  "general"   -> "General libraries"
  "algorithm" -> "Data structures and algorithms"
  "database"  -> "Database access and manipulation"
  "web"       -> "Libraries for web applications"
  "meta"      -> "Libraries for meta-programming"
  _           -> "Other Libraries"

genHtmlLibCats :: [[(String, ModuleHeader)]] -> [HtmlExp]
genHtmlLibCats = concatMap gen
  where
    gen [] = []
    gen mods@((_, (ModuleHeader xs _)):_) =
      let c = getCategoryWithDefault "general" xs
      in [anchoredSection (getCategoryWithDefault "general" xs)
            (h2 [htxt (genCatExplain c ++ ":")] : genHtmlLibCat mods)]


genHtmlLibCat :: [(String, ModuleHeader)] -> [HtmlExp]
genHtmlLibCat mods =
  [dlist [(genHtmlName modname, docComment2HTML defaultCurryDocOptions modcmt)
  | (modname, ModuleHeader _ modcmt) <- mods ]
  ]
 where
  genHtmlName modname = [code [href (modname ++ ".html") [htxt modname]]]

--------------------------------------------------------------------------
-- Auxiliary operation for general page style.

--- Generate the main page with the default documentation style.
--- @param title - the title of the page
--- @param htmltitle - the title in HTML format (shown as h1)
--- @param lefttopmenu - the menu shown at left of the top
--- @param righttopmenu - the menu shown at right of the top
--- @param sidemenu - the menu shown at the left-hand side
--- @param maindoc - the main contents of the page
mainPage :: String -> [HtmlExp] -> [[HtmlExp]] -> [[HtmlExp]]
         -> [HtmlExp] -> [HtmlExp] -> IO String
mainPage title htmltitle lefttopmenu righttopmenu sidemenu maindoc = do
    time <- getLocalTime
    return $ showHtmlPage $
      bootstrapPage styleBaseURL cssIncludes title homeBrand
                    lefttopmenu righttopmenu 3 sidemenu htmltitle maindoc
                    (curryDocFooter time)

cssIncludes :: [String]
cssIncludes = ["bootstrap.min","currydoc"]

homeBrand :: (String,[HtmlExp])
homeBrand = (currySystemURL, [homeIcon, nbsp, htxt currySystem])

--- Generate a page with the default documentation style.
--- @param title - the title of the page
--- @param body  - the main contents of the page
showPageWithDocStyle :: String -> [HtmlExp] -> String
showPageWithDocStyle title body =
  showHtmlPage $
    HtmlPage title
             (map (\f -> pageCSS $ styleBaseURL++"/css/"++f++".css") cssIncludes)
             body

--- The standard right top menu.
rightTopMenu :: [[HtmlExp]]
rightTopMenu =
  [ curryHomeItem
  , [ehref (currySystemURL++"/lib/")
           [extLinkIcon, htxt $ " "++currySystem++" Libraries"]]
  , [ehref (curryHomeURL ++ "/tools/currydoc")
           [extLinkIcon, htxt " About CurryDoc"]]
  ]

--------------------------------------------------------------------------
-- Icons:

extLinkIcon :: HtmlExp
extLinkIcon = glyphicon "new-window"

detIcon :: HtmlExp
detIcon     = glyphicon "arrow-right"
                `withTitle` "This operation is deterministic"
nondetIcon :: HtmlExp
nondetIcon  = glyphicon "random"
                `withTitle` "This operation might be non-deterministic"
-- rigidIcon :: HtmlExp
-- rigidIcon     = italic [] `addClass` "fa fa-cogs"
--                   `withTitle` "This operation is rigid"
-- flexibleIcon :: HtmlExp
-- flexibleIcon  = italic [] `addClass` "fa fa-pagelines"
--                   `withTitle` "This operation is flexible"
-- flexrigidIcon :: HtmlExp
-- flexrigidIcon = italic [] `addClass` "fa fa-exclamation-triangle"
--     `withTitle` "This operation is partially flexible and partially rigid"

withTitle :: HtmlExp -> String -> HtmlExp
withTitle he t = he `addAttr` ("title",t)

--------------------------------------------------------------------------
-- Standard footer information for generated web pages:
curryDocFooter :: CalendarTime -> [HtmlExp]
curryDocFooter time =
  [italic [htxt "Generated by ",
           bold [htxt "CurryDoc"],
           htxt (" ("++currydocVersion++") at "),
           htxt (calendarTimeToString time)]]

curryHomeItem :: [HtmlExp]
curryHomeItem = [ehref curryHomeURL [extLinkIcon, htxt " Curry Homepage"]]

--- Generate a simple page with the default documentation style.
--- @param title - the title of the page
--- @param htmltitle - maybe a specific title for h1 header
--- @param lefttopmenu - the menu shown at left of the top
--- @param doc - the main contents of the page
simplePage :: String -> Maybe [HtmlExp] -> [[HtmlExp]] -> [HtmlExp] -> IO String
simplePage title htmltitle lefttopmenu maindoc = do
    time <- getLocalTime
    return $ showHtmlPage $
      bootstrapPage styleBaseURL cssIncludes title homeBrand lefttopmenu rightTopMenu 0 []
                    [h1 (maybe [htxt title] id htmltitle)]
                    maindoc
                    (curryDocFooter time)

--- An anchored section in the document:
anchoredSection :: String -> [HtmlExp] -> HtmlExp
anchoredSection tag doc = section doc `addAttr` ("id",tag)

--- An anchored element in the document:
anchored :: String -> [HtmlExp] -> HtmlExp
anchored tag doc = style "anchored" doc `addAttr` ("id",tag)

--- An anchored element in the document:
anchoredDiv :: String -> [HtmlExp] -> HtmlExp
anchoredDiv tag doc = block doc `addAttr` ("id",tag)

--- A bordered table:
borderedTable :: [[[HtmlExp]]] -> HtmlExp
borderedTable rows = table rows `addClass` "table table-bordered table-hover"

--- An external reference
ehref :: String -> [HtmlExp] -> HtmlExp
ehref url desc = href url desc `addAttr` ("target","_blank")

--------------------------------------------------------------------------
-- auxiliaries:

lookupTypeSig :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupTypeSig _ []     = Nothing
lookupTypeSig n (d:ds) = case d of
  CommentedTypeSig [n'] _ _ _
    | n =~= n' -> Just d
  _            -> lookupTypeSig n ds

ulistOrEmpty :: [[HtmlExp]] -> [HtmlExp]
ulistOrEmpty items | null items = []
                   | otherwise  = [ulist items]

-- generate the html documentation for given comments ("param", "return",...)
ifNotNull :: [a] -> ([a] -> [b]) -> [b]
ifNotNull cmt genDoc
  | null cmt  = []
  | otherwise = genDoc cmt

-- style for explanation categories, like "Constructors:", "Parameters:",...
explainCat :: String -> HtmlExp
explainCat s = textstyle "explaincat" s

-- style for function/constructor name shown in the documentation part:
opnameDoc :: [HtmlExp] -> HtmlExp
opnameDoc = style "opname"

classnameDoc :: [HtmlExp] -> HtmlExp
classnameDoc = opnameDoc -- TODO

-- Sorts a list of strings.
sortStrings :: [String] -> [String]
sortStrings strings = mergeSortBy leqStringIgnoreCase strings

-- Returns the first sentence in a string:
firstSentence :: String -> String
firstSentence s = let (fs,ls) = break (=='.') s in
  if null ls
  then fs
  else if tail ls /= "" && isSpace (head (tail ls))
       then fs ++ "."
       else fs ++ "." ++ firstSentence (tail ls)

-- if first argument is True, put brackets around second argument:
bracketsIf :: Bool -> String -> String
bracketsIf False s = s
bracketsIf True  s = "("++s++")"

-- get the first identifier (name or operator in brackets) in a string:
getFirstId :: String -> String
getFirstId [] = ""
getFirstId (c:cs)
  | isAlpha c = takeWhile isIdChar (c:cs)
  | c == '('  = let bracketid = takeWhile (/=')') cs
                 in if all (`elem` infixIDs) bracketid
                    then bracketid
                    else ""
  | otherwise = ""

-- is an alphanumeric character, underscore, or apostroph?
isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_' || c == '\''

-- All characters occurring in infix operators.
infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

-- remove a single top-level paragraph in HTML expressions:
removeTopPar :: [HtmlExp] -> [HtmlExp]
removeTopPar hexps = case hexps of
  [HtmlStruct "p" [] hs] -> hs
  _ -> hexps

-- enclose a non-letter identifier in brackets:
showId :: String -> String
showId name = if isAlpha (head name) then name
                                     else ('(':name)++")"
