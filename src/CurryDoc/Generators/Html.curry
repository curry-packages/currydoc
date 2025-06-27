{- |
     Author  : Michael Hanus, Jan Tikovsky, Kai-Oliver Prott
     Version : May 2025

     Operations to generate documentation in HTML format.
-}
module CurryDoc.Generators.Html
  (generateHtmlDocs,
   genMainIndexPage, genFunctionIndexPage, genConsIndexPage, genSystemLibsPage,
   genClassesIndexPage,
   translateSource2ColoredHtml, replaceIdLinksMarkdown)
   where

import System.FrontendExec ( FrontendParams, FrontendTarget (..), defaultParams
                           , setQuiet, setHtmlDir, callFrontendWithParams )
import System.FilePath     ( (</>), (<.>) )
import System.Directory    ( getFileWithSuffix )
import Text.Pretty   ( showWidth, empty )
import Data.List     ( sortBy, last, intersperse, intercalate, nub )
import Data.Time     ( getLocalTime, calendarTimeToString, CalendarTime )
import Data.Char     ( isSpace, toUpper, toLower )
import Data.Maybe    ( catMaybes )
import Data.Function ( on )

import AbstractCurry.Types
import AbstractCurry.Files
import AbstractCurry.Select
import AbstractCurry.Build
import AbstractCurry.Pretty
import Analysis.TotallyDefined  ( Completeness(..) )
import Language.Curry.Resources ( currygleURL, baseLibsURL
                                , curryPackagesURL, curryHomeURL )
import Text.Markdown
import HTML.Base
import HTML.Styles.Bootstrap4
import HTML.CategorizedList

import CurryDoc.Data.AnaInfo
import CurryDoc.Data.CurryDoc
import CurryDoc.Info
import CurryDoc.Info.Export  ( flattenExport )
import CurryDoc.Info.Goodies ( snoc )
import CurryDoc.Options
import CurryDoc.Config

import Prelude hiding ( empty )

infixl 0 `addTitle`

-- | Generates the documentation of a module in HTML format.
generateHtmlDocs :: DocOptions -> CurryDoc -> IO String
generateHtmlDocs opts (CurryDoc mname mhead ex is) = do
  let
    moduleHeaderLink = block [jumpToTop $ htxt title]
    navigation =
      [ ulistWithClass "list-group" "list-group-item" $
          [moduleHeaderLink, genHtmlExportIndex ex] 
            : [importedModules | not (null imps)]
      ]
    content = [anchored "moduleheader" (genHtmlModule opts mhead ++ genExportEntityList opts ex)] ++ [hrule] ++
              snd (genHtmlForExport 0 opts ex)
  mainPage ("?", [htxt title]) title [htmltitle] [] rightTopMenu navigation content
   where
    imps = if mname == "Prelude"
             then is
             else nub $ "Prelude" : is
    title = "Module " ++ mname
    htmltitle = h1 [ htxt "Module "
                   , href (mname ++ "_curry.html") [htxt mname]
                   ]
    importedModules 
      = [anchored "imported_modules" [h5 [htxt "Imported modules:"]],
          ulistWithClass "nav flex-column" "nav-item"
                (map (\i -> [href (docURL opts i ++ ".html") [htxt i]])
                     imps)]

-- | Generates HTML for the given export structure.
--   The first parameter is used to assign a number to each
--   'CurryDoc.Info.Comments.ExportSection'.
--   Returns the generated HTML and the next number to be given to any
--   further 'CurryDoc.Info.Comments.ExportSection'.
genHtmlForExport :: Int -> DocOptions -> [ExportEntry CurryDocDecl]
                 -> (Int, [BaseHtml])
genHtmlForExport num _   []                                  = (num, [])
genHtmlForExport num doc (ExportSection c nesting ex : rest) =
  let (num' , innerHtml) = genHtmlForExport (num  + 1) doc ex
      (num'', outerHtml) = genHtmlForExport num'       doc rest
  in (num'', (anchoredSection ("g" ++ show num)
               ((hnest [htxt (commentString c)]) : hrule : innerHtml) 
               `addClass` "jump-offset")
              : outerHtml)
  where hnest = case nesting of
                  1 -> h2
                  2 -> h3
                  3 -> h4
                  _ -> h5
genHtmlForExport num doc (ExportEntryModule mtc : rest) =
  (num', (par [code [htxt "module ", href doclink [htxt mtc]]]
            `addClass` "moduleexport") : hrule : restHtml)
  where (num', restHtml) = genHtmlForExport num doc rest
        doclink = docURL doc mtc ++".html"
genHtmlForExport num doc (ExportEntry decl : rest)
  | isCurryDocFuncDecl  decl = (num', genHtmlFunc  "functionheader"
                                                   doc decl ++ [hrule]
                                                            ++ restHtml)
  | isCurryDocClassDecl decl = (num', genHtmlClass doc decl ++ [hrule]
                                                            ++ restHtml)
  | otherwise                = (num', genHtmlType  doc decl ++ [hrule]
                                                            ++ restHtml)
  where (num', restHtml) = genHtmlForExport num doc rest

-- | Translates a documentation comment to HTML
--   and uses markdown translation if necessary.
docComment2HTML :: DocOptions -> String
                -> [BaseHtml] -- ^ either a paragraph (`<p>`) element
                              --   or an empty list
docComment2HTML opts cmt
  | null cmt          = []
  | withMarkdown opts = markdownText2HTML (replaceIdLinksMarkdown opts cmt)
  | otherwise         = [par (replaceIdLinksHtml opts cmt)]

-- | Replaces identifier hyperlinks in a string (i.e., enclosed in single quotes)
--   by markdown hyperrefences.
replaceIdLinksMarkdown ::  DocOptions -> String -> String
replaceIdLinksMarkdown opts = replaceIdLinks idCon otherCon
  where idCon md fun = if null md
                         then "[" ++ fun ++ "](#" ++ fun ++ ")"
                         else "[" ++ fun ++ "](" ++
                              docURL opts md ++ ".html#" ++ fun ++ ")"
        otherCon = id

-- | Replaces identifier hyperlinks in a string (i.e., enclosed in single quotes)
--   by HTML hyperrefences.
replaceIdLinksHtml ::  DocOptions -> String -> [BaseHtml]
replaceIdLinksHtml opts = replaceIdLinks idCon otherCon
  where idCon md fun = if null md
                         then [hrefNav ('#':fun) [htxt fun]]
                         else [hrefNav (docURL opts md ++ ".html#" ++ fun)
                                       [htxt fun]]
        otherCon = (:[]) . htxt

-- TODO: not working correctly, may be ambigous
-- | Replaces identifier hyperlinks in a string (i.e., enclosed in single quotes)
--   according to the given function.
replaceIdLinks :: (String -> String -> [a]) ->
                  (String ->           [a]) ->
                  String -> [a]
replaceIdLinks idCon otherCon str = case str of
  [] -> []
  ('\\':'\'':cs) -> otherCon "'" ++ replaceIdLinks idCon otherCon cs
  (c:cs) -> if c=='\'' then tryReplaceIdLink [] cs
                       else otherCon (c:"") ++ replaceIdLinks idCon otherCon cs
 where
  tryReplaceIdLink ltxt [] = otherCon ('\'' : reverse ltxt)
  tryReplaceIdLink ltxt (c:cs)
   | isSpace c -- no space in id
   = otherCon ('\'' : reverse ltxt ++ [c]) ++ replaceIdLinks idCon otherCon cs
   | c == '\''
   = checkId ltxt ++ replaceIdLinks idCon otherCon cs
   | otherwise
   = tryReplaceIdLink (c:ltxt) cs

  checkId s =
    if ' ' `elem` s
    then otherCon ('\'' : (reverse s) ++ ['\''])
    else let (revfun, revmd) = break (=='.') s
          in if null revmd
               then idCon ""                     (reverse revfun)
               else idCon (reverse $ tail revmd) (reverse revfun)

genHtmlExportIndex :: [ExportEntry a] -> BaseHtml
genHtmlExportIndex es = ulistWithClass "nav flex-column" "nav-item"
                         $ map singleton (genHtmlExportSections es)

-- | Generates the left navigation panel from the export structure.
-- |
-- | The leafs (concrete export entities) of the export structure 
-- | are not included in the navigation panel, but only the sections.
genHtmlExportSections :: [ExportEntry a] -> [BaseHtml]
genHtmlExportSections = genHtmlExportSections' 0
  where genHtmlExportSections' _   [] = []
        genHtmlExportSections' num (ExportSection c nesting sub : rest) =
          (block [hrefNav ("#g"++show num) [htxt (commentString c)]]
            `addClass` ("indent" ++ show nesting)) :
          genHtmlExportSections' (num + 1) (sub ++ rest)
        genHtmlExportSections' num (ExportEntry _ : rest) =
          genHtmlExportSections' num rest
        genHtmlExportSections' num (ExportEntryModule _ : rest) =
          genHtmlExportSections' num rest

-- | Generates a simple list of exported types, operations and classes.
genExportEntityList :: DocOptions -> [ExportEntry CurryDocDecl] -> [BaseHtml]
genExportEntityList docopts es 
  | null allEntities 
    = []
  | otherwise 
    =  [ hrule ] 
    ++ singletonIf (not $ null types)
        (par $ [bold [htxt ("Exported Datatypes: ")]] ++ listIdentifiers types)
    ++ singletonIf (not $ null ops) 
        (par $ [bold [htxt ("Exported Functions: ")]] ++ listIdentifiers ops)
    ++ singletonIf (not $ null classes)
        (par $ [bold [htxt ("Exported Classes: "  )]] ++ listIdentifiers classes)
 where
  collectExportEntities :: CurryDocDecl
                        -> ([CurryDocDecl], [CurryDocDecl], [CurryDocDecl])
                        -> ([CurryDocDecl], [CurryDocDecl], [CurryDocDecl])
  collectExportEntities d (ts, os, cs) = case d of
    CurryDocTypeDecl     {} -> (d:ts,   os,   cs)
    CurryDocDataDecl     {} -> (d:ts,   os,   cs)
    CurryDocNewtypeDecl  {} -> (d:ts,   os,   cs)
    CurryDocFunctionDecl {} -> (  ts, d:os,   cs)
    CurryDocClassDecl    {} -> (  ts,   os, d:cs)

  allEntities = flattenExport es
  (types, ops, classes) = foldr collectExportEntities ([], [], [])
                        $ allEntities

  listIdentifiers = intersperse (htxt ", ") 
                  . map (showRef docopts)
                  . sortBy ((<) `on` (map toLower . snd))
                  . map curryDocDeclName

-- | Generates HTML documentation for a module.
genHtmlModule :: DocOptions -> ModuleHeader -> [BaseHtml]
genHtmlModule docopts (ModuleHeader fields maincmt) =
  [ block [ block (docComment2HTML docopts maincmt)
              `addClass` "info-left"
          , block [ block (map fieldHtml fields) 
                      `addClass` "info" 
                  ]
              `addClass` "info-right"
          ] 
      `addClass` "info-row" ]
 where 
  fieldHtml (typ, value) =
    (block [ block [htxt $ show typ] 
              `addClass` "info-key"
           , block [htxt value]      
              `addClass` "info-value"
           ]) 
      `addClass` "info-item"

-- | Generates HTML documentation for a datatype.
genHtmlType :: DocOptions -> CurryDocDecl -> [BaseHtml]
genHtmlType docopts d = case d of
  CurryDocDataDecl n@(tmod,tcons) vs inst _ cns cs ->
    code [anchored tcons [style "typeheader"
       ([bold [htxt "data"], nbsp,
         showCodeNameRef docopts n] ++ showVars vs)]]
    :  docComment2HTML docopts (concatCommentStrings (map commentString cs))
    ++ ifNotNull cns  [par [explainCat "Constructors: "]]
                      (ulistOrEmpty . map (genHtmlCons docopts n vs))
    ++ ifNotNull inst [par [explainCat "Known instances: "]]
                      (ulistOrEmpty . map (genHtmlInst docopts tmod))
  CurryDocNewtypeDecl n@(tmod,tcons) vs inst cn cs ->
    code [anchored tcons [style "typeheader"
       ([bold [htxt "newtype"], nbsp,
        showCodeNameRef docopts n] ++ showVars vs)]]
    :  docComment2HTML docopts (concatCommentStrings (map commentString cs))
    ++ (maybe [] ((par [explainCat "Constructor: "] :) .
                  genHtmlCons docopts n vs) cn)
    ++ ifNotNull inst [par [explainCat "Known instances: "]]
                      (ulistOrEmpty . map (genHtmlInst docopts tmod))
  CurryDocTypeDecl n@(tmod,tcons) vs ty cs ->
    code [anchored tcons [style "typeheader"
       ([bold [htxt "type"], nbsp,
         showCodeNameRef docopts n]
        ++ showVars vs ++
        [htxt " = ", BaseText rhs])]]
    :  docComment2HTML docopts (concatCommentStrings (map commentString cs))
    where rhs = case (tmod,tcons) of
                  ("Prelude", "String")
                    -> "[" ++ showTypeCons docopts tmod (tmod,"Char") ++ "]"
                  _ -> showType docopts tmod False ty
  _ -> []
 where
  showVars vs 
    | null vs   = [] 
    | otherwise = [nbsp, htxt (unwords (map snd vs))]

-- | Generates HTML documentation for a constructor.
genHtmlCons :: DocOptions -> QName -> [CTVarIName] -> CurryDocCons
            -> [BaseHtml]
genHtmlCons docopts ds vs (CurryDocConsOp (cmod, cname) ty1 ty2 ai cs) =
  genHtmlCons docopts ds vs (CurryDocConstr (cmod, "(" ++ cname ++ ")")
                              [ty1, ty2] ai cs)
genHtmlCons docopts (_, tcons) vs (CurryDocConstr (cmod, cname) tys ai cs) =
  anchored cname
    [code [opnameDoc [htxt cname],
           BaseText (" :: " ++
                    concatMap (\t -> " " ++ showType docopts cmod True t ++ " -> ")
                              tys ++
                    tcons ++ " " ++ unwords (map snd vs))]] :
    (ifNotNull txt [] (removeTopPar . docComment2HTML docopts)) ++
    maybe []
      (\(fixity, prec) -> [par [htxt ("defined as " ++ showFixity fixity ++
                                      " infix operator with precedence " ++
                                      show prec)]]) fix
  where txt = concatCommentStrings (map commentString cs)
        fix = case ai of
          NoAnalysisInfo -> Nothing
          _              -> precedence ai
genHtmlCons docopts (_, tcons) vs (CurryDocRecord (cmod,cname) tys fs ai cs) =
  anchored cname
    [code [opnameDoc [htxt cname],
          BaseText (" :: " ++
                    concatMap (\t -> " "++showType docopts cmod True t++" -> ")
                              tys ++
                    tcons ++ " " ++ unwords (map snd vs))]] :
    (ifNotNull txt [] (removeTopPar . docComment2HTML docopts)) ++
    ifNotNull fs [par [explainCat "Fields: "]]
                 (ulistOrEmpty . map (genHtmlField docopts)) ++
    maybe []
      (\p -> genPrecedenceText p) fix
  where txt = concatCommentStrings (map commentString cs)
        fix = case ai of
          NoAnalysisInfo -> Nothing
          _              -> precedence ai

-- TODO: show precedence
-- | Generates HTML documentation for record fields.
genHtmlField :: DocOptions -> CurryDocField -> [BaseHtml]
genHtmlField docopts (CurryDocField (fmod,fname) ty _ cs) =
  [anchored fname
    ([ code [opnameDoc [htxt fname], 
             BaseText (" :: " ++ showType docopts fmod True ty)]
     ] ++ ifNotNull txt [htxt " : "]
                        (removeTopPar . docComment2HTML docopts))]
  where txt = concatCommentStrings (map commentString cs)

-- Generate HTMl for a typeclass instance.
genHtmlInst :: DocOptions -> String -> CurryDocInstanceDecl -> [BaseHtml]
genHtmlInst docopts dn d = case d of
  CurryDocInstanceDecl i@(imod, _) cx ts _ _ ->
    [code ((if null cxString then [] else [BaseText cxString, nbsp]) ++
           [BaseText (showType docopts dn False (CTCons i)), nbsp] ++
           (intersperse nbsp $ map (\ty -> BaseText (showType docopts dn
                                            (isApplyType ty || isFunctionType ty) ty)) ts))]
    where cxString = showContext docopts imod True cx

-- | Generates HTML documentation for a typeclass.
genHtmlClass :: DocOptions -> CurryDocDecl -> [BaseHtml]
genHtmlClass docopts d = case d of
  CurryDocClassDecl (cmod, cname) cx vs fdeps ds cs ->
       [anchored cname
         [(code
           ([bold [htxt "class "]] ++
             (if null cxString then [] else [BaseText cxString, nbsp]) ++
             [showCodeNameRef docopts (cmod, cname)] ++ [BaseText $ ' ' : showVarList vs] ++ fdepsExp)
           `addClass` "classheader")]]
    ++ docComment2HTML docopts (concatCommentStrings (map commentString cs))
    ++ ifNotNull ds
         [par [explainCat "Methods: "]]
         ((:[]) . borderedTable
                . map ((:[]) . genHtmlFunc "classfunctionheader" docopts))
    where cxString = showContext docopts cmod True cx
          fdepsExp 
            | null fdeps = []
            | otherwise  = [BaseText $ " | "
                             ++ (concat $ intersperse ", " $ map funDepString fdeps)]
          funDepString (CurryDocFunDep (tl, tr)) = showVarList tl ++ " -> " ++ showVarList tr
  _ -> []

-- | Generates HTML documentation for a function.
genHtmlFunc :: String -> DocOptions -> CurryDocDecl -> [BaseHtml]
genHtmlFunc cssclass docopts d = case d of
  CurryDocFunctionDecl f@(fmod,fname) qty sig ai cs ->
    [par ([code [anchored fname [style cssclass
             [opnameDoc [showCodeNameRef docopts f],
              BaseText (" :: "++ showQualType docopts fmod qty)]]],
           nbsp, nbsp] ++
           genFuncPropIcons ai)] ++
    genSigComment docopts sig ++
    docComment2HTML docopts (concatCommentStrings (map commentString cs)) ++
    genFurtherInfos docopts (fmod, fname) ai
  _ -> []

-- | Generates HTML for the given type signature, if one is present.
genSigComment :: DocOptions -> Maybe CurryDocTypeSig -> [BaseHtml]
genSigComment _       Nothing  = []
genSigComment docopts (Just d) = case d of
  CurryDocTypeSig (fmod, _) (CContext []   ) ps cs ->
    [ par (docComment2HTML docopts
             (concatCommentStrings (map commentString cs)))]
    ++ if all (null . snd) ps
         then []
         else [table (genParamComments docopts fmod "::" ps)]
  CurryDocTypeSig (fmod, _) cx@(CContext (_:_)) ps cs ->
    [ par (docComment2HTML docopts
             (concatCommentStrings (map commentString cs)))]
    ++ if all (null . snd) ps
         then []
         else [table ([[code [BaseText (":: " ++
                                        showContext docopts fmod False cx)]]]
                : genParamComments docopts fmod "=>" ps)]

-- | Generates HTML for the parameters of a given type signature.
genParamComments :: DocOptions -> String -> String -> [(CTypeExpr, [Comment])]
                 -> [[[BaseHtml]]]
genParamComments _       _    _   []              = []
genParamComments docopts fmod sym ((ty, cs) : xs) =
  [[code [BaseText (sym ++ " " ++ showType docopts fmod False ty)], nbsp],
   removeTopPar (docComment2HTML docopts (unwords (map commentString cs)))]
    : genParamComments docopts fmod "->" xs

-- | Generates HTML for any given `AnalysisInfo` of an entity.
genFurtherInfos :: DocOptions -> QName -> AnalysisInfo -> [BaseHtml]
genFurtherInfos docopts qn ai = case ai of
    NoAnalysisInfo          -> []
    PrecedenceInfo Nothing  -> []
    PrecedenceInfo (Just p) -> [dlist [([explainCat "Further infos:"],
                                         genPrecedenceText p)]]
    ShortAnalysisInfo {}    -> properties ++
                               if null shortContent
                                 then []
                                 else [dlist [([explainCat "Further infos:"],
                                                shortContent)]]
    AnalysisInfo {}         -> properties ++
                               if null content
                                 then []
                                 else [dlist [([explainCat "Further infos:"],
                                                content)]]
  where
    properties = showProperties "Precondition"  PreSpec
              ++ showProperties "Postcondition" PostSpec
              ++ showProperties "Specification" Spec
              ++ showProperties "Properties"    Prop

    showProperties :: String -> Property -> [BaseHtml]
    showProperties title prop =
      let ps = filter ((== prop) . fst) (property ai)
      in if null ps
           then []
           else [dlist [( [explainCat $ title ++ ":"]
                        , intercalate [breakline] 
                            $ map (showProperty docopts qn) ps
                        )]]

    shortContent =
      maybe [] (\p -> genPrecedenceText p) (precedence ai) ++
      [ulist $ map singleton $ catMaybes [externalInfo]]

    content =
      maybe [] (\p -> genPrecedenceText p) (precedence ai) ++
      [ulist $ map singleton $ catMaybes
        [ completenessInfo
        , indeterminismInfo
        , opcompleteInfo
        , externalInfo 
        ]]

    -- Comment about partial/incomplete definition:
    completenessInfo = let ci = complete ai in
      if ci == Complete
       then Nothing
       else Just (htxt
         (if ci == InComplete
            then 
              "partially defined"
            else
              "partially defined in each disjunction (but might be complete)"))

    -- Comment about the indeterminism of a function:
    indeterminismInfo =
      if indet ai
        then Just (htxt "might behave indeterministically")
        else Nothing

    -- Comment about the solution completeness of a function:
    opcompleteInfo =
       if opComplete ai
         then Just (htxt "solution complete, i.e., able to compute all solutions")
         else Nothing

    -- Comment about the external definition of a function:
    externalInfo  =
      if ext ai
        then Just (htxt "externally defined")
        else Nothing

-- | Generates a descriptive text for the given precedence.
genPrecedenceText :: (CFixity, Int) -> [BaseHtml]
genPrecedenceText (fixity, prec) =
  [par [htxt ("defined as " ++ showFixity fixity ++
              " infix operator with precedence " ++
              show prec)]]

-- | Generates HTML for a given property of a function.
--   'Data.Type.GuardedRhs' are not formatted in any specific way.
showProperty :: DocOptions -> QName -> (Property, QRule) -> [BaseHtml]
showProperty docopts qn pr = showProperty' (unqualify pr)
                          ++ [htxt "(", showCodeNameRef docopts (functionName pr), htxt ")"]
 where 
  showProperty' (sp, rule) = case (sp, rule) of
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

-- | Generates icons for particular properties of functions.
genFuncPropIcons :: AnalysisInfo -> [BaseHtml]
genFuncPropIcons    NoAnalysisInfo       = []
genFuncPropIcons    PrecedenceInfo    {} = []
genFuncPropIcons    ShortAnalysisInfo {} = []
genFuncPropIcons ai@AnalysisInfo      {} = [detPropIcon, nbsp]
 where
   -- (non)deterministically defined property:
   detPropIcon =
    if nondet ai
     then href "index.html#nondet_explain" [nondetIcon]
     else href "index.html#det_explain"    [detIcon]

--------------------------------------------------------------------------
-- Pretty printer for qualified types in Curry syntax:
showQualType :: DocOptions -> String -> CQualTypeExpr -> String
showQualType opts mod (CQualType ctxt texp) =
  unwords [showContext opts mod True ctxt, showType opts mod False texp]

showContext :: DocOptions -> String -> Bool -> CContext -> String
showContext _ _ _ (CContext []) = ""
showContext opts mod arr (CContext [clscon]) =
  showConstraint opts mod clscon ++ if arr then " =>" else ""
showContext opts mod arr (CContext ctxt@(_:_:_)) =
  bracketsIf True (intercalate ", " (map (showConstraint opts mod) ctxt)) ++
  if arr then " =>" else ""

-- | Pretty-prints a single class constraint.
showConstraint :: DocOptions -> String -> CConstraint -> String
showConstraint opts mod (cn,texps) =
  showTypeCons opts mod cn ++ " " ++ unwords (map (showType opts mod True) texps)

-- | Pretty-prints type expressions in Curry syntax.  
--   The second argument is True iff brackets must be written around complex types.
showType :: DocOptions -> String -> Bool -> CTypeExpr -> String
showType opts mod nested texp = case texp of
  CTVar (_,n) -> n
  CFuncType t1 t2 ->
    bracketsIf nested (showType opts mod (isFunctionalType t1) t1++" -&gt; "++
                     showType opts mod False t2)
  CTCons tc -> showTConsType opts mod nested tc []
  CTApply t1 t2 ->
       maybe (bracketsIf nested $
                showType opts mod True t1 ++ "&nbsp;" ++
                showType opts mod True t2)
             (\ (tc,ts) -> showTConsType opts mod nested tc ts)
             (tconsArgsOfType texp)

-- | Pretty-prints a list of type variables.
showVarList :: [CTVarIName] -> String
showVarList = unwords . map snd

showTConsType :: DocOptions -> String -> Bool -> QName -> [CTypeExpr] -> String
showTConsType opts mod nested tc ts
 | ts==[]  = showTypeCons opts mod tc
 | tc=~=("Prelude","[]") && (head ts == CTCons ("Prelude","Char"))
   = showTypeCons opts mod ("Prelude", "String")
 | tc=~=("Prelude","[]")
   = "[" ++ showType opts mod False (head ts) ++ "]" -- list type
 | take 2 (snd tc) == "(,"                           -- tuple type
   = "(" ++ concat (intersperse ", " (map (showType opts mod False) ts)) ++ ")"
 | otherwise
   = bracketsIf nested
      (showTypeCons opts mod tc ++ "&nbsp;" ++
       concat (intersperse " " (map (showType opts mod True) ts)))

showTypeCons :: DocOptions -> String -> QName -> String
showTypeCons opts mod (mtc,tc) =
  if mod == mtc
    then "<a href=\"#"++tc++"\">"++tc++"</a>"
    else "<a href=\""++docURL opts mtc++".html#"++tc++"\">"++tc++"</a>"

showFixity :: CFixity -> String
showFixity CInfixOp  = "non-associative"
showFixity CInfixlOp = "left-associative"
showFixity CInfixrOp = "right-associative"

--------------------------------------------------------------------------
-- | Translates source file into HTML file with syntax coloring.
translateSource2ColoredHtml :: String -> String -> IO ()
translateSource2ColoredHtml docdir modname = do
    let output = docdir </> modname++"_curry.html"
    putStrLn ("Writing source file as HTML to \""++output++"\"...")
    callFrontendWithParams HTML
      (setQuiet True (setHtmlDir docdir defaultParams)) modname

--------------------------------------------------------------------------
-- | Generates the index page for the documentation directory.
genMainIndexPage :: DocOptions -> String -> [String] -> IO ()
genMainIndexPage docopts docdir modnames =
 do putStrLn ("Writing index page to \""++docdir++"/index.html\"...")
    simplePage ("index.html", shorttitle) 
               "Documentation of Curry modules"
               (Just pageTitle)
               allConsFuncsClassesMenu (indexPage modnames)
     >>= writeFile (docdir++"/index.html")
 where
  shorttitle = if not (null (mainTitle docopts))
                then [htxt (mainTitle docopts)]
                else if length modnames == 1
                      then [code [htxt $ head modnames], nbsp,
                            htxt "documentation"]
                      else [htxt "Curry Documentation"]
  pageTitle = if not (null (mainTitle docopts))
                then [htxt (mainTitle docopts)]
                else if length modnames == 1
                      then [htxt "Documentation of the Curry program ",
                            href (head modnames ++ ".html")
                                 [htxt (head modnames)]]
                      else [htxt "Documentation of Curry programs"]

allConsFuncsClassesMenu :: [[BaseHtml]]
allConsFuncsClassesMenu =
  [[hrefNav "findex.html"   [htxt "All operations"]],
   [hrefNav "cindex.html"   [htxt "All constructors"]],
   [hrefNav "clsindex.html" [htxt "All type classes"]]]

indexPage :: [String] -> [BaseHtml]
indexPage modnames =
  (if null modnames
   then []
   else [h2 [htxt "Modules:"],
         ulist (map (\m->[href (m++".html") [htxt m]])
                    (sortBy leqStringIgnoreCase modnames))])
  ++ [explainIcons]

-- | Paragraph to explain the meaning of the icons.
explainIcons :: BaseHtml
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
-- | Generates the function index page for the documentation directory.
genFunctionIndexPage :: DocOptions -> String -> [CFuncDecl] -> IO ()
genFunctionIndexPage opts docdir funs = do
  putStrLn ("Writing operation index page to \""++docdir++"/findex.html\"...")
  simplePage (getHomeRef opts) "Index to all operations" Nothing
             allConsFuncsClassesMenu (htmlIndex opts (sortNames expfuns))
    >>= writeFile (docdir++"/findex.html")
 where
   expfuns = nub $ map funcName $ filter ((== Public) . funcVis) funs

htmlIndex :: DocOptions -> [QName] -> [BaseHtml]
htmlIndex opts = categorizeByItemKey . map (showModNameRef opts)

-- | Generates a reference to an identifier.
showRef :: DocOptions -> QName -> BaseHtml
showRef opts (modname, name) =
  href (docURL opts modname ++ ".html#" ++ name) [htxt name]

showModNameRef :: DocOptions -> QName -> (String,[BaseHtml])
showModNameRef opts (modname,name) =
  (name,
   [href (docURL opts modname++".html#"++name) [htxt name], nbsp, nbsp,
    htxt "(", href (docURL opts modname ++ ".html") [htxt modname], htxt ")"]
  )

showCodeNameRef :: DocOptions -> QName -> BaseHtml
showCodeNameRef opts (modname,name) =
  href (docURL opts modname++"_curry.html#"++name) [htxt name']
  where name' = if isOperator then "(" ++ name ++ ")" else name
        isOperator = all (`elem` "~!@#$%^&*+-=<>:?./|\\") name

sortNames :: [(a,String)] -> [(a,String)]
sortNames names = sortBy (\(_,n1) (_,n2) -> leqStringIgnoreCase n1 n2) names

--------------------------------------------------------------------------
-- | Generates the constructor index page for the documentation directory.
genConsIndexPage :: DocOptions ->  String -> [CTypeDecl] -> IO ()
genConsIndexPage opts docdir types = do
  putStrLn ("Writing constructor index page to \""++docdir++"/cindex.html\"...")
  simplePage (getHomeRef opts) "Index to all constructors" Nothing 
             allConsFuncsClassesMenu (htmlIndex opts (sortNames expcons))
    >>= writeFile (docdir++"/cindex.html")
 where
   consDecls (CType    _ _ _ cs _) = cs
   consDecls (CNewType _ _ _ c  _) = [c]
   consDecls (CTypeSyn _ _ _ _   ) = []
   expcons = nub $ map consName $ filter ((== Public) . consVis) $
     concatMap consDecls types

--------------------------------------------------------------------------
-- | Generates the type classes index page for the documentation directory.
genClassesIndexPage :: DocOptions ->  String -> [CClassDecl] -> IO ()
genClassesIndexPage opts docdir cls = do
  putStrLn ("Writing type classes index page to \"" ++ docdir ++
            "/clsindex.html\"...")
  simplePage (getHomeRef opts) "Index to all type classes" Nothing 
             allConsFuncsClassesMenu (htmlIndex opts (sortNames expclasses))
    >>= writeFile (docdir++"/clsindex.html")
 where
   expclasses = nub $ map    (\(CClass n _   _ _ _ _) -> n) $
                      filter (\(CClass _ vis _ _ _ _) -> vis == Public) cls

--------------------------------------------------------------------------
-- | Generates the index page categorizing all system libraries of PAKCS/KICS2.
genSystemLibsPage :: String -> [String] -> [[(String, ModuleHeader)]] -> IO ()
genSystemLibsPage docdir cats modInfos = do
  putStrLn $ "Writing main index page for " ++ currySystem ++
             " to \"" ++ fname ++ "\"..."
  mainPage (curryPackagesURL, [htxt "Curry Packages"])
           (currySystem ++ " Libraries")
           [h1 [htxt $ currySystem ++ ": System Libraries"]]
           syslibsLeftTopMenu
           syslibsRightTopMenu
           (syslibsSideMenu cats)
           ([infoTxt, hrule] ++
             genHtmlLibCats modInfos ++
             [hrule, explainIcons])
   >>= writeFile fname
 where
  fname = docdir ++ "/" ++ currySystem ++ "_libs.html"

syslibsLeftTopMenu :: [[BaseHtml]]
syslibsLeftTopMenu =
  [ [hrefNav (currySystemURL ++ "/Manual.pdf")    [htxt "Manual (PDF)"]]
  , [hrefNav (currySystemURL ++ "/lib/")          [htxt "Libraries"]]
  , [ehrefNav currygleURL                         [htxt "API Search"]]
  , [hrefNav (currySystemURL ++ "/download.html") [htxt "Download"]]
  ]

syslibsRightTopMenu :: [[BaseHtml]]
syslibsRightTopMenu =
  [ curryHomeItem
  , [ehrefNav "https://curry-lang.org/documentation/report/"
              [htxt "Curry Report"]]
  ]

syslibsSideMenu :: [String] -> [BaseHtml]
syslibsSideMenu cats = map par $
     [[ehrefNav currygleURL [htxt "Search with Curr(y)gle"]]]
  ++ [[hrefNav ("#" ++ c) [ htxt (genCatExplain c)]] | c <- cats]
  ++ [ [hrefNav "findex.html"    [htxt "Index to all library functions"]]
     , [hrefNav "cindex.html"    [htxt "Index to all library constructors"]]
     , [hrefNav "#explain_icons" [htxt "Icons used in the documentation"]]
     ]

infoTxt :: BaseHtml
infoTxt = par
  [ htxt "Here is the collection of libraries contained in the distribution of "
  , href currySystemURL [htxt currySystem]
  , htxt $ ". Most of these libraries have been implemented during the "
        ++ "development of larger Curry applications. If you have suggestions "
        ++ "for changes/improvements or if you want to contribute your own "
        ++ "library, please contact "
  , href "http://www.michaelhanus.de/" [htxt "Michael Hanus"]
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

genHtmlLibCats :: [[(String, ModuleHeader)]] -> [BaseHtml]
genHtmlLibCats = concatMap gen
  where
    gen [] = []
    gen mods@((_, (ModuleHeader xs _)):_) =
      let c = getCategoryWithDefault "general" xs
      in [anchoredSection (getCategoryWithDefault "general" xs)
            (h2 [htxt (genCatExplain c ++ ":")] : genHtmlLibCat mods)]

genHtmlLibCat :: [(String, ModuleHeader)] -> [BaseHtml]
genHtmlLibCat mods =
  [dlist [(genHtmlName modname, docComment2HTML defaultCurryDocOptions modcmt)
  | (modname, ModuleHeader _ modcmt) <- mods ]
  ]
 where
  genHtmlName modname = [code [href (modname ++ ".html") [htxt modname]]]

--------------------------------------------------------------------------
-- Auxiliary operation for general page style.

-- | Generates the main page with the default documentation style.
mainPage :: (String, [BaseHtml])
         -> String      -- ^ The title of the page
         -> [BaseHtml]   -- ^ The title of the pagethe title in HTML format
         -> [[BaseHtml]] -- ^ The menu shown at left of the top
         -> [[BaseHtml]] -- ^ The menu shown at left of the top
         -> [BaseHtml]   -- ^ The menu shown at the left-hand side
         -> [BaseHtml]   -- ^ The main contents of the page
         -> IO String
mainPage homeref title htmltitle lefttopmenu righttopmenu sidemenu maindoc = do
    time <- getLocalTime
    return $ showHtmlPage $
      bootstrapPage favIcon cssIncludes jsIncludes title homeref
                    lefttopmenu righttopmenu 3 sidemenu htmltitle maindoc
                    (curryDocFooter time)

favIcon :: String
favIcon = styleBaseURL </> "img" </> "favicon.ico"

-- | The CSS includes relative to the base directory of bt4.
cssIncludes :: [String]
cssIncludes = map (\n -> styleBaseURL </> "css" </> n ++ ".css") 
                  ["bootstrap.min", "currydoc"]

-- | The javascript includes.
jsIncludes :: [String]
jsIncludes = 
  ["https://code.jquery.com/jquery-3.4.1.slim.min.js",
   styleBaseURL </> "js" </> "bootstrap.bundle.min.js",
   styleBaseURL </> "js" </> "theme.js"]

-- | Generates a page with the default documentation style.
showPageWithDocStyle :: String    -- ^ The title of the page
                     -> [BaseHtml] -- ^ The main contents of the page
                     -> String
showPageWithDocStyle title body =
  showHtmlPage $
    HtmlPage title
             (map (\f -> pageCSS $ styleBaseURL ++ "/css/" ++ f ++ ".css")
                  cssIncludes)
             body

-- | The standard right top menu.
rightTopMenu :: [[BaseHtml]]
rightTopMenu =
  [ [hrefNav  "index.html"     [htxt "Module Index"]]
  , [ehrefNav baseLibsURL      [htxt "Base Libraries"]]
  , [ehrefNav curryPackagesURL [htxt "Curry Packages"]]
  , curryHomeItem
  , [ehrefNav curryDocURL      [htxt "About CurryDoc"]]
  , [themeToggleButton]
  ]
 where
  themeToggleButton :: BaseHtml
  themeToggleButton = 
    htmlStruct "button" 
      [("id", "theme-toggle"), ("class", "btn btn-sm btn-outline-secondary")]
      [ htmlStruct "span" [("class", "theme-icon-light")] [htmlText "🌙"]
      , htmlStruct "span" [("class", "theme-icon-dark")]  [htmlText "☀️"]
      ]
--------------------------------------------------------------------------
-- Icons:

detIcon :: BaseHtml
detIcon     = image (styleBaseURL ++ "/img/forward-fill.svg") "Deterministic"
                `addTitle` "This operation is deterministic" 
                `addClass` "svg-icon"
nondetIcon :: BaseHtml
nondetIcon  = image (styleBaseURL ++ "/img/share-fill.svg") "Non-deterministic"
                `addTitle` "This operation might be non-deterministic"
                `addClass` "svg-icon"

-- rigidIcon :: HtmlExp
-- rigidIcon     = italic [] `addClass` "fa fa-cogs"
--                   `withTitle` "This operation is rigid"
-- flexibleIcon :: HtmlExp
-- flexibleIcon  = italic [] `addClass` "fa fa-pagelines"
--                   `withTitle` "This operation is flexible"
-- flexrigidIcon :: HtmlExp
-- flexrigidIcon = italic [] `addClass` "fa fa-exclamation-triangle"
--     `withTitle` "This operation is partially flexible and partially rigid"

addTitle :: BaseHtml -> String -> BaseHtml
addTitle he t = he `addAttr` ("title",t)

--------------------------------------------------------------------------
-- Standard footer information for generated web pages:
curryDocFooter :: CalendarTime -> [BaseHtml]
curryDocFooter time = map fromHtmlExp $ 
  [italic [htxt "Generated by ",
           bold [htxt "CurryDoc"],
           htxt (" ("++currydocVersion++") at "),
           htxt (calendarTimeToString time)]]

curryHomeItem :: [BaseHtml]
curryHomeItem = [ehrefNav curryHomeURL [htxt "Curry Homepage"]]

-- | Generate a simple page with the default documentation style.
simplePage :: (String, [BaseHtml]) 
           -> String               -- ^ The title of the page
           -> Maybe [BaseHtml]     -- ^ Maybe a specific title for h1 header
           -> [[BaseHtml]]         -- ^ The menu shown at left of the top
           -> [BaseHtml]           -- ^ The main contents of the page
           -> IO String
simplePage homeref title htmltitle lefttopmenu maindoc = do
    time <- getLocalTime
    return $ showHtmlPage $
      bootstrapPage favIcon cssIncludes jsIncludes title homeref
                    lefttopmenu rightTopMenu 0 []
                    [h1 (maybe [htxt title] id htmltitle)]
                    maindoc
                    (curryDocFooter time)

-- | An anchored section in the document:
anchoredSection :: String -> [BaseHtml] -> BaseHtml
anchoredSection tag doc = section doc `addAttr` ("id",tag)

-- | An anchored element in the document:
anchored :: String -> [BaseHtml] -> BaseHtml
anchored tag doc = style "anchored jump-offset" doc `addAttr` ("id",tag)

-- | An anchored element in the document:
anchoredDiv :: String -> [BaseHtml] -> BaseHtml
anchoredDiv tag doc = block doc `addAttr` ("id",tag)

-- | A bordered table:
borderedTable :: [[[BaseHtml]]] -> BaseHtml
borderedTable rows = table rows `addClass` "table table-bordered table-hover"

-- | A reference to the index page with a suitable title.
getHomeRef :: DocOptions -> (String, [BaseHtml])
getHomeRef opts = ("index.html", title)
 where 
  title 
    | not (null (mainTitle opts)) = [htxt (mainTitle opts)]
    | otherwise                   = [htxt "Curry Documentation"]

--------------------------------------------------------------------------
-- Auxiliaries:

-- | Generates a link to the top of the page.
--   Useful to jump back to the module header.
jumpToTop :: BaseHtml -> BaseHtml
jumpToTop = href "#" . singleton

fromHtmlExps :: [HtmlExp] -> [BaseHtml]
fromHtmlExps = map fromHtmlExp

ulistOrEmpty :: [[BaseHtml]] -> [BaseHtml]
ulistOrEmpty items | null items = []
                   | otherwise  = [ulist items]

-- | Generates the html documentation for given comments ("param", "return",...).
ifNotNull :: [a] -> [b] -> ([a] -> [b]) -> [b]
ifNotNull cmt doc genDoc
  | null cmt  = []
  | otherwise = doc ++ genDoc cmt

-- | Style for explanation categories, like "Constructors:", "Parameters:",...
explainCat :: String -> BaseHtml
explainCat s = textstyle "explaincat" s

-- | Style for function/constructor name shown in the documentation part:
opnameDoc :: [BaseHtml] -> BaseHtml
opnameDoc = style "opname"

-- | Less-or-equal comparison for strings (ignoring case)
leqStringIgnoreCase :: String -> String -> Bool
leqStringIgnoreCase = leqList leqCharIgnoreCase

-- | Less-or-equal comparison for lists
leqList :: Eq a => (a -> a -> Bool) -> [a] -> [a] -> Bool
leqList leq xs ys = case (xs,ys) of
  ([],_)        -> True
  (_,[])        -> False
  (x:xs',y:ys') -> leq x y || (x == y && leqList leq xs' ys')

-- | Less-or-equal comparison for characters (ignoring case)
leqCharIgnoreCase :: Char -> Char -> Bool
leqCharIgnoreCase c1 c2 = toUpper c1 <= toUpper c2

-- | Sorts a list of strings.
sortStrings :: [String] -> [String]
sortStrings strings = sortBy leqStringIgnoreCase strings

-- | Returns the first sentence in a string:
firstSentence :: String -> String
firstSentence s = let (fs,ls) = break (=='.') s in
  if null ls
  then fs
  else if tail ls /= "" && isSpace (head (tail ls))
       then fs ++ "."
       else fs ++ "." ++ firstSentence (tail ls)

-- | Puts brackets around second argument if the first argument is True.
bracketsIf :: Bool -> String -> String
bracketsIf False s = s
bracketsIf True  s = "("++s++")"

-- | Gets the first identifier (name or operator in brackets) in a string.
getFirstId :: String -> String
getFirstId [] = ""
getFirstId (c:cs)
  | isAlpha c = takeWhile isIdChar (c:cs)
  | c == '('  = let bracketid = takeWhile (/= ')') cs
                 in if all (`elem` infixIDs) bracketid
                    then bracketid
                    else ""
  | otherwise = ""

-- | Is an alphanumeric character, underscore, or apostroph?
isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_' || c == '\''

-- | All characters occurring in infix operators.
infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

-- | Removes a single top-level paragraph in HTML expressions.
removeTopPar :: [BaseHtml] -> [BaseHtml]
removeTopPar hexps = case hexps of
  [BaseStruct "p" [] hs] -> hs
  _ -> hexps

-- | Encloses a non-letter identifier in brackets.
showId :: String -> String
showId name 
  | null name = error "showId: empty name"
  | otherwise = if isAlpha (head name) 
                  then name
                  else '(' : name ++ ")"

-- | Generates a singleton list containing the given element.
singleton :: a -> [a]
singleton = (:[])

-- | Generates a singleton list containing the given element if the condition is True.
singletonIf :: Bool -> a -> [a]
singletonIf True  = singleton
singletonIf False = const []