----------------------------------------------------------------------
--- Operations to generate documentation in HTML format.
---
--- @author Michael Hanus, Jan Tikovsky
--- @version April 2016
----------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

module CurryDoc.Html where

import AbstractCurry.Types
import AbstractCurry.Files
import AbstractCurry.Select
import AbstractCurry.Build
import AbstractCurry.Pretty
import qualified FlatCurry.Types as FC
import qualified FlatCurry.Goodies as FCG
import FilePath
import FileGoodies     (getFileInPath)
import HTML
import List
import Char
import Sort
import Time
import Distribution
import Markdown
import Maybe
import Pretty          (pretty, empty)

import Analysis.TotallyDefined(Completeness(..))
import HTML.Styles.Bootstrap3 (bootstrapPage, glyphicon, homeIcon)
import HTML.CategorizedList

import CurryDoc.AnaInfo
import CurryDoc.Options
import CurryDoc.Read
import CurryDoc.Config

infixl 0 `withTitle`

--------------------------------------------------------------------------
-- Generates the documentation of a module in HTML format where the comments
-- are already analyzed.
generateHtmlDocs :: DocOptions -> AnaInfo -> String -> String
                 -> [(SourceLine,String)] -> IO String
generateHtmlDocs docopts anainfo modname modcmts progcmts = do
  acyname <- getLoadPathForModule modname >>=
             getFileInPath (abstractCurryFileName modname) [""]
  putStrLn $ "Reading AbstractCurry program \""++acyname++"\"..."
  (CurryProg _ imports types functions ops) <- readAbstractCurryFile acyname
  let
    exptypes   = filter isExportedType types
    expfuns    = filter isExportedFun  functions
    propspecs  = map cmtFunc2Func
                     (filter (\fd -> isProperty fd || isSpecFunc fd) functions)
    navigation =
      [ bold [htxt "Exported names:"]
      , genHtmlExportIndex (map tName      exptypes)
                           (getExportedCons   types)
                           (getExportedFields types)
                           (map fName       expfuns)
      , anchored "imported_modules" [bold [htxt "Imported modules:"]]
      , ulist (map (\i -> [href (getLastName i++".html") [htxt i]]) imports)
        `addClass` "nav nav-sidebar"
      ]
    content =
      genHtmlModule docopts modcmts ++
      [ h2 [htxt "Summary of exported operations:"]
      , borderedTable (map (genHtmlFuncShort docopts progcmts anainfo) expfuns)
      ] ++
      ifNotNull exptypes (\tys ->
         [anchoredSection "exported_datatypes"
           (h2 [htxt "Exported datatypes:"] : hrule :
           concatMap (genHtmlType docopts progcmts) tys)]) ++
      [anchoredSection "exported_operations"
         (h2 [htxt "Exported operations:"] :
          map (genHtmlFunc docopts modname progcmts
                 (attachProperties2Funcs propspecs progcmts) anainfo ops)
              expfuns)
      ]
  mainPage title [htmltitle] (lefttopmenu types functions) rightTopMenu 
           navigation content
 where
  title = "Module " ++ modname

  htmltitle = h1 [ htxt "Module "
                 , href (modname ++ "_curry.html") [htxt modname]
                 ]

  lefttopmenu :: [CTypeDecl] -> [CFuncDecl] -> [[HtmlExp]]
  lefttopmenu ts fs
    = [[href "?" [htxt title]], [href "#imported_modules" [htxt "Imports"]]]
   ++ ifNotNull ts (const [[href "#exported_datatypes"  [htxt "Datatypes" ]]])
   ++ ifNotNull fs (const [[href "#exported_operations" [htxt "Operations"]]])

  cmtFunc2Func fdecl = case fdecl of
                         CmtFunc _ qn a v tExp rs -> CFunc qn a v tExp rs
                         _                        -> fdecl

-- Datatype to classify the kind of information attached to a function:
data FuncAttachment = Property | PreCond | PostCond | SpecFun

-- Associate the properties or contracts (first argument)
-- to functions according to their positions and name in the source code
-- (we assume that they follow the actual function definitions).
-- Each property or contract is represented by its kind ('FuncAttachment'),
-- its name, and its documentation (HTML document).
attachProperties2Funcs :: [CFuncDecl] -> [(SourceLine,String)]
                       -> [(String,[(FuncAttachment,String,[HtmlExp])])]
attachProperties2Funcs _ [] = []
attachProperties2Funcs props ((sourceline,_) : slines) =
  case sourceline of
    FuncDef fn -> let (fprops,rslines) = span isPropFuncDef slines
                   in (fn, showContracts fn ++ concatMap showProp fprops) :
                      attachProperties2Funcs props rslines
    _          -> attachProperties2Funcs props slines
 where
  propNames = map (snd . funcName) props

  showProp (FuncDef fn,_) =
    let propdecl = fromJust (find (\fd -> snd (funcName fd) == fn) props)
     in if isProperty propdecl
        then map (\rhs -> (Property, fn,
                           [code [htxt $ prettyWith (ppCRhs empty) rhs]]))
                 (map ruleRHS (funcRules propdecl))
        else []

  showContracts fn =
    showContract (fn++"'pre")  showPreCond ++
    showContract (fn++"'post") showPostCond ++
    showContract (fn++"'spec") showSpec

  showContract fnsuff formatrule =
    maybe []
          (\contractdecl -> showRulesWith formatrule fnsuff contractdecl)
          (find (\fd -> snd (funcName fd) == fnsuff) props)

  showRulesWith formatrule fnsuff (CFunc qn@(mn,fn) ar _ ftype rules) =
    let stripSuffix = reverse . tail . dropWhile (/='\'') . reverse
     in map (formatrule fnsuff qn (mn,stripSuffix fn)
              . etaExpand ar (length (argTypes ftype))) rules

  -- eta expand simple rules for more reasonable documentation
  etaExpand arity tarity rule = case rule of
    CRule ps (CSimpleRhs exp ldecls) ->
      if arity == tarity
      then rule
      else let evars = map (\i -> (i,"x"++show i)) [(arity+1) .. tarity]
            in CRule (ps ++ map CPVar evars)
                     (CSimpleRhs (foldl CApply exp (map CVar evars)) ldecls)
    _ -> rule -- don't do it for complex rules

  showPreCond fnpre qp qn rule = case rule of
   CRule _ (CSimpleRhs _ _) ->
     let (lhs,rhs) = break (=='=') (prettyRule qn rule)
      in (PreCond, fnpre, [code [htxt $ "(" ++ stripSpaces lhs ++ ")"],
                           italic [htxt " requires "],
                           code [htxt (safeTail rhs)]])
   _ -> -- we don't put must effort to format complex preconditions:
        (PreCond, fnpre, [code [htxt $ prettyRule qp rule]])

  showPostCond fnpost qp qn rule = case rule of
   CRule ps (CSimpleRhs _ _) ->
     let (_,rhs) = break (=='=') (prettyRule qn rule)
      in (PostCond, fnpost,
          [code [htxt $ prettyWith ppCPattern (last ps) ++ " = " ++
                        prettyWith ppCPattern
                                   (CPComb qn (take (length ps - 1) ps)) ],
                 italic [htxt " satisfies "],
                 code [htxt (safeTail rhs)]])
   _ -> -- we don't put must effort to format complex postconditions:
        (PostCond, fnpost, [code [htxt $ prettyRule qp rule]])

  showSpec fnspec qp qn rule = case rule of
   CRule _ (CSimpleRhs _ _) ->
     let (lhs,rhs) = break (=='=') (prettyRule qn rule)
      in (SpecFun, fnspec, [code [htxt $ "(" ++ stripSpaces lhs ++ ")"],
                            italic [htxt " is equivalent to "],
                            code [htxt (safeTail rhs)]])
   _ -> -- we don't put must effort to format complex specifications:
        (SpecFun, fnspec, [code [htxt $ prettyRule qp rule]])

  prettyWith ppfun = pretty 78 . ppfun prettyOpts
  prettyRule qn rl = pretty 78 (ppCRule prettyOpts qn rl)
  prettyOpts       = setNoQualification defaultOptions

  safeTail xs = if null xs then xs else tail xs

  isPropFuncDef (sline,_) =
    case sline of FuncDef fn -> fn `elem` propNames
                  _          -> False


--- Translate a documentation comment to HTML and use markdown translation
--- if necessary
--- @return: either a paragraph (`<p>`) element or an empty list.
docComment2HTML :: DocOptions -> String -> [HtmlExp]
docComment2HTML opts cmt
  | null cmt          = []
  | withMarkdown opts = markdownText2HTML (replaceIdLinks cmt)
  | otherwise         = [par [HtmlText (replaceIdLinks cmt)]]

-- replace identifier hyperlinks in a string (i.e., enclosed in single quotes)
-- by HTML hyperrefences:
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

  checkId s =
    if ' ' `elem` s
    then '\'' : s ++ ['\'']
    else let (md,dotfun) = break (=='.') s
          in "<code><a href=\"" ++
             (if null dotfun then '#':s else md++".html#"++tail dotfun) ++
             "\">"++s++"</a></code>"

-- generate HTML index for all exported names:
genHtmlExportIndex :: [String] -> [String] -> [String] -> [String] -> HtmlExp
genHtmlExportIndex exptypes expcons expfields expfuns =
  HtmlStruct "ul" [("class","nav nav-sidebar")]
    (concatMap (\ (htmlnames,cattitle) ->
                 if null htmlnames
                 then []
                 else HtmlStruct "li" [("class","nav-header")] [htxt cattitle] :
                      map (HtmlStruct "li" []) htmlnames)
            [(htmltypes,"Datatypes:"),
             (htmlcons ,"Constructors:"),
             (htmlfields,"Fields:"),
             (htmlfuns ,"Operations:")])
 where
  htmltypes  = map (\n->[href ('#':n) [htxt n]])
                   (nub (sortStrings exptypes))
  htmlcons   = map (\n->[href ('#':n++"_CONS") [htxt n]])
                   (nub (sortStrings expcons))
  htmlfields = map (\n->[href ('#':n++"_FIELD") [htxt n]])
                  (nub (sortStrings expfields))
  htmlfuns   = map (\n->[href ('#':n) [htxt n]])
                   (nub (sortStrings expfuns))

tName :: CTypeDecl -> String
tName = snd . typeName

fName :: CFuncDecl -> String
fName = snd . funcName

cName :: CConsDecl -> String
cName = snd . consName

fldName :: CFieldDecl -> String
fldName (CField (_,name) _ _) = name

isExportedType :: CTypeDecl -> Bool
isExportedType = (== Public) . typeVis

isExportedCons :: CConsDecl -> Bool
isExportedCons = (== Public) . consVis

isExportedFun :: CFuncDecl -> Bool
isExportedFun = (== Public) . funcVis

isExportedField :: CFieldDecl -> Bool
isExportedField (CField _ vis _) = vis == Public

-- extract the names of all exported constructors
getExportedCons :: [CTypeDecl] -> [String]
getExportedCons = map cName . filter isExportedCons . concatMap typeCons

-- extract the names of all exported fields
getExportedFields :: [CTypeDecl] -> [String]
getExportedFields = map fldName . filter isExportedField . concatMap getFields
                  . concatMap typeCons
 where
  getFields (CCons   _ _ _ ) = []
  getFields (CRecord _ _ fs) = fs

-- Is a function definition a property?
isProperty :: CFuncDecl -> Bool
isProperty fdecl = fst (funcName fdecl) /= easyCheckModule
                   && isPropType (funcType fdecl)
 where
  isPropType :: CTypeExpr -> Bool
  isPropType ct =  ct == baseType (easyCheckModule,"Prop") -- I/O test?
                || ct == baseType (propModule,"Prop") -- I/O test?
                || resultType ct == baseType (easyCheckModule,"Prop")
                || resultType ct == baseType (propModule,"Prop")

  easyCheckModule = "Test.EasyCheck" 
  propModule      = "Test.Prop" 

-- Is a function definition part of a specification, i.e.,
-- a full specification (suffix 'spec), a precondition (suffix 'pre),
-- or a postcondition (suffix 'post)?
isSpecFunc :: CFuncDecl -> Bool
isSpecFunc fdecl =
  let rfname = reverse (snd (funcName fdecl))
   in any (`isPrefixOf` rfname) ["ceps'","erp'","tsop'"]

--- generate HTML documentation for a module:
genHtmlModule :: DocOptions -> String -> [HtmlExp]
genHtmlModule docopts modcmts =
  let (maincmt,avcmts) = splitComment modcmts
   in docComment2HTML docopts maincmt ++
      map (\a->par [bold [htxt "Author: "], htxt a])
          (getCommentType "author" avcmts) ++
      map (\a->par [bold [htxt "Version: "], htxt a])
          (getCommentType "version" avcmts)

ulistOrEmpty :: [[HtmlExp]] -> [HtmlExp]
ulistOrEmpty items | null items = []
                   | otherwise  = [ulist items]

-- generate the html documentation for given comments ("param", "return",...)
ifNotNull :: [a] -> ([a] -> [b]) -> [b]
ifNotNull cmt genDoc
  | null cmt  = []
  | otherwise = genDoc cmt

--- generate HTML documentation for a datatype if it is exported:
genHtmlType :: DocOptions -> [(SourceLine,String)] -> CTypeDecl -> [HtmlExp]
genHtmlType docopts progcmts (CType (_,tcons) _ tvars constrs) =
  let (datacmt,consfldcmts) = splitComment (getDataComment tcons progcmts)
   in    [ anchored tcons [style "typeheader" [htxt tcons]] ]
      ++ docComment2HTML docopts datacmt
      ++ [par [explainCat "Constructors:"]]
      ++ ulistOrEmpty (map (genHtmlCons docopts consfldcmts tcons tvars fldCons)
                           (filter isExportedCons constrs))
      ++ [hrule]
 where
  expFields = [f | CRecord _ _ fs <- constrs, f <- fs, isExportedField f]
  fldCons   = [ (fn,cn) | f@(CField (_,fn) _ _) <- expFields
              , CRecord (_,cn) _ fs <- constrs, f `elem` fs
              ]
genHtmlType docopts progcmts (CTypeSyn (tcmod,tcons) _ tvars texp) =
  let (typecmt,_) = splitComment (getDataComment tcons progcmts)
   in    [ anchored tcons [style "typeheader" [htxt tcons]] ]
      ++ docComment2HTML docopts typecmt
      ++ [ par [explainCat "Type synonym:"
         , nbsp
         ,
            if tcons=="String" && tcmod=="Prelude"
            then code [htxt "String = [Char]"]
            else code [HtmlText
                        (tcons ++ concatMap (\(i,_) -> [' ',chr (97+i)]) tvars ++
                         " = " ++ showType tcmod False texp)]]
         , hrule
         ]
genHtmlType docopts progcmts t@(CNewType (_,tcons) _ tvars constr) =
  let (datacmt,consfldcmts) = splitComment (getDataComment tcons progcmts)
   in if isExportedCons constr
        then    [anchored tcons [style "typeheader" [htxt tcons]] ]
             ++ docComment2HTML docopts datacmt
             ++ [par [explainCat "Constructor:"]]
             ++ ulistOrEmpty [genHtmlCons docopts consfldcmts tcons tvars fldCons constr]
             ++ [hrule]
        else []
 where
  cn      = cName constr
  fldCons = map (\fn -> (fn,cn)) (getExportedFields [t])

--- generate HTML documentation for a constructor if it is exported:
genHtmlCons :: DocOptions -> [(String,String)] -> String -> [CTVarIName]
             -> [(String,String)] -> CConsDecl -> [HtmlExp]
genHtmlCons docopts consfldcmts tcons tvars _
            (CCons (cmod,cname) _ argtypes) =
    anchored (cname ++ "_CONS")
      [code [opnameDoc [htxt cname],
             HtmlText (" :: " ++
                       concatMap (\t -> " "++showType cmod True t++" -> ")
                                 argtypes ++
                       tcons ++ concatMap (\(i,_) -> [' ',chr (97+i)]) tvars)]] :
      maybe []
            (\ (_,cmt) -> htxt " : " : removeTopPar (docComment2HTML docopts
                                                    (removeDash cmt)))
            (getConsComment conscmts cname)
 where
  conscmts = getCommentType "cons" consfldcmts
genHtmlCons docopts consfldcmts tcons tvars fldCons
            (CRecord (cmod,cname) _ fields) =
    anchored (cname ++ "_CONS")
      [code [opnameDoc [htxt cname],
             HtmlText (" :: " ++
                       concatMap (\t -> " "++showType cmod True t++" -> ")
                                 argtypes ++
                       tcons ++ concatMap (\(i,_) -> [' ',chr (97+i)]) tvars)]] :
      (maybe []
            (\ (_,cmt) -> htxt " : " : removeTopPar (docComment2HTML docopts
                                                    (removeDash cmt)))
            (getConsComment conscmts cname)) ++
      par [explainCat "Fields:"] :
      ulistOrEmpty (map (genHtmlField docopts fldcmts cname fldCons)
                        (filter isExportedField fields))
 where
  argtypes = map (\(CField _ _ t) -> t) fields
  conscmts = getCommentType "cons" consfldcmts
  fldcmts  = getCommentType "field" consfldcmts

-- generate HTML documentation for record fields
genHtmlField :: DocOptions -> [String] -> String -> [(String,String)]
             -> CFieldDecl -> [HtmlExp]
genHtmlField docopts fldcmts cname fldCons (CField (fmod,fname) _ ty)
  | withAnchor fname = [anchored (fname ++ "_FIELD") html]
  | otherwise        = html
 where
  withAnchor f = maybe False (== cname) (lookup f fldCons)
  html         = [ code [opnameDoc [htxt fname]
                 , HtmlText (" :: " ++ showType fmod True ty)]
                 ] ++ maybe []
                            (\ (_,cmt) -> htxt " : " : removeTopPar
                               (docComment2HTML docopts (removeDash cmt)))
                            (getConsComment fldcmts fname)

-- generate short HTML documentation for a function:
genHtmlFuncShort :: DocOptions -> [(SourceLine,String)] -> AnaInfo -> CFuncDecl
                 -> [[HtmlExp]]
genHtmlFuncShort docopts progcmts anainfo
                 (CFunc (fmod,fname) _ _ ftype _) =
 [[code [opnameDoc
            [anchored (fname ++ "_SHORT")
                      [href ('#':fname) [htxt (showId fname)]]],
         HtmlText (" :: " ++ showType fmod False ftype)],
     nbsp, nbsp]
     ++ genFuncPropIcons anainfo (fmod,fname) ++
  [breakline] ++
   removeTopPar
      (docComment2HTML docopts
         (firstSentence (fst (splitComment
                                (getFuncComment fname progcmts)))))]
genHtmlFuncShort docopts progcmts anainfo (CmtFunc _ n a vis ftype rules) =
  genHtmlFuncShort docopts progcmts anainfo (CFunc n a vis ftype rules)

-- generate HTML documentation for a function:
genHtmlFunc :: DocOptions -> String -> [(SourceLine,String)]
            -> [(String,[(FuncAttachment,String,[HtmlExp])])] -> AnaInfo
            -> [COpDecl] -> CFuncDecl -> HtmlExp
genHtmlFunc docopts modname progcmts funcattachments anainfo ops
            (CmtFunc _ n a vis ftype rules) =
  genHtmlFunc docopts modname progcmts funcattachments anainfo ops
              (CFunc n a vis ftype rules)
genHtmlFunc docopts modname progcmts funcattachments anainfo ops
            (CFunc (fmod,fname) _ _ ftype rules) =
  let (funcmt,paramcmts) = splitComment (getFuncComment fname progcmts)
   in anchoredDiv fname
       [borderedTable [[
         [par $
           [code [opnameDoc [showCodeHRef fname],
                  HtmlText (" :: "++ showType fmod False ftype)],
            nbsp, nbsp] ++
           genFuncPropIcons anainfo (fmod,fname)] ++
         docComment2HTML docopts funcmt ++
         genParamComment paramcmts ++
         -- show contracts and properties (if present):
         showAttachments "Precondition"  PreCond  ++
         showAttachments "Postcondition" PostCond ++
         showAttachments "Specification" SpecFun  ++
         showAttachments "Properties"    Property ++
         -- show further infos for this function, if present:
         (if furtherInfos == []
          then []
          else [dlist [([explainCat "Further infos:"],
                        [ulist furtherInfos])]] )]]]
 where
  showCodeHRef fn = href (modname++"_curry.html#"++fn) [htxt (showId fn)]

  showAttachments aname attachkind =
   let attachfuns = filter (\ (k,_,_) -> k==attachkind)
                           (maybe [] id (lookup fname funcattachments))
    in if null attachfuns then [] else
        [dlist [([explainCat (aname++":")],
                 [par (intercalate [breakline]
                         (map (\ (_,pn,pc) -> pc ++
                                 [nbsp, htxt "(", showCodeHRef pn, htxt ")"])
                              attachfuns))])]]

  furtherInfos = genFuncPropComments anainfo (fmod,fname) rules ops

  genParamComment paramcmts =
    let params = map (span isIdChar) (getCommentType "param" paramcmts)
        ret    = getCommentType "return" paramcmts
     in  ifNotNull params (\parCmts ->
          [ dlist [([explainCat "Example call:"],
                    [code [htxt (showCall fname (map fst params))]])
                  ]
          , dlist [([explainCat "Parameters:"],
                    [ulist (map (\ (parid,parcmt) ->
                           [code [htxt parid], htxt " : "] ++
                           removeTopPar (docComment2HTML docopts
                                                         (removeDash parcmt)))
                       parCmts)])]
          ])
      ++ ifNotNull ret (\retCmt -> [dlist (map (\rescmt ->
          ([explainCat "Returns:"],
           removeTopPar (docComment2HTML docopts rescmt))) retCmt)])

  showCall f params =
    if isAlpha (head f) || length params /= 2
    then "(" ++ showId f ++ concatMap (" "++) params ++ ")"
    else "(" ++ params!!0 ++ " " ++ f ++ " " ++ params!!1 ++ ")"

-- remove initial dash sign (of a parameter comment)
removeDash :: String -> String
removeDash s = let ds = dropWhile isSpace s in
  if take 2 ds == "- " then dropWhile isSpace (drop 2 ds)
                       else ds

-- remove a single top-level paragraph in HTML expressions:
removeTopPar :: [HtmlExp] -> [HtmlExp]
removeTopPar hexps = case hexps of
  [HtmlStruct "p" [] hs] -> hs
  _ -> hexps

--------------------------------------------------------------------------
--- Generates icons for particular properties of functions.
genFuncPropIcons :: AnaInfo -> QName -> [HtmlExp]
genFuncPropIcons anainfo fname =
   [detPropIcon, nbsp]
 where
   --(non)deterministically defined property:
   detPropIcon =
    if getNondetInfo anainfo fname
    then href "index.html#nondet_explain" [nondetIcon]
    else href "index.html#det_explain"    [detIcon]

--------------------------------------------------------------------------
--- Generates further textual infos about particular properties
--- of a function. The result is a list of HTML expressions to be
--- formatted (if not empty) as some HTML list.
genFuncPropComments :: AnaInfo -> QName -> [CRule] -> [COpDecl] -> [[HtmlExp]]
genFuncPropComments anainfo fname rules ops =
   filter (/=[]) [genFixityInfo fname ops,
                  completenessInfo,
                  indeterminismInfo,
                  opcompleteInfo,
                  externalInfo rules]
 where
   -- comment about the definitional completeness of a function:
   completenessInfo = let ci = getCompleteInfo anainfo fname in
     if ci == Complete
     then []
     else [htxt (if ci == InComplete
                 then "partially defined"
                 else
             "partially defined in each disjunction (but might be complete)")]

   -- comment about the indeterminism of a function:
   indeterminismInfo = if getIndetInfo anainfo fname
                       then [htxt "might behave indeterministically"]
                       else []

   -- comment about the indeterminism of a function:
   opcompleteInfo =
      if getOpCompleteInfo anainfo fname
      then [htxt "solution complete, i.e., able to compute all solutions"]
      else []

   -- comment about the external definition of a function:
   externalInfo []    = [htxt "externally defined"]
   externalInfo (_:_) = []


--- Generates a comment about the associativity and precedence
--- if the name is defined as an infix operator.
genFixityInfo :: QName -> [COpDecl] -> [HtmlExp]
genFixityInfo fname ops =
    concatMap (\(COp n fix prec)->
                  if n == fname
                  then [htxt ("defined as "++showFixity fix++
                              " infix operator with precedence "++show prec)]
                  else [])
              ops
 where
  showFixity CInfixOp  = "non-associative"
  showFixity CInfixlOp = "left-associative"
  showFixity CInfixrOp = "right-associative"


--------------------------------------------------------------------------
-- Pretty printer for types in Curry syntax:
-- second argument is True iff brackets must be written around complex types
showType :: String -> Bool -> CTypeExpr -> String
showType _ _ (CTVar (i,_)) = [chr (97+i)] -- TODO: use name given in source program instead?
showType mod nested (CFuncType t1 t2) =
   brackets nested
    (showType mod (isFunctionalType t1) t1 ++ " -&gt; " ++ showType mod False t2)
showType mod nested (CTCons tc ts)
 | ts==[]  = showTypeCons mod tc
 | tc==("Prelude","[]") && (head ts == CTCons ("Prelude","Char") [])
   = "String"
 | tc==("Prelude","[]")
   = "[" ++ showType mod False (head ts) ++ "]" -- list type
 | take 2 (snd tc) == "(,"                      -- tuple type
   = "(" ++ concat (intersperse "," (map (showType mod False) ts)) ++ ")"
 | otherwise
   = brackets nested
      (showTypeCons mod tc ++ " " ++
       concat (intersperse " " (map (showType mod True) ts)))

showTypeCons :: String -> QName -> String
showTypeCons mod (mtc,tc) =
  if mtc == "Prelude"
  then tc --"<a href=\"Prelude.html#"++tc++"\">"++tc++"</a>"
  else
    if mod == mtc
    then "<a href=\"#"++tc++"\">"++tc++"</a>"
    else "<a href=\""++mtc++".html#"++tc++"\">"++tc++"</a>"


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
               allConsFuncsMenu (indexPage modnames)
     >>= writeFile (docdir++"/index.html")
 where
  pageTitle =
    if not (null (mainTitle docopts))
      then [htxt (mainTitle docopts)]
      else if length modnames == 1
            then [htxt "Documentation of the Curry program ",
                  href (head modnames ++ ".html")
                       [htxt (head modnames ++ ".curry")]]
            else [htxt "Documentation of Curry programs"]

allConsFuncsMenu :: [[HtmlExp]]
allConsFuncsMenu =
  [[href "findex.html" [htxt "All operations"]],
   [href "cindex.html" [htxt "All constructors"]]]

indexPage :: [String] -> [HtmlExp]
indexPage modnames =
  (if null modnames
   then []
   else [ulist (map (\m->[href (m++".html") [htxt (m++".curry ")]])
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
genFunctionIndexPage :: String -> [FC.FuncDecl] -> IO ()
genFunctionIndexPage docdir funs = do
  putStrLn ("Writing operation index page to \""++docdir++"/findex.html\"...")
  simplePage "Index to all operations" Nothing allConsFuncsMenu
             (htmlFuncIndex (sortNames expfuns))
    >>= writeFile (docdir++"/findex.html")
 where
   expfuns = map FCG.funcName $ filter ((== FC.Public) . FCG.funcVisibility) funs

htmlFuncIndex :: [QName] -> [HtmlExp]
htmlFuncIndex qnames = categorizeByItemKey (map showModNameRef qnames)
   
showModNameRef :: QName -> (String,[HtmlExp])
showModNameRef (modname,name) =
  (name,
   [href (modname++".html#"++name) [htxt name], nbsp, nbsp,
    htxt "(", href (getLastName modname++".html") [htxt modname], htxt ")"]
  )

sortNames :: [(a,String)] -> [(a,String)]
sortNames names = mergeSortBy (\(_,n1) (_,n2)->leqStringIgnoreCase n1 n2) names


--------------------------------------------------------------------------
-- generate the constructor index page for the documentation directory:
genConsIndexPage :: String -> [FC.TypeDecl] -> IO ()
genConsIndexPage docdir types = do
  putStrLn ("Writing constructor index page to \""++docdir++"/cindex.html\"...")
  simplePage "Index to all constructors" Nothing allConsFuncsMenu
             (htmlConsIndex (sortNames expcons))
    >>= writeFile (docdir++"/cindex.html")
 where
   consDecls (FC.Type    _ _ _ cs) = cs
   consDecls (FC.TypeSyn _ _ _ _ ) = []
   expcons = map FCG.consName $ filter ((== FC.Public) . FCG.consVisibility) $
     concatMap consDecls types

htmlConsIndex :: [QName] -> [HtmlExp]
htmlConsIndex qnames = categorizeByItemKey (map showModNameRef qnames)

--------------------------------------------------------------------------
-- generate the index page categorizing all system libraries of PAKCS/KICS2
genSystemLibsPage :: String -> [Category] -> [[ModInfo]] -> IO ()
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

syslibsSideMenu :: [Category] -> [HtmlExp]
syslibsSideMenu cats = map par $
     [[ehref currygleURL [extLinkIcon, htxt " Search with Curr(y)gle"]]]
  ++ [[href ("#" ++ genCatLink c) [ htxt (showCategory c)]] | c <- cats]
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

-- Generate links for a category (system libraries page)
genCatLink :: Category -> String
genCatLink cat = getCategoryID cat

genHtmlLibCats :: [[ModInfo]] -> [HtmlExp]
genHtmlLibCats = concatMap gen
  where
  gen []              = []
  gen cat@((c,_,_):_) = [anchoredSection (getCategoryID c)
                        (h2 [htxt (showCategory c ++ ":")] : genHtmlLibCat cat)]

genHtmlLibCat :: [ModInfo] -> [HtmlExp]
genHtmlLibCat category =
  [dlist [(genHtmlName modname, docComment2HTML defaultCurryDocOptions modcmt)
  | (_,modname,modcmt) <- category ]
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
anchoredDiv tag doc = block doc `addAttr` ("class", "anchored")
                                `addAttr` ("id",tag)

--- A bordered table:
borderedTable :: [[[HtmlExp]]] -> HtmlExp
borderedTable rows = table rows `addClass` "table table-bordered table-hover"

--- An external reference
ehref :: String -> [HtmlExp] -> HtmlExp
ehref url desc = href url desc `addAttr` ("target","_blank")

--------------------------------------------------------------------------
-- auxiliaries:

stripSpaces :: String -> String
stripSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- style for explanation categories, like "Constructors:", "Parameters:",...
explainCat :: String -> HtmlExp
explainCat s = textstyle "explaincat" s

-- style for function/constructor name shown in the documentation part:
opnameDoc :: [HtmlExp] -> HtmlExp
opnameDoc = style "opname"

-- Sorts a list of strings.
sortStrings :: [String] -> [String]
sortStrings strings = mergeSortBy leqStringIgnoreCase strings

-- Returns the first sentence in a string:
firstSentence :: String -> String
firstSentence s = let (fs,ls) = break (=='.') s in
  if null ls
  then fs
  else if tail ls /= "" && isWhiteSpace (head (tail ls))
       then fs ++ "."
       else fs ++ "." ++ firstSentence (tail ls)

firstPassage :: String -> String
firstPassage = unlines . takeWhile (\s -> s /= "" && not (all isWhiteSpace s))
             . lines

--------------------------------------------------------------------------
