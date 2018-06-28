module CurryDoc.Info.Comments
  (readComments, associateCurryDoc,
   isCommentedTypeSig, isCommentedInstanceDecl,
   splitNestedComment, commentString,
   commentedDeclName, instTypeName,
   Comment(..),
   CommentedDecl(..),
   CommentedConstr(..), CommentedNewtypeConstr(..),
   CommentedField) where

import CurryDoc.Data.Span
import CurryDoc.Data.SpanInfo
import CurryDoc.Data.Type
import CurryDoc.Data.AnaInfo
import CurryDoc.Info.Goodies

import AbstractCurry.Types
import AbstractCurry.Select

import Char            (isSpace)
import Maybe           (listToMaybe, mapMaybe)
import List            (partition, init, last, isPrefixOf)
import Directory       (doesFileExist)
import FileGoodies     (getFileInPath, lookupFileInPath)
import FilePath        (takeFileName, (</>), (<.>))
import Distribution    ( FrontendParams, FrontendTarget (..), defaultParams
                       , setQuiet, inCurrySubdir, stripCurrySuffix
                       , callFrontend, callFrontendWithParams
                       , lookupModuleSourceInLoadPath, getLoadPathForModule
                       )

data Comment = NestedComment String
             | LineComment   String
  deriving (Eq, Ord, Read, Show)

data CDocComment = Pre  { comment :: Comment }
                 | Post { comment :: Comment }
                 | None { comment :: Comment }
  deriving Show

data CommentedDecl
  = CommentedTypeDecl QName [CTVarIName] CTypeExpr [Comment]
  | CommentedDataDecl QName [CTVarIName] [Comment] [CommentedConstr]
  | CommentedNewtypeDecl QName [CTVarIName] [Comment] (Maybe CommentedNewtypeConstr)
  | CommentedClassDecl QName CContext CTVarIName [Comment] [CommentedDecl]
  | CommentedInstanceDecl QName CContext CTypeExpr [Comment] [CommentedDecl]
  | CommentedFunctionDecl QName [Comment] (Maybe CQualTypeExpr) AnalysisInfo
  | CommentedTypeSig [QName] [Comment] CContext [(CTypeExpr, [Comment])]
  | CommentedExternalDecl [QName] [Comment] (Maybe CQualTypeExpr) AnalysisInfo
  | UnsupportedDecl [Comment]
  deriving Show

data CommentedConstr
  = CommentedConstr QName [Comment] [CTypeExpr] AnalysisInfo
  | CommentedRecord QName [Comment] [CTypeExpr] [CommentedField] AnalysisInfo
  | CommentedConsOp QName [Comment] CTypeExpr CTypeExpr AnalysisInfo
  deriving Show

type CommentedField = ([QName], [Comment], CTypeExpr) -- TODO: can have fixity

data CommentedNewtypeConstr
  = CommentedNewConstr QName [Comment] CTypeExpr AnalysisInfo
  | CommentedNewRecord QName [Comment] CTypeExpr (Maybe CommentedField) AnalysisInfo
  deriving Show

-- Reads the comments from a specified module
readComments :: String -> IO [(Span, Comment)]
readComments progname =
   readCommentsWithParseOptions progname (setQuiet True defaultParams)

readCommentsWithParseOptions :: String -> FrontendParams -> IO [(Span, Comment)]
readCommentsWithParseOptions progname options = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find Comments file in load path:
      loadpath <- getLoadPathForModule progname
      filename <- getFileInPath (commentsFileName (takeFileName progname)) [""]
                                loadpath
      readCommentsFile filename
    Just (dir,_) -> do
      callFrontendWithParams COMMS options progname
      readCommentsFile (commentsFileName (dir </> takeFileName progname))

commentsFileName :: String -> String
commentsFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "cycom"

readCommentsFile :: String -> IO [(Span, Comment)]
readCommentsFile filename = do
  filecontents <- readCommentsFileRaw filename
  return (read filecontents)

readCommentsFileRaw :: String -> IO String
readCommentsFileRaw filename = do
  extfcy <- doesFileExist filename
  if extfcy
   then readFile filename
   else do let subdirfilename = inCurrySubdir filename
           exdirtfcy <- doesFileExist subdirfilename
           if exdirtfcy
            then readFile subdirfilename
            else error ("EXISTENCE ERROR: Comment file '" ++ filename ++
                        "' does not exist")

-- Associates given comments with declarations from given module
-- based on the source code positions
associateCurryDoc :: [(Span, Comment)] -> Module a -> ([CommentedDecl], [Comment])
associateCurryDoc []       _                       = ([], [])
associateCurryDoc xs@(_:_) (Module spi _ _ _ _ ds) =
  let (result, rest) = associateCurryDocHeader spi sp xs'
  in  (cleanup $ merge $ associateCurryDocDecls rest ds Nothing, result)
  where xs' = map (\(sp',c) -> (sp', classifyComment c)) xs
        sp = case ds of
          (d:_) -> getSrcSpan d
          _     -> NoSpan

associateCurryDocHeader :: SpanInfo                           -- ^ module SpanInfo
                        -> Span                               -- ^ first decl span
                        -> [(Span, CDocComment)]              -- ^ to be matched
                        -> ([Comment], [(Span, CDocComment)]) -- ^ (matched, rest)
associateCurryDocHeader spi@(SpanInfo _ (spm : ss)) sp (c:cs) =
  case c of
    (sp', Pre  c')
      | vertDist sp' spm >= 0 ->
        let (match, next)   = getToMatch spm sp' cs isPre
            (matched, rest) = associateCurryDocHeader spi sp next
        in (c' : ((map (comment . snd) match) ++ matched), rest)
      | otherwise             ->
        let (matched, rest) = associateCurryDocHeader spi sp cs
        in (matched, c:rest)

    (sp', Post c')
      | (vertDist sp' sp  >= 1
          || isNoSpan sp) &&
        isAfter sp' (last ss) ->
          let (match, next)   = getToMatch sp sp' cs isPost
              (matched, rest) = associateCurryDocHeader spi sp next
          in (c' : ((map (comment . snd) match) ++ matched), rest)
      | otherwise             ->
          ([], c:cs)

    (_  , None _)             ->
          associateCurryDocHeader spi sp cs
associateCurryDocHeader (SpanInfo _ []) _ (c:cs) = ([], c:cs)
associateCurryDocHeader NoSpanInfo      _ (c:cs) = ([], c:cs)
associateCurryDocHeader _               _ []     = ([], [])

associateCurryDocDecls :: [(Span, CDocComment)]
                       -> [Decl a]
                       -> Maybe (Decl a)
                       -> [CommentedDecl]
associateCurryDocDecls []               _      _    = []
associateCurryDocDecls (c         : cs) []     prev = matchLast (c:cs) prev
associateCurryDocDecls ((sp, cdc) : cs) (d:ds) prev =
  case cdc of
    Pre  _ | vertDist sp spd >= 0 ->
               let (match, next) = getToMatch spd sp cs isPre
               in  associateCurryDocDeclPre ((sp, cdc) : match) d
                     : associateCurryDocDecls next (d:ds) (Just d)
           | otherwise ->
               associateCurryDocDecls ((sp, cdc) : cs) ds (Just d)

    Post _ | vertDist sp spd >= 1 ->
               case prev of
                 Nothing -> associateCurryDocDecls cs (d:ds) prev
                 Just d' -> let (match, next) = getToMatch spd sp cs isPost
                            in  associateCurryDocDeclPost ((sp, cdc) : match) d'
                                  : associateCurryDocDecls next (d:ds) prev
           | vertDist sp spd == 0 ->
               let (match, next) = getToMatch spd sp cs isPost
               in  associateCurryDocDeclPost ((sp, cdc) : match) d
                     : associateCurryDocDecls next (d:ds) prev
           | otherwise ->
               associateCurryDocDecls ((sp, cdc) : cs) ds (Just d)

    None _ -> associateCurryDocDecls cs (d:ds) prev
  where spd  = getSrcSpan d

getToMatch :: Span                  -- ^ until
           -> Span                  -- ^ last undiscarded comment span
           -> [(Span, CDocComment)] -- ^ next comments
           -> (CDocComment -> Bool) -- ^ predicate to test for right comment type
           -> ([(Span, CDocComment)], [(Span, CDocComment)])
getToMatch _    _    []             _ = ([], [])
getToMatch stop last ((sp, c) : cs) p =
  if (sp `isBefore` stop || isNoSpan stop)           -- pos is ok
       && (p c || (isNone c && vertDist last sp <= 1))  -- CDocType is ok
    then add (sp, c) (getToMatch stop sp cs p)
    else ([], (sp, c) : cs)
  where add x (xs, rest) = (x:xs, rest)

matchLast :: [(Span, CDocComment)]
          -> Maybe (Decl a)
          -> [CommentedDecl]
matchLast []                  (Just _) = []
matchLast _                   Nothing  = []
matchLast ((sp, Post c) : cs) (Just d) =
  let (match, next) = getToMatch (getSrcSpan d) sp cs isPost
  in  associateCurryDocDeclPost ((sp, Post c) : match) d
        : matchLast next (Just d)
matchLast ((_ , None _) : cs) (Just d) = matchLast cs (Just d)
matchLast ((_ , Pre  _) : cs) (Just d) = matchLast cs (Just d)

associateCurryDocDeclPre :: [(Span, CDocComment)]
                         -> Decl a
                         -> CommentedDecl
associateCurryDocDeclPre xs d@(FunctionDecl _ _ f _) =
  let (match, _) = getToMatch (getSrcSpan d) NoSpan xs isPre
  in  CommentedFunctionDecl (identToQName f) (map (comment . snd) match)
                            Nothing NoAnalysisInfo
associateCurryDocDeclPre xs d@(ExternalDecl _ fs) =
  let (match, _) = getToMatch (getSrcSpan d) NoSpan xs isPre
  in  CommentedExternalDecl (map (\(Var _ i) -> identToQName i) fs)
                            (map (comment . snd) match)
                            Nothing NoAnalysisInfo
associateCurryDocDeclPre xs d@(TypeDecl _ f vs ty) =
  let (match, _) = getToMatch (getSrcSpan d) NoSpan xs isPre
  in  CommentedTypeDecl (identToQName f) (map tvIName vs)
                        (typeExprToCType ty) (map (comment . snd) match)
associateCurryDocDeclPre xs (ClassDecl spi cx f v ds) =
  let (result, rest) = associateCurryDocHeader spi sp xs
      sp             = case ds of
                         (d:_) -> getSrcSpan d
                         _     -> NoSpan
  in  CommentedClassDecl (identToQName f)(contextToCContext cx) (tvIName v)
                          result (associateCurryDocDecls rest ds Nothing)
associateCurryDocDeclPre xs (InstanceDecl spi cx f ty ds) =
  let (result, rest) = associateCurryDocHeader spi sp xs
      sp             = case ds of
                         (d:_) -> getSrcSpan d
                         _     -> NoSpan
  in  CommentedInstanceDecl (qIdentToQName f) (contextToCContext cx)
                            (typeExprToCType ty) result
                            (associateCurryDocDecls rest ds Nothing)
associateCurryDocDeclPre xs d@(NewtypeDecl _ f vs c _) =
  let (match, rest ) = getToMatch (getSrcSpan d) NoSpan xs isPre
      (cons,  rest') = getToMatch (getSrcSpan c) NoSpan rest isPre
      ccon           = case c of
        NewConstrDecl _ cn ty
          -> CommentedNewConstr (identToQName cn) (map (comment . snd) cons)
                                (typeExprToCType ty) NoAnalysisInfo
        NewRecordDecl spi cn (idt, ty) ->
          let SpanInfo _ (sp:_) = spi -- sp = '{'
              [field]           = matchFieldsPre [FieldDecl NoSpanInfo [idt] ty]
                                                 (skipUntilAfter sp rest')
          in  CommentedNewRecord (identToQName cn) (map (comment . snd) cons)
                                 (typeExprToCType ty) (Just field)
                                 NoAnalysisInfo
  in CommentedNewtypeDecl (identToQName f)  (map tvIName vs)
                          (map (comment . snd) match) (Just ccon)
associateCurryDocDeclPre xs d@(DataDecl _ f vs [] _) =
  let (match, _) = getToMatch (getSrcSpan d) NoSpan xs isPre
  in  CommentedDataDecl (identToQName f) (map tvIName vs)
                        (map (comment . snd) match) []
associateCurryDocDeclPre xs d@(DataDecl spi f vs (c:cs) _) =
  let (match, rest) = getToMatch (getSrcSpan d) NoSpan xs isPre
      sp = case spi of
        SpanInfo _ (_:sp':_)   -> sp' -- throw away everything until '='
        _                      -> error ("Comment.associateCurryDocDeclPre: "
                                         ++ show spi)
  in CommentedDataDecl (identToQName f) (map tvIName vs)
                       (map (comment . snd) match)
                       (matchConstructorsPre (c:cs) (skipUntilAfter sp rest))
associateCurryDocDeclPre xs d@(TypeSig _ fs
  (QualTypeExpr (SpanInfo _ (s:ss)) cx ty)) =
  let (match, rest) = getToMatch (getSrcSpan d) NoSpan xs isPre
      sp = last (s:ss) -- throw away everything until '=>'
  in  CommentedTypeSig (map identToQName fs) (map (comment . snd) match)
                       (contextToCContext cx)
                       (matchArgumentsPre ty (skipUntilAfter sp rest))
associateCurryDocDeclPre xs d@(TypeSig spi fs
  (QualTypeExpr (SpanInfo _ []) cx ty)) =
   let (match, rest) = getToMatch (getSrcSpan d) NoSpan xs isPre
       sp = case spi of
         SpanInfo _ [sp']   -> sp' -- throw away everything until '::'
         _                  -> error ("Comment.associateCurryDocDeclPre: "
                                       ++ show spi)
   in  CommentedTypeSig (map identToQName fs) (map (comment . snd) match)
                        (contextToCContext cx)
                        (matchArgumentsPre ty (skipUntilAfter sp rest))
associateCurryDocDeclPre xs (InfixDecl _ _ _ _) = UnsupportedDecl
  (map (comment . snd) xs)
associateCurryDocDeclPre xs (ExternalDataDecl _ _ _) = UnsupportedDecl
  (map (comment . snd) xs)
associateCurryDocDeclPre xs (DefaultDecl _ _) = UnsupportedDecl
  (map (comment . snd) xs)
associateCurryDocDeclPre _ (TypeSig _ _ (QualTypeExpr NoSpanInfo _ _)) =
  error "associateCurryDocDeclPre: NoSpanInfo in QualTypeExpr"

matchArgumentsPre :: TypeExpr -> [(Span, CDocComment)] -> [(CTypeExpr, [Comment])]
matchArgumentsPre ty cs = case ty of
  ArrowType _ ty1 ty2 ->
    let (match, rest) = getToMatch (getSrcSpan ty) NoSpan cs isPre
    in  (typeExprToCType ty1, map (comment . snd) match)
          : matchArgumentsPre ty2 (skipUntilAfter (getSrcSpan ty1) rest)
  _                   ->
    let (match, _) = getToMatch (getSrcSpan ty) NoSpan cs isPre
    in  [(typeExprToCType ty , map (comment . snd) match)]

matchConstructorsPre :: [ConstrDecl] -> [(Span, CDocComment)]
                     -> [CommentedConstr]
matchConstructorsPre []       _  = []
matchConstructorsPre (RecordDecl spi _ _ f fs:cns) cs =
  let SpanInfo stop (sp:_) = spi
      (match, rest)        = getToMatch stop NoSpan cs isPre
      fields               = matchFieldsPre fs (skipUntilAfter sp cs)
  in  CommentedRecord (identToQName f) (map (comment . snd) match)
                      (map fieldType fs) fields NoAnalysisInfo
        : matchConstructorsPre cns (skipUntilAfter stop rest)
matchConstructorsPre (ConstrDecl spi _ _ f tys :cns) cs =
  let stop          = getSrcSpan spi
      (match, rest) = getToMatch stop NoSpan cs isPre
  in  CommentedConstr (identToQName f) (map (comment . snd) match)
                      (map typeExprToCType tys) NoAnalysisInfo
        : matchConstructorsPre cns (skipUntilAfter stop rest)
matchConstructorsPre (ConOpDecl spi _ _ ty1 f ty2 :cns) cs =
  let stop          = getSrcSpan spi
      (match, rest) = getToMatch stop NoSpan cs isPre
  in  CommentedConsOp (identToQName f) (map (comment . snd) match)
                      (typeExprToCType ty1) (typeExprToCType ty2) NoAnalysisInfo
        : matchConstructorsPre cns (skipUntilAfter stop rest)

matchFieldsPre :: [FieldDecl] -> [(Span, CDocComment)] -> [CommentedField]
matchFieldsPre []                            _  = []
matchFieldsPre (f@(FieldDecl _ idts ty) : fs) cs =
  let (match, rest) = getToMatch (getSrcSpan f) NoSpan cs isPre
  in (map identToQName idts, map (comment . snd) match, typeExprToCType ty)
       : matchFieldsPre fs (skipUntilAfter (getSrcSpan f) rest)

associateCurryDocDeclPost :: [(Span, CDocComment)]
                          -> Decl a
                          -> CommentedDecl
associateCurryDocDeclPost xs d@(FunctionDecl _ _ f _) =
  CommentedFunctionDecl (identToQName f)
                        (map (comment . snd)
                             (skipUntilAfter (getSrcSpan d) xs))
                        Nothing NoAnalysisInfo
associateCurryDocDeclPost xs d@(ExternalDecl _ fs) =
  CommentedExternalDecl (map (\(Var _ i) -> identToQName i) fs)
                        (map (comment . snd)
                             (skipUntilAfter (getSrcSpan d) xs))
                        Nothing NoAnalysisInfo
associateCurryDocDeclPost xs d@(TypeDecl _ f idts ty) =
  CommentedTypeDecl (identToQName f) (map tvIName idts) (typeExprToCType ty)
                    (map (comment . snd) (skipUntilAfter (getSrcSpan d) xs))
associateCurryDocDeclPost xs (ClassDecl spi cx f n ds) =
  let (result, rest) = associateCurryDocHeader spi sp xs
  in  CommentedClassDecl (identToQName f) (contextToCContext cx) (tvIName n)
                         result (associateCurryDocDecls rest ds Nothing)
  where sp = case ds of
          (d:_) -> getSrcSpan d
          _     -> NoSpan
associateCurryDocDeclPost xs (InstanceDecl spi cx f ty ds) =
  let (result, rest) = associateCurryDocHeader spi sp xs
  in  CommentedInstanceDecl (qIdentToQName f) (contextToCContext cx)
                            (typeExprToCType ty) result
                            (associateCurryDocDecls rest ds Nothing)
  where sp = case ds of
          (d:_) -> getSrcSpan d
          _     -> NoSpan
associateCurryDocDeclPost xs (NewtypeDecl _ f vs c []) = --no deriving
  CommentedNewtypeDecl (identToQName f) (map tvIName vs) []
                       (Just (matchNewConstrPost c
                               (skipUntilAfter (getSrcSpan c) xs) xs))
associateCurryDocDeclPost xs (NewtypeDecl spi f vs c (_:_)) =
  let SpanInfo sp ss = spi -- otherwise something is wrong
      (match, rest)  = getToMatch (last ss) NoSpan
                         (skipUntilAfter (getSrcSpan c) xs) isPost
  in CommentedNewtypeDecl (identToQName f) (map tvIName vs)
                          (map (comment . snd) (skipUntilAfter sp rest))
                          (Just (matchNewConstrPost c match xs))
associateCurryDocDeclPost xs d@(DataDecl _ f vs [] _) = -- cannot have deriving
  CommentedDataDecl (identToQName f) (map tvIName vs)
                    (map (comment . snd) (skipUntilAfter (getSrcSpan d) xs)) []
associateCurryDocDeclPost xs (DataDecl _ f vs (c:cs) []) = -- no deriving
  CommentedDataDecl (identToQName f) (map tvIName vs) []
                    (matchConstructorsPost (c:cs) xs)
associateCurryDocDeclPost xs (DataDecl spi f vs (c:cs) (_:_)) =
  let SpanInfo sp ss = spi -- otherwise something is wrong
      (declC, consC) = partition ((`isAfter` sp) . fst) xs
      spDeriving     = ss !! (length cs + 2)
  in CommentedDataDecl (identToQName f) (map tvIName vs)
                       (map (comment . snd) declC)
                       (matchConstructorsPost (c:cs)
                         (filter ((`isBefore` spDeriving) . fst) consC))
associateCurryDocDeclPost xs (TypeSig _ fs (QualTypeExpr _ cx ty)) =
  CommentedTypeSig (map identToQName fs) [] (contextToCContext cx)
                   (matchArgumentsPost ty xs)
associateCurryDocDeclPost xs (ExternalDataDecl _ _ _) = UnsupportedDecl
  (map (comment . snd) xs)
associateCurryDocDeclPost xs (InfixDecl _ _ _ _) = UnsupportedDecl
  (map (comment . snd) xs)
associateCurryDocDeclPost xs (DefaultDecl _ _) = UnsupportedDecl
  (map (comment . snd) xs)

matchNewConstrPost :: NewConstrDecl -> [(Span, CDocComment)]
                   -> [(Span, CDocComment)] -> CommentedNewtypeConstr
matchNewConstrPost (NewConstrDecl _ cn ty) cs _ =
    CommentedNewConstr (identToQName cn)
                       (map (comment . snd) cs)
                       (typeExprToCType ty) NoAnalysisInfo
matchNewConstrPost (NewRecordDecl spiR cn (idt, ty)) cs xs =
    let SpanInfo _ ssR = spiR
        [f] = matchFieldsPost (last ssR) [FieldDecl NoSpanInfo [idt] ty]
                       (skipUntilAfter (getSrcSpan ty) xs)
    in CommentedNewRecord (identToQName cn) (map (comment . snd) cs)
                          (typeExprToCType ty) (Just f) NoAnalysisInfo

matchArgumentsPost :: TypeExpr -> [(Span, CDocComment)] -> [(CTypeExpr, [Comment])]
matchArgumentsPost ty cs = case ty of
  ArrowType spi ty1 ty2 ->
    let SpanInfo _ [sp] = spi -- arrow span
        (match, rest) = getToMatch sp NoSpan
                          (skipUntilAfter (getSrcSpan ty1) cs) isPost
    in  (typeExprToCType ty1, map (comment . snd) match)
          : matchArgumentsPost ty2 rest
  _                     ->
    [(typeExprToCType ty , map (comment . snd)
                               (skipUntilAfter (getSrcSpan ty) cs))]

matchConstructorsPost :: [ConstrDecl] -> [(Span, CDocComment)]
                      -> [CommentedConstr]
matchConstructorsPost []           _  = []
matchConstructorsPost [c]          cs = case c of
  RecordDecl spi _ _ f fs   ->
    let SpanInfo spR (sp:ss) = spi
    in  [CommentedRecord (identToQName f)
                        (map (comment . snd) (skipUntilAfter spR cs))
                        (map fieldType fs)
                        (matchFieldsPost (last ss) fs (skipUntilAfter sp cs))
                        NoAnalysisInfo]
  ConOpDecl _ _ _ ty1 f ty2 ->
    [CommentedConsOp (identToQName f)
                     (map (comment . snd) (skipUntilAfter (getSrcSpan c) cs))
                     (typeExprToCType ty1) (typeExprToCType ty2) NoAnalysisInfo]
  ConstrDecl _ _ _ f tys ->
    [CommentedConstr (identToQName f)
                     (map (comment . snd) (skipUntilAfter (getSrcSpan c) cs))
                     (map typeExprToCType tys) NoAnalysisInfo]
matchConstructorsPost (RecordDecl spi _ _ f fs:cn':cns) cs =
  let stop          = getSrcSpan cn'
      cs'           = skipUntilAfter (getSrcSpan spi) cs
      (match, rest) = getToMatch stop NoSpan cs' isPost
      SpanInfo _ (sp:ss) = spi
      fields        = matchFieldsPost (last ss) fs (skipUntilAfter sp cs)
  in  CommentedRecord (identToQName f) (map (comment . snd) match)
                      (map fieldType fs) fields
                      NoAnalysisInfo
        : matchConstructorsPost (cn':cns) rest
matchConstructorsPost (ConstrDecl spi _ _ f tys:cn':cns) cs =
  let stop          = getSrcSpan cn'
      cs'           = skipUntilAfter (getSrcSpan spi) cs
      (match, rest) = getToMatch stop NoSpan cs' isPost
  in  CommentedConstr (identToQName f) (map (comment . snd) match)
                      (map typeExprToCType tys) NoAnalysisInfo
        : matchConstructorsPost (cn':cns) rest
matchConstructorsPost (ConOpDecl spi _ _ ty1 f ty2:cn':cns) cs =
  let stop          = getSrcSpan cn'
      cs'           = skipUntilAfter (getSrcSpan spi) cs
      (match, rest) = getToMatch stop NoSpan cs' isPost
  in  CommentedConsOp (identToQName f) (map (comment . snd) match)
                      (typeExprToCType ty1) (typeExprToCType ty2) NoAnalysisInfo
        : matchConstructorsPost (cn':cns) rest

matchFieldsPost :: Span -- ^ Until
                -> [FieldDecl]
                -> [(Span, CDocComment)]
                -> [CommentedField]
matchFieldsPost _  []                                  _  = []
matchFieldsPost sp [f@(FieldDecl _ idts ty)]            cs =
  let (match, _) = getToMatch sp NoSpan (skipUntilAfter (getSrcSpan f) cs) isPost
  in  [(map identToQName idts, map (comment . snd) match, typeExprToCType ty)]
matchFieldsPost sp (f1@(FieldDecl _ idts ty) : f2 : fs) cs =
  let (match, rest) = getToMatch (getSrcSpan f2) NoSpan
                                 (skipUntilAfter (getSrcSpan f1) cs) isPost
  in (map identToQName idts, map (comment . snd) match, typeExprToCType ty)
        : matchFieldsPost sp (f2:fs) (skipUntilAfter (getSrcSpan f2) rest)


-- relies on the fact that for subsequent entries of the same decl,
-- all comments in the first are before the comments of the second and vice versa
merge :: [CommentedDecl] -> [CommentedDecl]
merge []                                 = []
merge [x]                                = [x]
merge (x1:x2:xs) = case (x1, x2) of
   (CommentedTypeDecl f1 v1 ty1 ys1, CommentedTypeDecl f2 _ _ ys2)
     | f1 == f2 -> merge (CommentedTypeDecl f1 v1 ty1 (ys1 ++ ys2) : xs)
   (CommentedDataDecl f1 vs1 ys1 cs1, CommentedDataDecl f2 _ ys2 cs2)
     | f1 == f2 -> merge (CommentedDataDecl f1 vs1 (ys1 ++ ys2)
                                            (zipWith zipCons cs1 cs2) : xs)
   (CommentedNewtypeDecl f1 vs1 ys1 (Just cns1), CommentedNewtypeDecl f2 _ ys2 (Just cns2))
     | f1 == f2 -> merge (CommentedNewtypeDecl f1 vs1 (ys1 ++ ys2)
                          (Just (zipNCons cns1 cns2)) : xs)
   (CommentedFunctionDecl f1 ys1 cx1 a1, CommentedFunctionDecl f2 ys2 _ _)
     | f1 == f2 -> merge (CommentedFunctionDecl f1 (ys1 ++ ys2) cx1 a1 : xs)
   (CommentedTypeSig f1 ys1 cx1 ps1, CommentedTypeSig f2 ys2 _ ps2)
     | f1 == f2 -> merge (CommentedTypeSig f1 (ys1 ++ ys2) cx1
                                              (zipWith zipPair ps1 ps2) : xs)
   (CommentedClassDecl f1 cx1 v1 ys1 ds1, CommentedClassDecl f2 _ _ ys2 ds2)
     | f1 == f2 -> merge (CommentedClassDecl f1 cx1 v1 (ys1 ++ ys2)
                                             (merge (ds1 ++ ds2)) : xs)
   (CommentedInstanceDecl f1 cx1 ty1 ys1 ds1, CommentedInstanceDecl f2 _ ty2 ys2 ds2)
     | ty1 == ty2 &&
       f1 == f2 -> merge (CommentedInstanceDecl f1 cx1 ty1 (ys1 ++ ys2)
                                                (merge (ds1 ++ ds2)) : xs)
   (CommentedExternalDecl f1 cs1 cx1 ai1, CommentedExternalDecl f2 cs2 _ _)
     | f1 == f2 -> merge (CommentedExternalDecl f1 (cs1 ++ cs2) cx1 ai1 : xs)
   _ -> x1 : merge (x2 : xs)

  where
    zipPair (a1, b1) (_, b2) = (a1, b1 ++ b2)

    zipField (a1, b1, c1) (_, b2, _) = (a1, b1 ++ b2, c1)

    zipCons a b = case (a, b) of
      ((CommentedConstr n1 cs1 tys1 ai1), (CommentedConstr _ cs2 _   _))
              -> CommentedConstr n1 (cs1 ++ cs2) tys1 ai1
      ((CommentedRecord n1 cs1 tys fs1  ai1), (CommentedRecord _ cs2 _ fs2 _))
              -> CommentedRecord n1 (cs1 ++ cs2) tys (zipWith zipField fs1 fs2) ai1
      ((CommentedConsOp n1 cs1 ty1 ty1' ai1), (CommentedConsOp _ cs2 _ _ _))
              -> CommentedConsOp n1 (cs1 ++ cs2) ty1 ty1' ai1
      _       -> error "Comment.merge.zipCons: different constructors"

    zipNCons a b = case (a, b) of
      ((CommentedNewConstr n1 cs1 ty1 ai1), (CommentedNewConstr _ cs2 _ _))
              -> CommentedNewConstr n1 (cs1 ++ cs2) ty1 ai1
      ((CommentedNewRecord n1 cs1 ty1 (Just f1) ai1),
       (CommentedNewRecord _  cs2 _   (Just f2) _))
              -> CommentedNewRecord n1 (cs1 ++ cs2) ty1 (Just (zipField f1 f2)) ai1
      _       -> error "Comment.merge.zipNCons: different constructors"

skipUntilAfter :: Span -> [(Span, a)] -> [(Span, a)]
skipUntilAfter sp = filter (( `isAfter` sp) . fst)

-------------------------------------------------------------------------------
-- utility for matching and conversions while matching

isPre, isPost, isNone :: CDocComment -> Bool
isPre  Pre  {} = True
isPre  Post {} = False
isPre  None {} = False

isPost Pre  {} = False
isPost Post {} = True
isPost None {} = False

isNone Pre  {} = False
isNone Post {} = False
isNone None {} = True

classifyComment :: Comment -> CDocComment
classifyComment (NestedComment s)
  | "{- |" `isPrefixOf` s = Pre  $ NestedComment $ dropLast2 $ drop 4 s
  | "{- ^" `isPrefixOf` s = Post $ NestedComment $ dropLast2 $ drop 4 s
  | otherwise             = None $ NestedComment $ dropLast2 $ drop 2 s
  where dropLast2 = init . init
classifyComment (LineComment   s)
  | "---"      ==       s = Pre  $ LineComment               $ drop 3 s
  | "--- " `isPrefixOf` s = Pre  $ LineComment               $ drop 4 s
  | "-- |" `isPrefixOf` s = Pre  $ LineComment               $ drop 4 s
  | "-- ^" `isPrefixOf` s = Post $ LineComment               $ drop 4 s
  | otherwise             = None $ LineComment               $ drop 2 s

commentString :: Comment -> String
commentString (LineComment   s) = s
commentString (NestedComment s) = s

splitNestedComment :: Comment -> [Comment]
splitNestedComment c@(LineComment   _) = [c]
splitNestedComment   (NestedComment s) = map LineComment $ lines s

isCommentedInstanceDecl :: CommentedDecl -> Bool
isCommentedInstanceDecl d = case d of
  CommentedInstanceDecl _ _ _ _ _ -> True
  _                               -> False

isCommentedTypeSig :: CommentedDecl -> Bool
isCommentedTypeSig d = case d of
  CommentedTypeSig _ _ _ _ -> True
  _                        -> False

instTypeName :: CommentedDecl -> QName
instTypeName d = case d of
  CommentedInstanceDecl _ _ ty _ _ ->
    let Just (q,_) = tconsArgsOfType ty
    in q
  _ -> error "Eoment.instTypeName: No instance type"

commentedDeclName :: CommentedDecl -> QName
commentedDeclName (CommentedTypeDecl n _ _ _) = n
commentedDeclName (CommentedDataDecl n _ _ _) = n
commentedDeclName (CommentedNewtypeDecl n _ _ _) = n
commentedDeclName (CommentedClassDecl n _ _ _ _) = n
commentedDeclName (CommentedInstanceDecl n _ _ _ _) = n
commentedDeclName (CommentedFunctionDecl n _ _ _) = n
commentedDeclName (CommentedTypeSig _ _ _ _) =
  error "Comment.commentedDeclName: CommentedTypeSig"
commentedDeclName (CommentedExternalDecl _ _ _ _) =
  error "Comment.commentedDeclName: CommentedExternalDecl"
commentedDeclName (UnsupportedDecl _) =
  error "Comment.commentedDeclName: UnsupportedDecl"

-------------------------------------------------------------------------------
-- Splitting of TypeSigs with multiple idents and field decls inside DataDecls
-- and filtering of UnsupportedDecls
-- also translates ExternalDecls to normal ones

cleanup :: [CommentedDecl] -> [CommentedDecl]
cleanup [] = []
cleanup (d@(CommentedTypeDecl            _ _ _ _) : ds) = d :  cleanup ds
cleanup (d@(CommentedFunctionDecl        _ _ _ _) : ds) = d :  cleanup ds
cleanup (d@(CommentedNewtypeDecl         _ _ _ _) : ds) = d :  cleanup ds
cleanup (  (UnsupportedDecl                    _) : ds) =      cleanup ds
cleanup (  (CommentedDataDecl        f vs cs cns) : ds) =
          CommentedDataDecl f vs cs (map cleanupConstr cns) :  cleanup ds
cleanup (  (CommentedClassDecl     f cx v cs ds') : ds) =
          CommentedClassDecl f cx v cs (cleanup ds')        :  cleanup ds
cleanup (  (CommentedInstanceDecl f cx ty cs ds') : ds) =
          CommentedInstanceDecl f cx ty cs (cleanup ds')    :  cleanup ds
cleanup (  (CommentedExternalDecl    fs cs cx ai) : ds) =
          map (\i -> CommentedFunctionDecl i cs cx ai) fs   ++ cleanup ds
cleanup (  (CommentedTypeSig     idts cs vs args) : ds) =
          map (\i -> CommentedTypeSig [i] cs vs args) idts  ++ cleanup ds

cleanupConstr :: CommentedConstr -> CommentedConstr
cleanupConstr c = case c of
  CommentedRecord f cs tys fs ai
    -> CommentedRecord f cs tys (concatMap cleanupField fs) ai
  _ -> c


cleanupField :: CommentedField -> [CommentedField]
cleanupField (ns, cs, ty) = map (\n -> ([n], cs, ty)) ns
