{- |
     Author  : Kai-Oliver Prott
     Version : August 2018

     Operations and datatypes to read comment files and
     match comments to declarations
-}
module CurryDoc.Info.Comments
  (readComments, associateCurryDoc,
   isExportSection,
   splitNestedComment, commentString,
   lookupFunc, lookupCons, lookupField, lookupClass, lookupRecord, lookupInstance,
   lookupRecord, lookupTypeSig, lookupNewDecl, lookupDataDecl, lookupTypeDecl,
   isOldStyleComment,
   Comment(..),
   CommentedDecl(..), ExportEntry(..), CommentedConstr(..), CommentedField)
   where

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
  deriving (Show, Read)

data CDocComment = Pre     { comment :: Comment }
                 | Post    { comment :: Comment }
                 | None    { comment :: Comment }
                 | Section { comment :: Comment, nest :: Int }

data CommentedDecl
  = CommentedTypeDecl QName [Comment]
  | CommentedDataDecl QName [Comment] [CommentedConstr]
  | CommentedNewtypeDecl QName [Comment] CommentedConstr
  | CommentedClassDecl QName [Comment] [CommentedDecl]
  | CommentedInstanceDecl QName CTypeExpr [Comment] [CommentedDecl]
  | CommentedFunctionDecl QName [Comment]
  | CommentedTypeSig [QName] [Comment] [(CTypeExpr, [Comment])]
  | CommentedExternalDecl [QName] [Comment]
  | CommentedExternalData QName [Comment]
  | UnsupportedDecl [Comment]

data CommentedConstr
  = CommentedConstr QName [Comment]
  | CommentedConsOp QName [Comment]
  | CommentedRecord QName [Comment] [CommentedField]

type CommentedField = ([QName], [Comment])

data ExportEntry a = ExportEntry a
                   | ExportEntryModule MName
                   | ExportSection Comment Int [ExportEntry a]
  deriving (Show, Read)

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
associateCurryDoc :: [(Span, Comment)] -> Module a
                  -> ([CommentedDecl], [Comment], Maybe [ExportEntry QName])
associateCurryDoc []       (Module _   _ _ ex _ _ ) =
  ([], [], maybe Nothing (Just . associateExports []) ex)
associateCurryDoc xs@(_:_) (Module spi _ _ ex _ ds) =
  let (header, rest) = associateCurryDocHeader spi sp xs'
      exportList     = maybe Nothing (Just . associateExports xs') ex
      matchings      = cleanup $ merge $ associateCurryDocDecls rest ds Nothing
  in  (matchings, header, exportList)
  where xs' = map (\(sp',c) -> (sp', classifyComment c)) xs
        sp = case ds of
          (d:_) -> getSrcSpan d
          _     -> NoSpan

associateExports :: [(Span, CDocComment)] -> ExportSpec -> [ExportEntry QName]
associateExports cs e = case e of
  Exporting (SpanInfo _ (sp:_)) ex
    -> associateExportList (skipUntilAfter sp cs) ex
  _ -> error $ "CurryDoc.Info.Comments.associateExports: " ++
               "Invalid SpanInfo in ExportList"

associateExportList :: [(Span, CDocComment)] -> [Export] -> [ExportEntry QName]
associateExportList _  []       = []
associateExportList cs (e : es) =
  if getSrcSpan e `isBeforeList` (map fst cs) -- True for null cs
    then genExportEntry e : associateExportList cs es
    else let ((_,c):cs') = cs
             es' = associateExportList cs' (e:es)
         in case c of
              Section com n -> genExportSection n com es'
              _             -> es'
  where genExportEntry (Export _ q)           = ExportEntry       (qIdentToQName q)
        genExportEntry (ExportTypeAll _ q)    = ExportEntry       (qIdentToQName q)
        genExportEntry (ExportTypeWith _ q _) = ExportEntry       (qIdentToQName q)
        genExportEntry (ExportModule _ m)     = ExportEntryModule (mIdentToMName m)

genExportSection :: Int -> Comment -> [ExportEntry QName] -> [ExportEntry QName]
genExportSection n c es =
  let (this, next) = span (\e -> not (isExportSection e) ||
                                     (exportSectionNesting e >= n)) es
  in ExportSection c n this : next

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

    (_  , _      )             ->
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
               let (match, next) = getToMatch spNextd sp cs isPost
               in  associateCurryDocDeclPost ((sp, cdc) : match) d
                     : associateCurryDocDecls next (d:ds) prev
           | otherwise ->
               associateCurryDocDecls ((sp, cdc) : cs) ds (Just d)

    _ -> associateCurryDocDecls cs (d:ds) prev
  where spd  = getSrcSpan d
        spNextd = case ds of
          []     -> NoSpan
          (d':_) -> getSrcSpan d'

matchLast :: [(Span, CDocComment)]
          -> Maybe (Decl a)
          -> [CommentedDecl]
matchLast []                       (Just _) = []
matchLast _                        Nothing  = []
matchLast ((sp, Post    c  ) : cs) (Just d) =
  let (match, next) = getToMatch (getSrcSpan d) sp cs isPost
  in  associateCurryDocDeclPost ((sp, Post c) : match) d
        : matchLast next (Just d)
matchLast ((_ , None    _  ) : cs) (Just d) = matchLast cs (Just d)
matchLast ((_ , Pre     _  ) : cs) (Just d) = matchLast cs (Just d)
matchLast ((_ , Section _ _) : cs) (Just d) = matchLast cs (Just d)

associateCurryDocDeclPre :: [(Span, CDocComment)]
                         -> Decl a
                         -> CommentedDecl
associateCurryDocDeclPre xs d@(FunctionDecl _ _ f _) =
  let (match, _) = getToMatch (getSrcSpan d) NoSpan xs isPre
  in  CommentedFunctionDecl (identToQName f) (map (comment . snd) match)
associateCurryDocDeclPre xs d@(ExternalDecl _ fs) =
  let (match, _) = getToMatch (getSrcSpan d) NoSpan xs isPre
  in  CommentedExternalDecl (map (\(Var _ i) -> identToQName i) fs)
                            (map (comment . snd) match)
associateCurryDocDeclPre xs d@(ExternalDataDecl _ f _) =
  let (match, _) = getToMatch (getSrcSpan d) NoSpan xs isPre
  in  CommentedExternalData (identToQName f) (map (comment . snd) match)
associateCurryDocDeclPre xs d@(TypeDecl _ f _ _) =
  let (match, _) = getToMatch (getSrcSpan d) NoSpan xs isPre
  in  CommentedTypeDecl (identToQName f) (map (comment . snd) match)
associateCurryDocDeclPre xs (ClassDecl spi _ f _ ds) =
  let (result, rest) = associateCurryDocHeader spi sp xs
      sp             = case ds of
                         (d:_) -> getSrcSpan d
                         _     -> NoSpan
  in  CommentedClassDecl (identToQName f) result
                         (associateCurryDocDecls rest ds Nothing)
associateCurryDocDeclPre xs (InstanceDecl spi _ f ty ds) =
  let (result, rest) = associateCurryDocHeader spi sp xs
      sp             = case ds of
                         (d:_) -> getSrcSpan d
                         _     -> NoSpan
  in  CommentedInstanceDecl (qIdentToQName f) (typeExprToCType ty) result
                            (associateCurryDocDecls rest ds Nothing)
associateCurryDocDeclPre xs d@(NewtypeDecl _ f _ c _) =
  let (match, rest ) = getToMatch (getSrcSpan d) NoSpan xs isPre
      (cons,  rest') = getToMatch (getSrcSpan c) NoSpan rest isPre
      ccon           = case c of
        NewConstrDecl _ cn _
          -> CommentedConstr (identToQName cn) (map (comment . snd) cons)
        NewRecordDecl spi cn (idt, ty) ->
          let SpanInfo _ (sp:_) = spi -- sp = '{'
              field = matchFieldsPre [FieldDecl NoSpanInfo [idt] ty]
                                     (skipUntilAfter sp rest')
          in  CommentedRecord (identToQName cn) (map (comment . snd) cons)
                              field
  in CommentedNewtypeDecl (identToQName f) (map (comment . snd) match) ccon
associateCurryDocDeclPre xs d@(DataDecl _ f _ [] _) =
  let (match, _) = getToMatch (getSrcSpan d) NoSpan xs isPre
  in  CommentedDataDecl (identToQName f) (map (comment . snd) match) []
associateCurryDocDeclPre xs d@(DataDecl spi f _ (c:cs) _) =
  let (match, rest) = getToMatch (getSrcSpan d) NoSpan xs isPre
      SpanInfo _ (_:sp:_) = spi
  in CommentedDataDecl (identToQName f) (map (comment . snd) match)
                       (matchConstructorsPre (c:cs) (skipUntilAfter sp rest))
associateCurryDocDeclPre xs d@(TypeSig _ fs
  (QualTypeExpr (SpanInfo _ (s:ss)) _ ty)) =
  let (match, rest) = getToMatch (getSrcSpan d) NoSpan xs isPre
      sp = last (s:ss) -- throw away everything until '=>'
  in  CommentedTypeSig (map identToQName fs) (map (comment . snd) match)
                       (matchArgumentsPre ty (skipUntilAfter sp rest))
associateCurryDocDeclPre xs d@(TypeSig spi fs
  (QualTypeExpr (SpanInfo _ []) _ ty)) =
   let (match, rest) = getToMatch (getSrcSpan d) NoSpan xs isPre
       SpanInfo _ [sp] = spi
   in  CommentedTypeSig (map identToQName fs) (map (comment . snd) match)
                        (matchArgumentsPre ty (skipUntilAfter sp rest))
associateCurryDocDeclPre xs (InfixDecl _ _ _ _) = UnsupportedDecl
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
  in  CommentedRecord (identToQName f) (map (comment . snd) match) fields
        : matchConstructorsPre cns (skipUntilAfter stop rest)
matchConstructorsPre (ConstrDecl spi _ _ f _ :cns) cs =
  let stop          = getSrcSpan spi
      (match, rest) = getToMatch stop NoSpan cs isPre
  in  CommentedConstr (identToQName f) (map (comment . snd) match)
        : matchConstructorsPre cns (skipUntilAfter stop rest)
matchConstructorsPre (ConOpDecl spi _ _ _ f _ :cns) cs =
  let stop          = getSrcSpan spi
      (match, rest) = getToMatch stop NoSpan cs isPre
  in  CommentedConsOp (identToQName f) (map (comment . snd) match)
        : matchConstructorsPre cns (skipUntilAfter stop rest)

matchFieldsPre :: [FieldDecl] -> [(Span, CDocComment)] -> [CommentedField]
matchFieldsPre []                            _  = []
matchFieldsPre (f@(FieldDecl _ idts _) : fs) cs =
  let (match, rest) = getToMatch (getSrcSpan f) NoSpan cs isPre
  in (map identToQName idts, map (comment . snd) match)
       : matchFieldsPre fs (skipUntilAfter (getSrcSpan f) rest)

associateCurryDocDeclPost :: [(Span, CDocComment)]
                          -> Decl a
                          -> CommentedDecl
associateCurryDocDeclPost xs d@(FunctionDecl _ _ f _) =
  CommentedFunctionDecl (identToQName f)
                        (map (comment . snd)
                             (skipUntilAfter (getSrcSpan d) xs))
associateCurryDocDeclPost xs d@(ExternalDecl _ fs) =
  CommentedExternalDecl (map (\(Var _ i) -> identToQName i) fs)
                        (map (comment . snd)
                             (skipUntilAfter (getSrcSpan d) xs))
associateCurryDocDeclPost xs d@(ExternalDataDecl _ f _) =
  CommentedExternalData (identToQName f)
                        (map (comment . snd)
                             (skipUntilAfter (getSrcSpan d) xs))
associateCurryDocDeclPost xs d@(TypeDecl _ f _ _) =
  CommentedTypeDecl (identToQName f) (map (comment . snd)
                    (skipUntilAfter (getSrcSpan d) xs))
associateCurryDocDeclPost xs (ClassDecl spi _ f _ ds) =
  let (result, rest) = associateCurryDocHeader spi sp xs
      sp = case ds of
             (d:_) -> getSrcSpan d
             _     -> NoSpan
  in  CommentedClassDecl (identToQName f) result
                         (associateCurryDocDecls rest ds Nothing)
associateCurryDocDeclPost xs (InstanceDecl spi _ f ty ds) =
  let (result, rest) = associateCurryDocHeader spi sp xs
      sp = case ds of
             (d:_) -> getSrcSpan d
             _     -> NoSpan
  in  CommentedInstanceDecl (qIdentToQName f) (typeExprToCType ty) result
                            (associateCurryDocDecls rest ds Nothing)
associateCurryDocDeclPost xs (NewtypeDecl _ f _ c []) = --no deriving
  CommentedNewtypeDecl (identToQName f) [] -- thus cannot have post comments
                       (matchNewConstrPost c
                          (skipUntilAfter (getSrcSpan c) xs) xs)
associateCurryDocDeclPost xs (NewtypeDecl spi f _ c (_:_)) =
  let SpanInfo sp ss = spi
      (match, rest)  = getToMatch (last ss) NoSpan
                         (skipUntilAfter (getSrcSpan c) xs) isPost
  in CommentedNewtypeDecl (identToQName f)
                          (map (comment . snd) (skipUntilAfter sp rest))
                          (matchNewConstrPost c match xs)
associateCurryDocDeclPost xs d@(DataDecl _ f _ [] _) = -- cannot have deriving
  CommentedDataDecl (identToQName f)
                    (map (comment . snd) (skipUntilAfter (getSrcSpan d) xs)) []
associateCurryDocDeclPost xs (DataDecl _ f _ (c:cs) []) = -- no deriving
  CommentedDataDecl (identToQName f) []
                    (matchConstructorsPost (c:cs) xs)
associateCurryDocDeclPost xs (DataDecl spi f _ (c:cs) (_:_)) =
  let SpanInfo sp ss = spi
      (declC, consC) = partition ((`isAfter` sp) . fst) xs
      spDeriving     = ss !! (length cs + 2)
  in CommentedDataDecl (identToQName f) (map (comment . snd) declC)
                       (matchConstructorsPost (c:cs)
                         (filter ((`isBefore` spDeriving) . fst) consC))
associateCurryDocDeclPost xs (TypeSig _ fs (QualTypeExpr _ _ ty)) =
  CommentedTypeSig (map identToQName fs) [] (matchArgumentsPost ty xs)
associateCurryDocDeclPost xs (InfixDecl _ _ _ _) = UnsupportedDecl
  (map (comment . snd) xs)
associateCurryDocDeclPost xs (DefaultDecl _ _) = UnsupportedDecl
  (map (comment . snd) xs)

matchNewConstrPost :: NewConstrDecl -> [(Span, CDocComment)]
                   -> [(Span, CDocComment)] -> CommentedConstr
matchNewConstrPost (NewConstrDecl _ cn _) cs _ =
    CommentedConstr (identToQName cn)
                       (map (comment . snd) cs)
matchNewConstrPost (NewRecordDecl spiR cn (idt, ty)) cs xs =
    let SpanInfo _ ssR = spiR
        field = matchFieldsPost (last ssR) [FieldDecl NoSpanInfo [idt] ty]
                                (skipUntilAfter (getSrcSpan ty) xs)
    in CommentedRecord (identToQName cn) (map (comment . snd) cs) field

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
                         (matchFieldsPost (last ss) fs (skipUntilAfter sp cs))]
  ConOpDecl _ _ _ _ f _ ->
    [CommentedConsOp (identToQName f)
                     (map (comment . snd) (skipUntilAfter (getSrcSpan c) cs))]
  ConstrDecl _ _ _ f _ ->
    [CommentedConstr (identToQName f)
                     (map (comment . snd) (skipUntilAfter (getSrcSpan c) cs))]
matchConstructorsPost (RecordDecl spi _ _ f fs:cn':cns) cs =
  let SpanInfo _ (sp:ss) = spi
      stop          = getSrcSpan cn'
      cs'           = skipUntilAfter (getSrcSpan spi) cs
      (match, rest) = getToMatch stop NoSpan cs' isPost
      fields        = matchFieldsPost (last ss) fs (skipUntilAfter sp cs)
  in  CommentedRecord (identToQName f) (map (comment . snd) match) fields
        : matchConstructorsPost (cn':cns) rest
matchConstructorsPost (ConstrDecl spi _ _ f _:cn':cns) cs =
  let stop          = getSrcSpan cn'
      cs'           = skipUntilAfter (getSrcSpan spi) cs
      (match, rest) = getToMatch stop NoSpan cs' isPost
  in  CommentedConstr (identToQName f) (map (comment . snd) match)
        : matchConstructorsPost (cn':cns) rest
matchConstructorsPost (ConOpDecl spi _ _ _ f _:cn':cns) cs =
  let stop          = getSrcSpan cn'
      cs'           = skipUntilAfter (getSrcSpan spi) cs
      (match, rest) = getToMatch stop NoSpan cs' isPost
  in  CommentedConsOp (identToQName f) (map (comment . snd) match)
        : matchConstructorsPost (cn':cns) rest

matchFieldsPost :: Span -- ^ Until
                -> [FieldDecl]
                -> [(Span, CDocComment)]
                -> [CommentedField]
matchFieldsPost _  []                                  _  = []
matchFieldsPost sp [f@(FieldDecl _ idts _)]            cs =
  let (match, _) = getToMatch sp NoSpan (skipUntilAfter (getSrcSpan f) cs) isPost
  in  [(map identToQName idts, map (comment . snd) match)]
matchFieldsPost sp (f1@(FieldDecl _ idts _) : f2 : fs) cs =
  let (match, rest) = getToMatch (getSrcSpan f2) NoSpan
                                 (skipUntilAfter (getSrcSpan f1) cs) isPost
  in (map identToQName idts, map (comment . snd) match)
        : matchFieldsPost sp (f2:fs) (skipUntilAfter (getSrcSpan f2) rest)


-- relies on the fact that for subsequent entries of the same decl,
-- all comments in the first are before the comments of the second and vice versa
merge :: [CommentedDecl] -> [CommentedDecl]
merge []                                 = []
merge [x]                                = [x]
merge (x1:x2:xs) = case (x1, x2) of
   (CommentedTypeDecl f1 cs1, CommentedTypeDecl f2 cs2)
     | f1 == f2 -> merge (CommentedTypeDecl f1 (cs1 ++ cs2) : xs)
   (CommentedDataDecl f1 cs1 cns1, CommentedDataDecl f2 cs2 cns2)
     | f1 == f2 -> merge (CommentedDataDecl f1 (cs1 ++ cs2)
                                            (zipWith zipCons cns1 cns2) : xs)
   (CommentedNewtypeDecl f1 cs1 cns1, CommentedNewtypeDecl f2 cs2 cns2)
     | f1 == f2 -> merge (CommentedNewtypeDecl f1 (cs1 ++ cs2)
                                               (zipCons cns1 cns2) : xs)
   (CommentedClassDecl f1 cs1 ds1, CommentedClassDecl f2 cs2 ds2)
     | f1 == f2 -> merge (CommentedClassDecl f1 (cs1 ++ cs2)
                                             (mergeLocal ds1 ds2) : xs)
   (CommentedInstanceDecl f1 ty1 cs1 ds1, CommentedInstanceDecl f2 ty2 cs2 ds2)
     | ty1 == ty2 &&
       f1 == f2 -> merge (CommentedInstanceDecl f1 ty1 (cs1 ++ cs2)
                                                (mergeLocal ds1 ds2) : xs)
   (CommentedFunctionDecl f1 cs1, CommentedFunctionDecl f2 cs2)
     | f1 == f2 -> merge (CommentedFunctionDecl f1 (cs1 ++ cs2) : xs)
   (CommentedTypeSig f1 cs1 ps1, CommentedTypeSig f2 cs2 ps2)
     | f1 == f2 -> merge (CommentedTypeSig f1 (cs1 ++ cs2)
                                              (zipWith zipPair ps1 ps2) : xs)
   (CommentedExternalDecl f1 cs1, CommentedExternalDecl f2 cs2)
     | f1 == f2 -> merge (CommentedExternalDecl f1 (cs1 ++ cs2) : xs)
   (CommentedExternalData f1 cs1, CommentedExternalData f2 cs2)
     | f1 == f2 -> merge (CommentedExternalData f1 (cs1 ++ cs2) : xs)
   _ -> x1 : merge (x2 : xs)

  where
    zipPair (a1, b1) (_, b2) = (a1, b1 ++ b2)

    zipCons a b = case (a, b) of
      ((CommentedConstr n1 cs1), (CommentedConstr _ cs2))
              -> CommentedConstr n1 (cs1 ++ cs2)
      ((CommentedRecord n1 cs1 fs1), (CommentedRecord _ cs2 fs2))
              -> CommentedRecord n1 (cs1 ++ cs2) (zipWith zipPair fs1 fs2)
      ((CommentedConsOp n1 cs1), (CommentedConsOp _ cs2))
              -> CommentedConsOp n1 (cs1 ++ cs2)
      _       -> error "Comment.merge.zipCons: different constructors"

-- | merge non-toplevel declarations,
--   as the assumption from above does not hold there
mergeLocal :: [CommentedDecl] -> [CommentedDecl] -> [CommentedDecl]
mergeLocal []     _   = []
mergeLocal (d:ds) ds' = (case d of
  CommentedTypeSig      f _ _ -> maybe d (combine d) (lookupTypeSig f ds')
  CommentedFunctionDecl f _   -> maybe d (combine d) (lookupFunc    f ds')
  _                           -> d)
  : mergeLocal ds ds'
  where combine d1 d2 = head (merge [d1, d2])


skipUntilAfter :: Span -> [(Span, a)] -> [(Span, a)]
skipUntilAfter sp = filter (( `isAfter` sp) . fst)

getToMatch :: Span                  -- ^ until
           -> Span                  -- ^ last undiscarded comment span
           -> [(Span, CDocComment)] -- ^ next comments
           -> (CDocComment -> Bool) -- ^ predicate to test for right comment type
           -> ([(Span, CDocComment)], [(Span, CDocComment)])
getToMatch _    _    []             _ = ([], [])
getToMatch stop last ((sp, c) : cs) p =
  if (sp `isBefore` stop || isNoSpan stop)            -- pos is ok
     && (p c || (isNone c && vertDist last sp <= 1))  -- CDocType is ok
    then add (sp, c) (getToMatch stop sp cs p)
    else ([], (sp, c) : cs)
  where add x (xs, rest) = (x:xs, rest)

-------------------------------------------------------------------------------
-- Splitting of TypeSigs with multiple idents and field decls inside DataDecls
-- and filtering of UnsupportedDecls
-- also translates CommentedExternalDecl/Data to CommmentedFunctionDecls/DataDecls

cleanup :: [CommentedDecl] -> [CommentedDecl]
cleanup [] = []
cleanup (d@(CommentedTypeDecl                _ _) : ds) = d :  cleanup ds
cleanup (d@(CommentedFunctionDecl            _ _) : ds) = d :  cleanup ds
cleanup (d@(CommentedNewtypeDecl           _ _ _) : ds) = d :  cleanup ds
cleanup (  (UnsupportedDecl                    _) : ds) =      cleanup ds
cleanup (  (CommentedExternalData           f cs) : ds) =
            CommentedDataDecl f cs []                       :  cleanup ds
cleanup (  (CommentedDataDecl           f cs cns) : ds) =
            CommentedDataDecl f cs (map cleanupConstr cns)  :  cleanup ds
cleanup (  (CommentedClassDecl          f cs ds') : ds) =
            CommentedClassDecl f cs (cleanup ds')           :  cleanup ds
cleanup (  (CommentedInstanceDecl    f ty cs ds') : ds) =
            CommentedInstanceDecl f ty cs (cleanup ds')     :  cleanup ds
cleanup (  (CommentedExternalDecl          fs cs) : ds) =
            map (\i -> CommentedFunctionDecl i cs) fs       ++ cleanup ds
cleanup (  (CommentedTypeSig        idts cs args) : ds) =
            map (\i -> CommentedTypeSig [i] cs args) idts   ++ cleanup ds

cleanupConstr :: CommentedConstr -> CommentedConstr
cleanupConstr c = case c of
  CommentedRecord f cs fs
    -> CommentedRecord f cs (concatMap cleanupField fs)
  _ -> c

cleanupField :: CommentedField -> [CommentedField]
cleanupField (ns, cs) = map (\n -> ([n], cs)) ns

-------------------------------------------------------------------------------
-- utility for matching and conversions while matching

isPre, isPost, isNone, isSection :: CDocComment -> Bool
isPre  Pre     {} = True
isPre  Post    {} = False
isPre  None    {} = False
isPre  Section {} = False

isPost Pre     {}    = False
isPost Post    {}    = True
isPost None    {}    = False
isPost Section {}    = False

isNone Pre     {} = False
isNone Post    {} = False
isNone None    {} = True
isNone Section {} = False

isSection Pre     {} = False
isSection Post    {} = False
isSection None    {} = False
isSection Section {} = True

isExportSection :: ExportEntry a -> Bool
isExportSection e = case e of
  ExportSection _ _ _-> True
  _                  -> False

exportSectionNesting :: ExportEntry a -> Int
exportSectionNesting e = case e of
  ExportSection _ i _ -> i
  _ -> error "CurryDoc.Info.Comments.exportSectionNesting: Not an ExportSection"

classifyComment :: Comment -> CDocComment
classifyComment (NestedComment s)
  | "{- |" `isPrefixOf` s = Pre     $ NestedComment $ dropLast2 $ drop 4     s
  | "{- ^" `isPrefixOf` s = Post    $ NestedComment $ dropLast2 $ drop 4     s
  | "{- *" `isPrefixOf` s = Section ( NestedComment $ dropLast2 $ drop (3+n) s) n
  | otherwise             = None    $ NestedComment $ dropLast2 $ drop 2     s
  where n = length $ takeWhile (=='*') $ drop 3 s
        dropLast2 = init . init
classifyComment (LineComment   s)
  | "---"      ==       s = Pre     $ LineComment               $ drop 3     s
  | "--- " `isPrefixOf` s = Pre     $ LineComment               $ drop 4     s
  | "-- |" `isPrefixOf` s = Pre     $ LineComment               $ drop 4     s
  | "-- ^" `isPrefixOf` s = Post    $ LineComment               $ drop 4     s
  | "-- *" `isPrefixOf` s = Section ( LineComment               $ drop (3+n) s) n
  | otherwise             = None    $ LineComment               $ drop 2     s
  where n = length $ takeWhile (=='*') $ drop 3 s

commentString :: Comment -> String
commentString (LineComment   s) = s
commentString (NestedComment s) = s

isOldStyleComment :: Comment -> Bool
isOldStyleComment (LineComment   s)
  | s == "---" || "--- " `isPrefixOf` s = True
  | otherwise                           = False
isOldStyleComment (NestedComment _)     = False

splitNestedComment :: Comment -> [Comment]
splitNestedComment c@(LineComment   _) = [c]
splitNestedComment   (NestedComment s) = map LineComment $ lines s

-------------------------------------------------------------------------------
-- lookup entries

lookupClass :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupClass _ []     = Nothing
lookupClass n (d:ds) = case d of
  CommentedClassDecl n' _ _
    | n =~= n' -> Just d
  _            -> lookupClass n ds

lookupInstance :: QName -> CTypeExpr -> [CommentedDecl] -> Maybe CommentedDecl
lookupInstance _ _  []     = Nothing
lookupInstance n ty (d:ds) = case d of
  CommentedInstanceDecl n' ty' _ _
    | n =~= n' && ty =~~= ty' -> Just d
  _                           -> lookupInstance n ty ds

lookupFunc :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupFunc _ []     = Nothing
lookupFunc n (d:ds) = case d of
  CommentedFunctionDecl n' _
    | n =~= n' -> Just d
  _            -> lookupFunc n ds

lookupTypeDecl :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupTypeDecl _ []     = Nothing
lookupTypeDecl n (d:ds) = case d of
  CommentedTypeDecl n' _
    | n =~= n' -> Just d
  _            -> lookupTypeDecl n ds

lookupDataDecl :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupDataDecl _ []     = Nothing
lookupDataDecl n (d:ds) = case d of
  CommentedDataDecl n' _ _
    | n =~= n' -> Just d
  _            -> lookupDataDecl n ds

lookupNewDecl :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupNewDecl _ []     = Nothing
lookupNewDecl n (d:ds) = case d of
  CommentedNewtypeDecl n' _ _
    | n =~= n' -> Just d
  _            -> lookupNewDecl n ds

lookupTypeSig :: [QName] -> [CommentedDecl] -> Maybe CommentedDecl
lookupTypeSig _ []     = Nothing
lookupTypeSig n (d:ds) = case d of
  CommentedTypeSig n' _ _
    | all (uncurry (=~=)) (zip n n')
       -> Just d
  _    -> lookupTypeSig n ds

lookupField :: QName -> [CommentedField] -> Maybe CommentedField
lookupField _ []     = Nothing
lookupField n (f:fs) = case f of
  ([n'], _)
    | n =~= n' -> Just f
  _            -> lookupField n fs

lookupRecord :: QName -> [CommentedConstr] -> Maybe CommentedConstr
lookupRecord _ []     = Nothing
lookupRecord n (c:cs) = case c of
  CommentedRecord n' _ _
    | n =~= n' -> Just c
  _            -> lookupRecord n cs

lookupCons :: QName -> [CommentedConstr] -> Maybe CommentedConstr
lookupCons _ []     = Nothing
lookupCons n (c:cs) = case c of
  CommentedConstr n'  _
    | n =~= n' -> Just c
  CommentedConsOp n'  _
    | n =~= n' -> Just c
  _            -> lookupCons n cs
