module CurryDoc.Comment where

import CurryDoc.Span
import CurryDoc.SpanInfo
import CurryDoc.Type
import CurryDoc.Ident

import List
import Maybe (listToMaybe)

data Comment = NestedComment String
             | LineComment   String
  deriving (Eq, Ord, Read, Show)

data CDocComment = Pre  { comment :: Comment }
                 | Post { comment :: Comment }
                 | None { comment :: Comment }
  deriving Show

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


data CommentedDecl
  = CommentedTypeDecl Ident [Comment]
  | CommentedDataDecl Ident [Comment] [(Ident, [Comment], [([Ident], [Comment])])]
  | CommentedNewtypeDecl Ident [Comment] Ident [Comment] (Maybe ([Ident], [Comment]))
  | CommentedClassDecl Ident [Comment] [CommentedDecl]
  | CommentedInstanceDecl QualIdent InstanceType [Comment] [CommentedDecl]
  | CommentedFunctionDecl Ident [Comment]
  | CommentedTypeSig [Ident] [Comment] [(TypeExpr, [Comment])]
  | UnsupportedDecl [Comment]
  deriving Show


readCommentFile :: String -> IO [(Span, Comment)]
readCommentFile s = readFile s >>= (return . read)

readASTFile :: String -> IO (Module ())
readASTFile s = readFile s >>= (return . read)

associateCurryDoc :: [(Span, Comment)] -> Module a -> ([CommentedDecl], [Comment])
associateCurryDoc []       _                       = ([], [])
associateCurryDoc xs@(_:_) (Module spi _ _ _ _ ds) =
  let (result, rest) = associateCurryDocHeader spi sp xs'
  in  (merge $ associateCurryDocDecls rest ds Nothing, result)
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
  in  CommentedFunctionDecl f (map (comment . snd) match)
associateCurryDocDeclPre xs d@(TypeDecl _ f _ _  ) =
  let (match, _) = getToMatch (getSrcSpan d) NoSpan xs isPre
  in  CommentedTypeDecl f (map (comment . snd) match)
associateCurryDocDeclPre xs (ClassDecl spi _ f _ ds) =
  let (result, rest) = associateCurryDocHeader spi sp xs
  in  CommentedClassDecl f result (associateCurryDocDecls rest ds Nothing)
  where sp = case ds of
          (d:_) -> getSrcSpan d
          _     -> NoSpan
associateCurryDocDeclPre xs (InstanceDecl spi _ f ty ds) =
  let (result, rest) = associateCurryDocHeader spi sp xs
  in  CommentedInstanceDecl f ty result (associateCurryDocDecls rest ds Nothing)
  where sp = case ds of
          (d:_) -> getSrcSpan d
          _     -> NoSpan
associateCurryDocDeclPre xs d@(NewtypeDecl _ f _ c _) =
  let (match, rest ) = getToMatch (getSrcSpan d) NoSpan xs isPre
      (cons,  rest') = getToMatch (getSrcSpan c) NoSpan rest isPre
      field          = listToMaybe (case c of
        NewConstrDecl _ _ _         -> []
        NewRecordDecl spi _ (idt, ty) ->
          let SpanInfo _ (sp:_) = spi -- sp = '{'
          in matchFieldsPre [FieldDecl NoSpanInfo [idt] ty]
                            (skipUntilAfter sp rest'))
  in CommentedNewtypeDecl f (map (comment . snd) match) (getNewtypeConstrName c)
                            (map (comment . snd) cons) field
associateCurryDocDeclPre xs d@(DataDecl _ f _ [] _) =
  let (match, _) = getToMatch (getSrcSpan d) NoSpan xs isPre
  in  CommentedDataDecl f (map (comment . snd) match) []
associateCurryDocDeclPre xs d@(DataDecl spi f _ (c:cs) _) =
  let (match, rest) = getToMatch (getSrcSpan d) NoSpan xs isPre
      sp = case spi of
        SpanInfo _ (_:sp':_)   -> sp' -- throw away everything until '='
        _                      -> error ("Comment.associateCurryDocDeclPre: "
                                         ++ show spi)
  in CommentedDataDecl f (map (comment . snd) match)
                         (matchConstructorsPre (c:cs) (skipUntilAfter sp rest))
associateCurryDocDeclPre xs d@(TypeSig _ fs
  (QualTypeExpr (SpanInfo _ (s:ss)) _ ty)) =
  let (match, rest) = getToMatch (getSrcSpan d) NoSpan xs isPre
      sp = last (s:ss) -- throw away everything until '=>'
  in  CommentedTypeSig fs (map (comment . snd) match)
                          (matchArgumentsPre ty (skipUntilAfter sp rest))
associateCurryDocDeclPre xs d@(TypeSig spi fs
  (QualTypeExpr (SpanInfo _ []) _ ty)) =
   let (match, rest) = getToMatch (getSrcSpan d) NoSpan xs isPre
       sp = case spi of
         SpanInfo _ [sp']   -> sp' -- throw away everything until '::'
         _                  -> error ("Comment.associateCurryDocDeclPre: "
                                       ++ show spi)
   in  CommentedTypeSig fs (map (comment . snd) match)
                           (matchArgumentsPre ty (skipUntilAfter sp rest))
associateCurryDocDeclPre xs (ExternalDecl _ _) = UnsupportedDecl
  (map (comment . snd) xs)
associateCurryDocDeclPre xs (ExternalDataDecl _ _ _) = UnsupportedDecl
  (map (comment . snd) xs)
associateCurryDocDeclPre xs (InfixDecl _ _ _ _) = UnsupportedDecl
  (map (comment . snd) xs)
associateCurryDocDeclPre xs (DefaultDecl _ _) = UnsupportedDecl
  (map (comment . snd) xs)
associateCurryDocDeclPre _ (TypeSig _ _ (QualTypeExpr NoSpanInfo _ _)) =
  error "associateCurryDocDeclPre: NoSpanInfo in QualTypeExpr"

matchArgumentsPre :: TypeExpr -> [(Span, CDocComment)] -> [(TypeExpr, [Comment])]
matchArgumentsPre ty cs = case ty of
  ArrowType _ ty1 ty2 ->
    let (match, rest) = getToMatch (getSrcSpan ty) NoSpan cs isPre
    in  (ty1, map (comment . snd) match)
          : matchArgumentsPre ty2 (skipUntilAfter (getSrcSpan ty1) rest)
  _                   ->
    let (match, _) = getToMatch (getSrcSpan ty) NoSpan cs isPre
    in  [(ty , map (comment . snd) match)]

matchConstructorsPre :: [ConstrDecl] -> [(Span, CDocComment)] -> [(Ident, [Comment], [([Ident], [Comment])])]
matchConstructorsPre []       _  = []
matchConstructorsPre (cn:cns) cs =
  let stop          = getSrcSpan cn
      (match, rest) = getToMatch stop NoSpan cs isPre
      fields        = case cn of
        RecordDecl spi _ _ _ fs ->
          let SpanInfo _ (sp:_) = spi
          in matchFieldsPre fs (skipUntilAfter sp cs)
        _                       -> []
  in  (getConstrName cn, map (comment . snd) match, fields)
        : matchConstructorsPre cns (skipUntilAfter stop rest)

matchFieldsPre :: [FieldDecl] -> [(Span, CDocComment)] -> [([Ident], [Comment])]
matchFieldsPre []                            _  = []
matchFieldsPre (f@(FieldDecl _ idts _) : fs) cs =
  let (match, rest) = getToMatch (getSrcSpan f) NoSpan cs isPre
  in (idts, map (comment . snd) match)
       : matchFieldsPre fs (skipUntilAfter (getSrcSpan f) rest)

associateCurryDocDeclPost :: [(Span, CDocComment)]
                          -> Decl a
                          -> CommentedDecl
associateCurryDocDeclPost xs d@(FunctionDecl _ _ f _) =
   CommentedFunctionDecl f  (map (comment . snd)
                                (skipUntilAfter (getSrcSpan d) xs))
associateCurryDocDeclPost xs d@(TypeDecl _ f _ _) =
  CommentedTypeDecl f  (map (comment . snd) (skipUntilAfter (getSrcSpan d) xs))
associateCurryDocDeclPost xs (ClassDecl spi _ f _ ds) =
  let (result, rest) = associateCurryDocHeader spi sp xs
  in  CommentedClassDecl f result (associateCurryDocDecls rest ds Nothing)
  where sp = case ds of
          (d:_) -> getSrcSpan d
          _     -> NoSpan
associateCurryDocDeclPost xs (InstanceDecl spi _ f ty ds) =
  let (result, rest) = associateCurryDocHeader spi sp xs
  in  CommentedInstanceDecl f ty result (associateCurryDocDecls rest ds Nothing)
  where sp = case ds of
          (d:_) -> getSrcSpan d
          _     -> NoSpan
associateCurryDocDeclPost xs (NewtypeDecl _ f _ c []) = --no deriving
  let field = listToMaybe (case c of
        NewConstrDecl _ _ _           -> []
        NewRecordDecl spi _ (idt, ty) ->
          let SpanInfo _ ss = spi
          in matchFieldsPost (last ss) [FieldDecl NoSpanInfo [idt] ty]
                             (skipUntilAfter (getSrcSpan ty) xs))
  in CommentedNewtypeDecl f [] (getNewtypeConstrName c)
                               (map (comment . snd)
                                    (skipUntilAfter (getSrcSpan c) xs)) field
associateCurryDocDeclPost xs (NewtypeDecl spi f _ c (_:_)) =
  let SpanInfo sp ss = spi -- otherwise something is wrong
      field          = listToMaybe (case c of
        NewConstrDecl _ _ _           -> []
        NewRecordDecl spiR _ (idt, ty) ->
          let SpanInfo _ ssR = spiR
          in matchFieldsPost (last ssR) [FieldDecl NoSpanInfo [idt] ty]
                             (skipUntilAfter (getSrcSpan ty) xs))
      (match, rest)  = getToMatch (last ss) NoSpan
                         (skipUntilAfter (getSrcSpan c) xs) isPost
  in CommentedNewtypeDecl f (map (comment . snd) (skipUntilAfter sp rest))
                            (getNewtypeConstrName c)
                            (map (comment . snd) match) field
associateCurryDocDeclPost xs d@(DataDecl _ f _ [] _) = -- cannot have deriving
  CommentedDataDecl f (map (comment . snd) (skipUntilAfter (getSrcSpan d) xs)) []
associateCurryDocDeclPost xs d@(DataDecl _ f _ (c:cs) []) = -- no deriving
  CommentedDataDecl f [] (matchConstructorsPost (c:cs) xs)
associateCurryDocDeclPost xs (DataDecl spi f _ (c:cs) (_:_)) =
  let SpanInfo sp ss = spi -- otherwise something is wrong
      (declC, consC) = partition ((`isAfter` sp) . fst) xs
      spDeriving     = ss !! (length cs + 2)
  in CommentedDataDecl f (map (comment . snd) declC)
                         (matchConstructorsPost (c:cs)
                               (filter ((`isBefore` spDeriving) . fst) consC))
associateCurryDocDeclPost xs (TypeSig _ fs (QualTypeExpr _ _ ty)) =
  CommentedTypeSig fs [] (matchArgumentsPost ty xs)
associateCurryDocDeclPost xs (ExternalDecl _ _) = UnsupportedDecl
  (map (comment . snd) xs)
associateCurryDocDeclPost xs (ExternalDataDecl _ _ _) = UnsupportedDecl
  (map (comment . snd) xs)
associateCurryDocDeclPost xs (InfixDecl _ _ _ _) = UnsupportedDecl
  (map (comment . snd) xs)
associateCurryDocDeclPost xs (DefaultDecl _ _) = UnsupportedDecl
  (map (comment . snd) xs)

matchArgumentsPost :: TypeExpr -> [(Span, CDocComment)] -> [(TypeExpr, [Comment])]
matchArgumentsPost ty cs = case ty of
  ArrowType spi ty1 ty2 ->
    let SpanInfo _ [sp] = spi -- arrow span
        (match, rest) = getToMatch sp NoSpan
                          (skipUntilAfter (getSrcSpan ty1) cs) isPost
    in  (ty1, map (comment . snd) match)
          : matchArgumentsPost ty2 rest
  _                     ->
    [(ty , map (comment . snd) (skipUntilAfter (getSrcSpan ty) cs))]

matchConstructorsPost :: [ConstrDecl] -> [(Span, CDocComment)] -> [(Ident, [Comment], [([Ident], [Comment])])]
matchConstructorsPost []           _  = []
matchConstructorsPost [c]          cs = case c of
  RecordDecl spi _ _ f fs ->
    let SpanInfo spR (sp:ss) = spi
    in  [(f, map (comment . snd) (skipUntilAfter spR cs),
                 matchFieldsPost (last ss) fs (skipUntilAfter sp cs))]
  _                       ->
    [(getConstrName c, map (comment . snd)
                           (skipUntilAfter (getSrcSpan c) cs), [])]
matchConstructorsPost (cn:cn':cns) cs =
  let stop          = getSrcSpan cn'
      cs'           = skipUntilAfter (getSrcSpan cn) cs
      (match, rest) = getToMatch stop NoSpan cs' isPost
      fields        = case cn of
        RecordDecl spi _ _ _ fs ->
          let SpanInfo _ (sp:ss) = spi
          in  matchFieldsPost (last ss) fs (skipUntilAfter sp cs)
        _                       -> []
  in  (getConstrName cn, map (comment . snd) match, fields)
        : matchConstructorsPost (cn':cns) rest

matchFieldsPost :: Span -- ^ Until
                -> [FieldDecl]
                -> [(Span, CDocComment)]
                -> [([Ident], [Comment])]
matchFieldsPost _  []                                  _  = []
matchFieldsPost sp [f@(FieldDecl _ idts _)]            cs =
  let (match, _) = getToMatch sp NoSpan (skipUntilAfter (getSrcSpan f) cs) isPost
  in  [(idts, map (comment . snd) match)]
matchFieldsPost sp (f1@(FieldDecl _ idts _) : f2 : fs) cs =
  let (match, rest) = getToMatch (getSrcSpan f2) NoSpan
                                 (skipUntilAfter (getSrcSpan f1) cs) isPost
  in (idts, map (comment . snd) match)
        : matchFieldsPost sp (f2:fs) (skipUntilAfter (getSrcSpan f2) rest)


-- relies on the fact that for subsequent entries of the same decl,
-- all comments in the first are before the comments of the second and vice versa
merge :: [CommentedDecl] -> [CommentedDecl]
merge []                                 = []
merge [x]                                = [x]
merge (x1:x2:xs) = case (x1, x2) of
   (CommentedTypeDecl f1 ys1, CommentedTypeDecl f2 ys2)
     | f1 == f2 -> merge (CommentedTypeDecl f1 (ys1 ++ ys2) : xs)
   (CommentedDataDecl f1 ys1 cs1, CommentedDataDecl f2 ys2 cs2)
     | f1 == f2 -> merge (CommentedDataDecl f1 (ys1 ++ ys2)
                                            (zipWith zipCons cs1 cs2) : xs)
   (CommentedNewtypeDecl f1 ys1 cn cc1 m1, CommentedNewtypeDecl f2 ys2 _ cc2 m2)
     | f1 == f2 -> merge (CommentedNewtypeDecl f1 (ys1 ++ ys2) cn
                                               (cc1 ++ cc2) (mMaybe m1 m2) : xs)
   (CommentedFunctionDecl f1 ys1, CommentedFunctionDecl f2 ys2)
     | f1 == f2 -> merge (CommentedFunctionDecl f1 (ys1 ++ ys2) : xs)
   (CommentedTypeSig f1 ys1 ps1, CommentedTypeSig f2 ys2 ps2)
     | f1 == f2 -> merge (CommentedTypeSig f1 (ys1 ++ ys2)
                                              (zipWith zipPair ps1 ps2) : xs)
   (CommentedClassDecl f1 ys1 ds1, CommentedClassDecl f2 ys2 ds2)
     | f1 == f2 -> merge (CommentedClassDecl f1 (ys1 ++ ys2)
                                                (merge (ds1 ++ ds2)) : xs)
   (CommentedInstanceDecl f1 ty1 ys1 ds1, CommentedInstanceDecl f2 ty2 ys2 ds2)
     | ty1 == ty2 &&
       f1 == f2 -> merge (CommentedInstanceDecl f1 ty1 (ys1 ++ ys2)
                                                (merge (ds1 ++ ds2)) : xs)
   _ -> x1 : merge (x2 : xs)

  where zipCons (a1, b1, c1) (a2, b2, c2)
          | a1 == a2  = (a1, b1 ++ b2, zipWith zipPair c1 c2)
          | otherwise = error ("Comment.merge.zipCons: " ++ show a1
                                 ++ ", " ++ show a2)
        zipPair (a1, b1) (a2, b2)
          | a1 == a2  = (a1, b1 ++ b2)
          | otherwise = error ("Comment.merge.zipPair: " ++ show a1
                                 ++ ", " ++ show a2)
        mMaybe Nothing       Nothing       = Nothing
        mMaybe (Just x)      Nothing       = Just x
        mMaybe Nothing       (Just y)      = Just y
        mMaybe (Just (f, x)) (Just (_, y)) = Just (f, x ++ y)

skipUntilAfter :: Span -> [(Span, a)] -> [(Span, a)]
skipUntilAfter sp = filter (( `isAfter` sp) . fst)

classifyComment :: Comment -> CDocComment
classifyComment c@(NestedComment s) | "{- |" `isPrefixOf` s = Pre  c
                                    | "{- ^" `isPrefixOf` s = Post c
                                    | otherwise             = None c
classifyComment c@(LineComment   s) | "-- |" `isPrefixOf` s = Pre  c
                                    | "-- ^" `isPrefixOf` s = Post c
                                    | otherwise             = None c
