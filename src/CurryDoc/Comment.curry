module CurryDoc.Comment
  (readComments, readCommentFile, associateCurryDoc,
   cleanup, addAnaInfoToCommentDecls, addAbstractCurryProg) where

import CurryDoc.Span
import CurryDoc.SpanInfo
import CurryDoc.Type
import CurryDoc.Ident
import CurryDoc.AnaInfo

import AbstractCurry.Types

import List
import Maybe (listToMaybe, mapMaybe)
import Char (isSpace)
import Distribution

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
  = CommentedTypeDecl QName [CTVarIName] CTypeExpr [Comment]
  | CommentedDataDecl QName [CTVarIName] [Comment] [CommentedConstr]
  | CommentedNewtypeDecl QName [CTVarIName] [Comment] (Maybe CommentedNewtypeConstr)
  | CommentedClassDecl QName CContext CTVarIName [Comment] [CommentedDecl]
  | CommentedInstanceDecl QName CContext CTypeExpr [Comment] [CommentedDecl]
  | CommentedFunctionDecl QName [Comment] (Maybe CQualTypeExpr) AnalysisInfo
  | CommentedTypeSig [QName] [Comment] CContext [(CTypeExpr, [Comment])]
  | UnsupportedDecl [Comment]
  deriving Show

data CommentedConstr
  = CommentedConstr QName [Comment] [CTypeExpr] AnalysisInfo
  | CommentedRecord QName [Comment] [CTypeExpr] [CommentedField] AnalysisInfo
  | CommentedConsOp QName [Comment] CTypeExpr CTypeExpr AnalysisInfo
  deriving Show

type CommentedField = ([QName], [Comment], CTypeExpr)

data CommentedNewtypeConstr
  = CommentedNewConstr QName [Comment] CTypeExpr AnalysisInfo
  | CommentedNewRecord QName [Comment] CTypeExpr (Maybe CommentedField) AnalysisInfo
  deriving Show

readCommentFile :: String -> IO [(Span, Comment)]
readCommentFile s = readFile s >>= (return . read)

readComments :: String -> IO [(Span, Comment)]
readComments modl = do ss <- readFile (".curry/" ++ modNameToPath modl ++ ".cycom")
                       return $ read ss

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
  in  CommentedFunctionDecl (identToQName f) (map (comment . snd) match)
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
associateCurryDocDeclPost xs (ExternalDecl _ _) = UnsupportedDecl
  (map (comment . snd) xs)
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
   (CommentedTypeSig f1 ys1 cx ps1, CommentedTypeSig f2 ys2 _ ps2)
     | f1 == f2 -> merge (CommentedTypeSig f1 (ys1 ++ ys2) cx
                                              (zipWith zipPair ps1 ps2) : xs)
   (CommentedClassDecl f1 cx1 v1 ys1 ds1, CommentedClassDecl f2 _ _ ys2 ds2)
     | f1 == f2 -> merge (CommentedClassDecl f1 cx1 v1 (ys1 ++ ys2)
                                             (merge (ds1 ++ ds2)) : xs)
   (CommentedInstanceDecl f1 cx1 ty1 ys1 ds1, CommentedInstanceDecl f2 _ ty2 ys2 ds2)
     | ty1 == ty2 &&
       f1 == f2 -> merge (CommentedInstanceDecl f1 cx1 ty1 (ys1 ++ ys2)
                                                (merge (ds1 ++ ds2)) : xs)
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

classifyComment :: Comment -> CDocComment
classifyComment (NestedComment s)
  | "{- |" `isPrefixOf` s = Pre  $ NestedComment $ dropWhile isSpace $ drop 4 s
  | "{- ^" `isPrefixOf` s = Post $ NestedComment $ dropWhile isSpace $ drop 4 s
  | otherwise             = None $ NestedComment $ dropWhile isSpace          s
classifyComment (LineComment   s)
  | "-- |" `isPrefixOf` s = Pre  $ LineComment   $ dropWhile isSpace $ drop 4 s
  | "---"  `isPrefixOf` s = Pre  $ LineComment   $ dropWhile isSpace $ drop 3 s
  | "-- ^" `isPrefixOf` s = Post $ LineComment   $ dropWhile isSpace $ drop 4 s
  | otherwise             = None $ LineComment   $ dropWhile isSpace          s

identToQName :: Ident ->  QName
identToQName (Ident _ s _) = ("", s)

qIdentToQName :: QualIdent -> QName
qIdentToQName (QualIdent _ Nothing   idt) = identToQName idt
qIdentToQName (QualIdent _ (Just mi) idt) = (intercalate "." ms, n)
  where (_, n) = identToQName idt
        ModuleIdent _ ms = mi

(=~=) :: QName -> QName -> Bool
(   ""   , x) =~= (   ""   , y) = x == y
(   ""   , x) =~= (   (_:_), y) = x == y
(   (_:_), x) =~= (   ""   , y) = x == y
(xs@(_:_), x) =~= (ys@(_:_), y) = (xs, x) == (ys, y)

typeExprToCType :: TypeExpr -> CTypeExpr
typeExprToCType (ParenType       _ t1   ) = typeExprToCType t1 -- TODO: is this ok?
typeExprToCType (VariableType    _ n    ) = CTVar (tvIName n)
typeExprToCType (ApplyType       _ t1 t2) =
  CTApply (typeExprToCType t1) (typeExprToCType t2)
typeExprToCType (ArrowType       _ t1 t2) =
  CFuncType (typeExprToCType t1) (typeExprToCType t2)
typeExprToCType (ConstructorType _ qid  ) = CTCons (qIdentToQName qid)
typeExprToCType (ListType        _ t1   ) =
  CTApply (CTCons ("", "[]")) (typeExprToCType t1)
typeExprToCType (TupleType       _ tys  ) =
  foldl (\b a -> CTApply b (typeExprToCType a))
        (CTCons ("", "(" ++ replicate (length tys - 1) ',' ++ ")")) tys

tvIName :: Ident -> CTVarIName
tvIName n = (0, snd $ identToQName n)

(=~~=) :: CTypeExpr -> CTypeExpr -> Bool
a =~~= b = case (a,b) of
  (CTVar (_, n1), CTVar (_, n2)) -> n1 == n2
  _                              -> a  == b

contextToCContext :: Context -> CContext
contextToCContext cs = CContext (map constraintToCConstraint cs)

constraintToCConstraint :: Constraint -> CConstraint
constraintToCConstraint (Constraint _ qid ty) =
  (qIdentToQName qid, typeExprToCType ty)

getConstrName   :: ConstrDecl -- ^ ConstrDecl
  {- | Ident -} -> QName
getConstrName (ConstrDecl _ _ _   idt _) = identToQName idt
getConstrName (ConOpDecl  _ _ _ _ idt _) = identToQName idt
getConstrName (RecordDecl _ _ _   idt _) = identToQName idt

getNewtypeConstrName :: NewConstrDecl -- ^ NewConstrDecl
       {- | Ident -} -> QName
getNewtypeConstrName (NewConstrDecl _ idt _) = identToQName idt
getNewtypeConstrName (NewRecordDecl _ idt _) = identToQName idt

isCommentedTypeSig :: CommentedDecl -> Bool
isCommentedTypeSig d = case d of
  CommentedTypeSig _ _ _ _ -> True
  _                        -> False

cFieldType :: CFieldDecl -> CTypeExpr
cFieldType (CField _ _ ty) = ty

fieldType :: FieldDecl -> CTypeExpr
fieldType (FieldDecl _ _ ty) = typeExprToCType ty

isPublicField :: CFieldDecl -> Bool
isPublicField (CField _ Public  _ ) = True
isPublicField (CField _ Private _ ) = False

-------------------------------------------------------------------------------
-- Splitting of TypeSigs with multiple idents and field decls inside DataDecls
-- and filtering of UnsupportedDecls

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
cleanup (  (CommentedTypeSig     idts cs vs args) : ds) =
          map (\i -> CommentedTypeSig [i] cs vs args) idts  ++ cleanup ds

cleanupConstr :: CommentedConstr -> CommentedConstr
cleanupConstr c = case c of
  CommentedRecord f cs tys fs ai
    -> CommentedRecord f cs tys (concatMap cleanupField fs) ai
  _ -> c


cleanupField :: CommentedField -> [CommentedField]
cleanupField (ns, cs, ty) = map (\n -> ([n], cs, ty)) ns

-------------------------------------------------------------------------------
-- Remove unexported entities and
-- add exported entities that did not have any comments

addAbstractCurryProg :: CurryProg -> [CommentedDecl] -> [CommentedDecl]
addAbstractCurryProg (CurryProg _ _ _ cls inst typ func _) ds =
  let withcls  = addAbstractCurryClassesInfo cls ds
      withins  = addAbstractCurryInstInfo inst ds
      withtyp  = addAbstractCurryDataInfo typ ds
      withfun  = addAbstractCurryFunInfo func ds
      --typesigs = filter isCommentedTypeSig ds
  in withcls ++ withins ++ withtyp ++ withfun

addAbstractCurryClassesInfo :: [CClassDecl] -> [CommentedDecl] -> [CommentedDecl]
addAbstractCurryClassesInfo []                               _   = []
addAbstractCurryClassesInfo (CClass n Public  cx vn ds : cs) cds =
  maybe (CommentedClassDecl n cx vn [] (addAbstractCurryFunInfo ds []))
    id (lookupClass n cds) : addAbstractCurryClassesInfo cs cds
addAbstractCurryClassesInfo (CClass _ Private _  _  _  : cs) cds =
  addAbstractCurryClassesInfo cs cds

lookupClass :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupClass _ []     = Nothing
lookupClass n (d:ds) = case d of
  CommentedClassDecl n' _ _ _ _
    | n =~= n' -> Just d
  _            -> lookupClass n ds

addAbstractCurryInstInfo :: [CInstanceDecl] -> [CommentedDecl] -> [CommentedDecl]
addAbstractCurryInstInfo []                          _   = []
addAbstractCurryInstInfo (CInstance n cx ty ds : is) cds =
  maybe (CommentedInstanceDecl n cx ty [] (addAbstractCurryFunInfo ds []))
    id (lookupInstance n ty cds) : addAbstractCurryInstInfo is cds

lookupInstance :: QName -> CTypeExpr -> [CommentedDecl] -> Maybe CommentedDecl
lookupInstance _ _  []     = Nothing
lookupInstance n ty (d:ds) = case d of
  CommentedInstanceDecl n' _ ty' _ _
    | n =~= n' && ty =~~= ty' -> Just d
  _                           -> lookupInstance n ty ds

addAbstractCurryFunInfo :: [CFuncDecl] -> [CommentedDecl] -> [CommentedDecl]
addAbstractCurryFunInfo []                                 _   = []
addAbstractCurryFunInfo (CFunc     n _ Public  qty _ : ds) cds =
  maybe (CommentedFunctionDecl n [] (Just qty) NoAnalysisInfo)
    (setType qty) (lookupFunc n cds) : addAbstractCurryFunInfo ds cds
addAbstractCurryFunInfo (CmtFunc _ n _ Public  qty _ : ds) cds =
  maybe (CommentedFunctionDecl n [] (Just qty) NoAnalysisInfo)
    (setType qty) (lookupFunc n cds) : addAbstractCurryFunInfo ds cds
addAbstractCurryFunInfo (CFunc     _ _ Private _   _ : ds) cds =
  addAbstractCurryFunInfo ds cds
addAbstractCurryFunInfo (CmtFunc _ _ _ Private _   _ : ds) cds =
  addAbstractCurryFunInfo ds cds

setType :: CQualTypeExpr -> CommentedDecl -> CommentedDecl
setType qty f = case f of
  (CommentedFunctionDecl n cs _ ai) -> CommentedFunctionDecl n cs (Just qty) ai
  _                                 -> f

lookupFunc :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupFunc _ []     = Nothing
lookupFunc n (d:ds) = case d of
  CommentedFunctionDecl n' _ _ _
    | n =~= n' -> Just d
  _            -> lookupFunc n ds

addAbstractCurryDataInfo :: [CTypeDecl] -> [CommentedDecl] -> [CommentedDecl]
addAbstractCurryDataInfo []                               _   = []
addAbstractCurryDataInfo (CTypeSyn n Public vs ty   : ds) cds =
  maybe (CommentedTypeDecl n vs ty []) id (lookupTypeDecl n cds)
    : addAbstractCurryDataInfo ds cds
addAbstractCurryDataInfo (CNewType n Public vs con _ : ds) cds =
  maybe (CommentedNewtypeDecl n vs [] (createNewConsInfos con))
    (\(CommentedNewtypeDecl a b c d)
      -> CommentedNewtypeDecl a b c (filterNewCons con d))
    (lookupNewDecl n cds) : addAbstractCurryDataInfo ds cds
addAbstractCurryDataInfo (CType n Public vs cons _   : ds) cds =
  maybe (CommentedDataDecl n vs [] (createConsInfos cons))
    (\(CommentedDataDecl a b c d) -> CommentedDataDecl a b c
       (mapMaybe (filterCons cons) d))
    (lookupDataDecl n cds) : addAbstractCurryDataInfo ds cds
addAbstractCurryDataInfo (CTypeSyn _ Private _ _     : ds) cds =
  addAbstractCurryDataInfo ds cds
addAbstractCurryDataInfo (CNewType _ Private _ _ _   : ds) cds =
  addAbstractCurryDataInfo ds cds
addAbstractCurryDataInfo (CType _ Private _ _ _      : ds) cds =
  addAbstractCurryDataInfo ds cds

filterCons :: [CConsDecl] -> CommentedConstr -> Maybe CommentedConstr
filterCons [] _ = Nothing
filterCons (CCons _ _ n v _    : cs) c@(CommentedConstr n' _ _ _)
  | n =~= n' && v == Public = Just c
  | otherwise               = filterCons cs c
filterCons (CCons _ _ n v _    : cs) c@(CommentedConsOp n' _ _ _ _)
  | n =~= n' && v == Public = Just c
  | otherwise               = filterCons cs c
filterCons (CRecord _ _ n v fs : cs) c@(CommentedRecord n' cms tys fs' ai)
  | n =~= n' && v == Public =
    Just (CommentedRecord n' cms tys (filter (filterFields fs) fs') ai)
  | otherwise               = filterCons cs c
filterCons (CCons _ _ _ _ _    : cs) c@(CommentedRecord _ _ _ _ _)
  = filterCons cs c
filterCons (CRecord _ _ _ _ _  : cs) c@(CommentedConsOp _ _ _ _ _)
  = filterCons cs c
filterCons (CRecord _ _ _ _ _  : cs) c@(CommentedConstr   _ _ _ _)
  = filterCons cs c

filterFields :: [CFieldDecl] -> CommentedField -> Bool
filterFields [] _ = False
filterFields (CField n Public  _ : fs) f@([n'], _, _)
  = n =~= n' || filterFields fs f
filterFields (CField _ Public  _ : _) ([], _, _)
  = error "CurryDoc.filterFields: field with no qname"
filterFields (CField _ Public  _ : _) ((_:_:_), _, _)
  = error "CurryDoc.filterFields: field with more than one qname"
filterFields (CField _ Private _ : fs) f
  = filterFields fs f

filterNewCons :: CConsDecl -> Maybe CommentedNewtypeConstr -> Maybe CommentedNewtypeConstr
filterNewCons _ Nothing  = Nothing
filterNewCons c (Just c') =  case (c, c') of
  (CCons   _ _ _ Public _ , CommentedNewConstr _ _ _ _)
      -> Just c'
  (CRecord _ _ _ Public fs, CommentedNewRecord n' a ty f b)
      -> Just (CommentedNewRecord n' a ty f' b)
    where f' = case f of
                 Nothing -> Nothing
                 Just x | any isPublicField fs -> Just x
                        | otherwise            -> Nothing
  _ -> Nothing

lookupTypeDecl :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupTypeDecl _ []     = Nothing
lookupTypeDecl n (d:ds) = case d of
  CommentedTypeDecl n' _ _ _
    | n =~= n' -> Just d
  _            -> lookupTypeDecl n ds

lookupDataDecl :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupDataDecl _ []     = Nothing
lookupDataDecl n (d:ds) = case d of
  CommentedDataDecl n' _ _ _
    | n =~= n' -> Just d
  _            -> lookupDataDecl n ds

lookupNewDecl :: QName -> [CommentedDecl] -> Maybe CommentedDecl
lookupNewDecl _ []     = Nothing
lookupNewDecl n (d:ds) = case d of
  CommentedNewtypeDecl n' _ _ _
    | n =~= n' -> Just d
  _            -> lookupNewDecl n ds

createConsInfos :: [CConsDecl] -> [CommentedConstr]
createConsInfos [] = []
createConsInfos (CCons _ _ n Public tys : cs) =
  CommentedConstr n [] tys NoAnalysisInfo : createConsInfos cs
createConsInfos (CRecord _ _ n Public fs : cs) =
  CommentedRecord n [] (map cFieldType fs)
    (map createFieldInfo (filter isPublicField fs))
    NoAnalysisInfo : createConsInfos cs
createConsInfos (CCons _ _ _ Private _ : cs) =
  createConsInfos cs
createConsInfos (CRecord _ _ _ Private _ : cs) =
  createConsInfos cs

createFieldInfo :: CFieldDecl -> CommentedField
createFieldInfo (CField n _ ty) = ([n], [], ty)

createNewConsInfos :: CConsDecl -> Maybe CommentedNewtypeConstr
createNewConsInfos (CCons _ _ n Public tys) =
  Just $ CommentedNewConstr n [] (head tys) NoAnalysisInfo
createNewConsInfos (CRecord _ _ n Public fs) =
  Just $ CommentedNewRecord n [] (head (map cFieldType fs))
           (listToMaybe (map createFieldInfo (filter isPublicField fs)))
           NoAnalysisInfo
createNewConsInfos (CCons _ _ _ Private _) = Nothing
createNewConsInfos (CRecord _ _ _ Private _) = Nothing

-------------------------------------------------------------------------------
-- Collecting instance informations


-------------------------------------------------------------------------------
-- Analysis Info conslidation

addAnaInfoToCommentDecls :: AnaInfo -> [COpDecl] -> [CFuncDecl]
                         -> [CommentedDecl] -> [CommentedDecl]
addAnaInfoToCommentDecls ai cop funs = map (addAnaInfoToCommentDecl ai cop funs)

addAnaInfoToCommentDecl :: AnaInfo -> [COpDecl] -> [CFuncDecl] -> CommentedDecl
                        -> CommentedDecl
addAnaInfoToCommentDecl _  _   _    d@(UnsupportedDecl                _) = d
addAnaInfoToCommentDecl _  _   _    d@(CommentedTypeDecl        _ _ _ _) = d
addAnaInfoToCommentDecl _  _   _    d@(CommentedTypeSig         _ _ _ _) = d
addAnaInfoToCommentDecl _  _   _    d@(CommentedInstanceDecl  _ _ _ _ _) = d
addAnaInfoToCommentDecl _  cop _      (CommentedClassDecl     a b c d e) =
  CommentedClassDecl a b c d (map (addPrecedenceInfoToCommentDecl cop) e)
  -- CommentedClassDecl a b (addAnaInfoToCommentDecls ai cop c) -- not reasonably possible for classes
addAnaInfoToCommentDecl ai cop funs    (CommentedFunctionDecl n qty cs _) =
  CommentedFunctionDecl n qty cs (createAnalysisInfoFun ai cop funs n)
addAnaInfoToCommentDecl _  cop _       (CommentedDataDecl     idt vs cs cns) =
  CommentedDataDecl idt vs cs (map add cns)
  where add (CommentedConstr c ccs tys _) =
             CommentedConstr c ccs tys (createPrecInfo cop c)
        add (CommentedRecord c ccs tys fs  _) =
             CommentedRecord c ccs tys fs (createPrecInfo cop c)
        add (CommentedConsOp c ccs ty1 ty2 _) =
             CommentedConsOp c ccs ty1 ty2 (createPrecInfo cop c)
addAnaInfoToCommentDecl _  cop _       (CommentedNewtypeDecl idt vs cs cns) =
  CommentedNewtypeDecl idt vs cs (fmapMaybe add cns)
  where add (CommentedNewConstr c ccs ty _) =
             CommentedNewConstr c ccs ty (createPrecInfo cop c)
        add (CommentedNewRecord c ccs ty f  _) =
             CommentedNewRecord c ccs ty f (createPrecInfo cop c)
        fmapMaybe _ Nothing  = Nothing
        fmapMaybe f (Just x) = Just  (f x)

addPrecedenceInfoToCommentDecl :: [COpDecl] -> CommentedDecl -> CommentedDecl
addPrecedenceInfoToCommentDecl cop d = case d of
  CommentedFunctionDecl n qty cs _ ->
    CommentedFunctionDecl n qty cs (createPrecInfo cop n)
  _                                -> d

createAnalysisInfoFun :: AnaInfo -> [COpDecl] -> [CFuncDecl] -> QName
                      -> AnalysisInfo
createAnalysisInfoFun ai cop funs n = AnalysisInfo {
    nondet = getNondetInfo ai n,
    indet  = getIndetInfo ai n,
    opComplete = getOpCompleteInfo ai n,
    complete = getCompleteInfo ai n,
    ext = getExternalInfo funs n,
    precedence = genPrecedenceInfo cop n
  }

getExternalInfo :: [CFuncDecl] -> QName -> Bool
getExternalInfo []                             _
  = error "CurryDoc.Comment.getExternalInfo: Function not found!"
getExternalInfo (CFunc     n _ _ _ []    : fs) n'
  | n =~= n'  = True
  | otherwise = getExternalInfo fs n'
getExternalInfo (CmtFunc _ n _ _ _ []    : fs) n'
  | n =~= n'  = True
  | otherwise = getExternalInfo fs n'
getExternalInfo (CFunc     n _ _ _ (_:_) : fs) n'
  | n =~= n'  = False
  | otherwise = getExternalInfo fs n'
getExternalInfo (CmtFunc _ n _ _ _ (_:_) : fs) n'
  | n =~= n'  = False
  | otherwise = getExternalInfo fs n'

createPrecInfo :: [COpDecl] -> QName -> AnalysisInfo
createPrecInfo cop n = PrecedenceInfo {
    precedence = genPrecedenceInfo cop n
  }

genPrecedenceInfo :: [COpDecl] -> QName -> Maybe (CFixity, Int)
genPrecedenceInfo []                     _ = Nothing
genPrecedenceInfo (COp m fix prec : cop) n
  | n =~= m   = Just (fix, prec)
  | otherwise = genPrecedenceInfo cop n
