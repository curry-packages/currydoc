module CurryDoc.Comment where

import CurryDoc.Span
import CurryDoc.SpanInfo
import CurryDoc.Type

import List

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


type CommentedDecl = (String, [Comment])
{-  = CommentedTypeDecl Ident [Comment]
  | CommentedDataDecl Ident [Comment] [CommentedConstrDecl]
  | CommentedNewtypeDecl Ident [Comment] CommentedNewtypeDecl
  | CommentedClassDecl Ident [Comment] [CommentedDecl]
  | CommentedInstanceDecl QualIdent InstanceType [CommentedDecl]
  | CommentedFunctionDecl Ident [Comment]
  | CommentedTypeSig Ident [Comment] [(TypeExpr, [Comment])]-}


readCommentFile :: String -> IO [(Span, Comment)]
readCommentFile s = readFile s >>= (return . read)

readASTFile :: String -> IO (Module ())
readASTFile s = readFile s >>= (return . read)

associateCurryDoc :: [(Span, Comment)] -> Module a -> ([CommentedDecl], [Comment])
associateCurryDoc []       _                       = ([], [])
associateCurryDoc xs@(_:_) (Module spi _ _ _ _ ds) =
  let (rest, result) = associateCurryDocModule spi sp xs'
  in  (merge $ associateCurryDocDecls rest ds Nothing, result)
  where xs' = map (\(sp',c) -> (sp', classifyComment c)) xs
        sp = case ds of
          (d:_) -> getSrcSpan d
          _     -> NoSpan

associateCurryDocModule :: SpanInfo                           -- ^ module SpanInfo
                        -> Span                               -- ^ first decl span
                        -> [(Span, CDocComment)]              -- ^ to be matched
                        -> ([(Span, CDocComment)], [Comment]) -- ^ (rest, matched)
associateCurryDocModule spi@(SpanInfo _ (spm : _)) sp (c:cs) =
  case c of
    (sp', Pre  c') | vertDist sp' spm >= 0 ->
      let (match, next)   = getToMatch spm sp' cs isPre
          (rest, matched) = associateCurryDocModule spi sp next
      in (rest, c' : ((map (comment . snd) match) ++ matched))
                   | otherwise             ->
      let (rest, matched) = associateCurryDocModule spi sp cs
      in (c:rest, matched)

    (sp', Post c') | vertDist sp' sp  >= 1
                       || isNoSpan sp      ->
      let (match, next)   = getToMatch sp sp' cs isPost
          (rest, matched) = associateCurryDocModule spi sp next
      in (rest, c' : ((map (comment . snd) match) ++ matched))
                   | otherwise             ->
      (c:cs, [])

    (_  , None _)                          ->
      associateCurryDocModule spi sp cs
associateCurryDocModule (SpanInfo _ []) _ (c:cs) = (c:cs, [])
associateCurryDocModule NoSpanInfo      _ (c:cs) = (c:cs, [])
associateCurryDocModule _               _ []     = ([]  , [])

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
  if (vertDist sp stop >= 0 || isNoSpan stop)           -- pos is ok
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
  let (match, next) = getToMatch (getSrcSpan d) sp cs isPre
  in  associateCurryDocDeclPre ((sp, Post c) : match) d
        : matchLast next (Just d)
matchLast ((_ , None _) : cs) (Just d) = matchLast cs (Just d)
matchLast ((_ , Pre  _) : cs) (Just d) = matchLast cs (Just d)

associateCurryDocDeclPre :: [(Span, CDocComment)]
                         -> Decl a
                         -> CommentedDecl
associateCurryDocDeclPre xs (DataDecl _ f _ _ _) = (show f, map comment $
                                                            snd         $
                                                            unzip xs)
associateCurryDocDeclPost :: [(Span, CDocComment)]
                          -> Decl a
                          -> CommentedDecl
associateCurryDocDeclPost xs (DataDecl _ f _ _ _) = (show f, map comment $
                                                             snd         $
                                                             unzip xs)

merge :: [CommentedDecl] -> [CommentedDecl]
merge []                                 = []
merge [x]                                = [x]
merge ((x1, y1):(x2, y2):xs) | x1 == x2  = merge ((x1, y1 ++ y2):xs)
                             | otherwise = (x1, y1) : merge ((x2, y2):xs)

classifyComment :: Comment -> CDocComment
classifyComment c@(NestedComment s) | "{- |" `isPrefixOf` s = Pre  c
                                    | "{- ^" `isPrefixOf` s = Post c
                                    | otherwise             = None c
classifyComment c@(LineComment   s) | "-- |" `isPrefixOf` s = Pre  c
                                    | "-- ^" `isPrefixOf` s = Post c
                                    | otherwise             = None c
