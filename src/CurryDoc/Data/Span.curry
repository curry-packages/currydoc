{- |
     Author  : Kai-Oliver Prott
     Version : August 2018

     Datatype and operations to handle Spans.
-}
module CurryDoc.Data.Span (
 Span(..),
 -- * Selectors
 start, end,
 -- * Transformer,
 isSpan, isNoSpan, fromPosition, stripStart,
 -- * Distance management
 vertDist, isAfter, isBefore, isBeforeList
) where

import CurryDoc.Data.Position

data Span = Span Position Position -- ^ Span StarPos EndPos
          | NoSpan
  deriving (Eq, Show, Read)

start :: Span -> Position
start NoSpan      = NoPos
start (Span st _) = st

end :: Span -> Position
end NoSpan      = NoPos
end (Span _ ed) = ed

isSpan :: Span -> Bool
isSpan (Span _ _) = True
isSpan NoSpan     = False

isNoSpan :: Span -> Bool
isNoSpan (Span _ _) = False
isNoSpan NoSpan     = True

fromPosition :: Position -> Span
fromPosition NoPos            = NoSpan
fromPosition p@(Position _ _) = Span p p

stripStart :: Span -> Span
stripStart = fromPosition . end

-- | Computes a "vertical distance" between two spans.
-- It is either the row distance of the start end end positions or
-- zero, if the spans overlap.
vertDist :: Span -> Span -> Int
vertDist NoSpan       NoSpan       = 0
vertDist NoSpan       (Span _  _ ) = 0
vertDist (Span _  _ ) NoSpan       = 0
vertDist (Span s1 e1) (Span s2 e2) =
  case rowDist e1 s2 of
    x | x >= 0    -> x
      | e1 <= e2  -> 0 -- they overlap
      | otherwise -> - (rowDist e2 s1)

-- | Checks if the first span is completely after the second span.
isAfter :: Span -> Span -> Bool
isAfter NoSpan     NoSpan     = False
isAfter (Span _ _) NoSpan     = False
isAfter NoSpan     (Span _ _) = False
isAfter (Span s _) (Span _ e) = s >= e

-- | Checks if the first span is completely before the second span.
isBefore :: Span -> Span -> Bool
isBefore NoSpan     NoSpan     = False
isBefore (Span _ _) NoSpan     = False
isBefore NoSpan     (Span _ _) = False
isBefore (Span _ e) (Span s _) = e <= s

-- | Checks if the first span is completely before the first span of a list.
--   Reurns `True` if the list is empty
isBeforeList :: Span -> [Span] -> Bool
isBeforeList _   []      = True
isBeforeList sp1 (sp2:_) = isBefore sp1 sp2
