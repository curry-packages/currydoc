module CurryDoc.Data.Span where

import CurryDoc.Data.Position

data Span = Span Position Position
          | NoSpan
  deriving (Eq, Show, Read)

isSpan :: Span -> Bool
isSpan (Span _ _) = True
isSpan NoSpan     = False

isNoSpan :: Span -> Bool
isNoSpan (Span _ _) = False
isNoSpan NoSpan     = True

vertDist :: Span -> Span -> Int
vertDist NoSpan       NoSpan       = 0
vertDist NoSpan       (Span _  _ ) = 0
vertDist (Span _  _ ) NoSpan       = 0
vertDist (Span _  e1) (Span s2 e2) =
  case rowDist e1 s2 of
    x | x >= 0    -> x
      | otherwise -> if e1 < e2
                       then 0 -- they overlap
                       else x

isAfter :: Span -> Span -> Bool
isAfter NoSpan     NoSpan     = False
isAfter (Span _ _) NoSpan     = False
isAfter NoSpan     (Span _ _) = False
isAfter (Span s _) (Span _ e) = s >= e

isBefore :: Span -> Span -> Bool
isBefore NoSpan     NoSpan     = False
isBefore (Span _ _) NoSpan     = False
isBefore NoSpan     (Span _ _) = False
isBefore (Span _ e) (Span s _) = e <= s
