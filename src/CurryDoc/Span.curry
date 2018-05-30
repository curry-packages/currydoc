module CurryDoc.Span where

import CurryDoc.Position

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
