{- |
     Author  : Kai-Oliver Prott
     Version : August 2018

     Datatype and operations to handle Positions.
-}
module CurryDoc.Data.Position where

data Position = Position Int Int
              | NoPos
  deriving (Eq, Ord, Show, Read)

-- | Diestance between the row of the first and the second argument
rowDist :: Position -> Position -> Int
rowDist NoPos           NoPos           = 0
rowDist (Position _  _) NoPos           = 0
rowDist NoPos           (Position _  _) = 0
rowDist (Position r1 _) (Position r2 _) = r2 - r1
