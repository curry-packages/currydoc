{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

{- |
    Description: A simple module for MPTC and other language tests for CurryDoc.
    Category   : general
    Author     : Lasse Züngel 
    Version    : March 2025
 
    Description and description and description. 
-}
module Example2 (MyMaybe(..), MyTypeSynonym1, myOperation1, Coerce()) where

import Data.Maybe    ( fromMaybe )
import Data.List     ( intersperse )
import Control.Monad ( Monad(..) )

-- | The 'MyMaybe' type encapsulates an optional value.
data MyMaybe a = MyNothing | MyJust a

-- | Simple type synonym example.
type MyTypeSynonym1 = MyMaybe Int

-- | Another example with a type parameter.
type MyTypeSynonym2 a = MyMaybe [a]

--- An old-style constructor.
--- @cons MyOldCons1 The first constructor
--- @cons MyOldCons2 The second constructor
data OldStyleCons = MyOldCons1
                  | MyOldCons2

-- | A new-style constructor.
data NewStyleCons = MyNewCons1 -- ^ The first constructor
                  | MyNewCons2 -- ^ The second constructor

--- We can also mix the styles.
--- @cons MyMixedCons1 The first constructor
data MixedStyleCons = MyMixedCons1 -- ^ ... is this one.
                    | MyMixedCons2 -- ^ The second constructor

-- | A simple function.
myOperation1 :: MyMaybe Int -> MyMaybe Int
myOperation1 MyNothing  = MyNothing
myOperation1 (MyJust a) = MyJust (a + 1)

-- | Another simple function.
myOperation2 :: a -> [b]
myOperation2 _ = []

-- | A nondeterministic operation.
myOperation3 :: a -> a -> a
myOperation3 = (?)

--- A function with an old-style comment.
--- @param x The input value
--- @return Some result
oldStyleOperation :: Int -> Int
oldStyleOperation x = 42 + x

-- | A function that always fails.
--
-- - This is a list
-- - of bullet points
-- 
-- ```
-- This is some code
-- ```
--
-- This is a new paragraph with inlined `code`, and also a link to [Google](https://www.google.com).
--
myOperation4 :: a -> b
myOperation4 _ = failed

data WithRecords a = WithRecords { runSomething :: a -> Int }

infixr 5 :+:

data MyExpr = MyInt Int 
            | MyExpr :+: MyExpr

instance Functor MyMaybe where
  fmap _ MyNothing  = MyNothing
  fmap f (MyJust a) = MyJust (f a)

class Coerce a b where
  coerce :: a -> b

instance Coerce (MyMaybe a) [a] where
  coerce MyNothing  = []
  coerce (MyJust a) = [a]

instance Coerce (MyMaybe a) (MyMaybe a) where
  coerce = id

class MPTC a b | a -> b where
  mptcOp :: a -> b

instance MPTC Int Bool where
  mptcOp _ = True

instance MPTC Bool Int where
  mptcOp _ = 42 + 73

class MPTC2 a b c | a -> b, b -> a, a -> , -> c, -> where
  mptcOp2 :: a -> b