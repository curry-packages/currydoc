{- |
     Description: An example module for CurryDoc
     Category   : Example
     Author     : Kai-Oliver Prott,
                  John Doe
     Version    : September 2018

     This Module shows how every Declaration can be documented.
     All comments can be formatted using _markdown_.

     The supported markdown syntax can be found at
     <https://www.informatik.uni-kiel.de/~pakcs/markdown_syntax.html>.
-}
module Guide (
  -- * Some datatypes
  List(..), Tree(..), MyExternal, MyInt, MyString,
  -- ** Operations on those datatypes
  nullL, nullT, (:++:), (:+:),
  -- ** Typeclasses
  Monoid(..),
  -- *** Re-exported typclasses
  Num(..),
  -- * Re-exported modules
  module Prelude,
  module List
) where
-- ^ Nice!

-- The documentation will include a link to fully
-- re-exported modules (like the Prelude in this example).
import Prelude
-- If a re-exported module is imported with any
-- specification, all entities in scope will be
-- inlined into the documentation.
import List hiding ()

-- | This kind of comment can be used to document the following declaration.
--   It can be longer than one line. Only the first comment needs the special
-- | symbol at the beginning, but others can have it too.
nullL :: List a -> Bool
nullL Nil        = True
nullL (Cons _ _) = False

nullT :: Tree a -> Bool
nullT Empty      = True
nullT (Leaf _)   = False
nullT (Bin _ _) = False
-- ^ This kind of comment can be used to document the preceding declaration.
--   Normally this is not used to document functions.

-- | This example shows how constructors can be documented:
data List a = Nil             -- ^ The ``Nil`` constructor for empty lists
            | Cons a (List a) -- ^ The ``Cons`` constructor for non-empty lists
  deriving (Eq, Ord, Show, Read)

-- | Newtypes can be documented just like regular datatypes.
newtype MyInt = MyInt {
                  theInt :: Int -- ^ Record fields can be documented
                } -- ^ This is for the constructor
  deriving (Eq, Ord, Show)
  -- ^ This is for the newtype again!

{- | Both types of doc-comments are available in nested and single-line style.-}
external data MyExternal
{- ^ See? -}

-- | Did you notice, that the order of definition is irrelevant?
--   The ordering is given by our export list.
data Tree a = Empty | Leaf a
            | Bin (Tree a) (Tree a) -- ^ Not every constructor needs comments.
  deriving Eq

-- | concatenation on lists
(:++:) :: List a -- ^ Parameters can be documented like this
  {- | or this (ugly!) -} -> List a
       -> List a -- ^ return type
Nil       :++: Nil       = Nil
Nil       :++: Cons y ys = Cons y ys
Cons x xs :++: Nil       = Cons x xs
Cons x xs :++: Cons y ys = Cons x (xs :++: Cons y ys)

-- A declaration dows not need comments, to be in the final documentation
(:+:) :: Tree a -> Tree a -> Tree a
-- External definition are also supported, but there is no external definition
-- for this specific function.
-- (:+:) external
-- | Thanks to the Curry Analysis Server System,
--   partial definition will have a special annotation in the
--   final documentation
Empty :+: Empty = Empty

-- | Reference to another documented entity: 'Prelude.String'
type MyString = List Char

-- | Typeclasses can be documented!
class Monoid a where
  mempty :: a
  mplus :: a -> a -> a

-- Instances cannot be documented
instance Monoid MyInt where
  mempty = MyInt 0
  MyInt a `mplus` MyInt b = MyInt (a + b)

-- Infix declarations cannot be documented
infixr 7 :++:, :+:
infixl 5 `mplus`

-- Default declarations cannot be documented
default (Int)
