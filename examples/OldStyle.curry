--------------------------------------------------------------------------------
--- This Module contains documentation in the old style.
--- It is used to test the system for backwards compatibility
--- with the old style of documentation. For new modules, 
--- the new style should be used instead.
---
--- @description An example module for CurryDoc
--- @category Example
--- @version March 2025
--- @author Guy Incognito,
---         Lasse Züngel
--------------------------------------------------------------------------------

module OldStyle where 

-- | A simple constant with a markdown code block:
--   
--       x :: Int
--       x = 42
-- 
--   @return The answer to the ultimate question of life, the universe, and everything.
--
--   Text behind the code block.
code :: Int
code = 42

--- A nondeterministic value.
nondet :: Int
nondet = 42 ? 73

--- A simple function (old style).
--- @param x - The first argument
--- @param y - The second argument
--- @return  - The result
myFun1 :: Int -> Int -> Int
myFun1 x y = if x > y then x else y
 
--- A simple function (old style). 
--- @param x The first argument
--- @param y The second argument
--- @return The result
myFun2 :: Int -> Int -> Int
myFun2 x y = if x > y then x else y

--- A simple function where parts of the 
--- parameters are omitted (old style). 
--- @param x The first argument
---          (multiline) 
--- @return The result 
---         is also documented
---         using multiline comments.
myFun3 :: Int -> Int -> Int
myFun3 x y = if x > y then x else y

--- A simple function (old style) without any param comments.
--- @return The result
--- @return - Another result comment (omit this).
--- @cons MyCons1 The first constructor (omit this). 
myFun4 :: Int -> Int -> Int
myFun4 x y = if x > y then x else y

-- | A simple function (haddock style).
myFunH :: Int -- ^ The first argument 
       -> Int -- ^ The second argument
              --   (multiline)
       -> Int -- ^ The result 
myFunH x y = if x > y then x else y 

--- A simple datatype.
--- @cons MyCons1 The first constructor 
--- @cons MyCons2 The second constructor
--- @cons MyCons3 The third constructor with an argument
--- The @cons annotations can also be placed between
--- comments! 
data MyData = MyCons1 
            | MyCons2 
            | MyCons3 Int

--- A simple type synonym.
type MyType = Int 

--- A simple newtype.
--- @cons MyNewType The constructor
newtype MyNewType = MyNewType Int

instance Show MyNewType where
  show (MyNewType x) = "MyNewType " ++ show x