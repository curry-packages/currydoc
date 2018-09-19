module NoExportListExample where

-- | Due to current limitations, the order of declarations is a bit different
--   when no explicit export list is given.
data DataTypeA

data DatatypeB

class TypeclassA a where
  funA1 :: a -> a
  funA2 :: a -> a -> a

class TypeclassB b where
  funB1 ::      b -> Bool
  funB2 :: b -> b -> Bool

operationA = (+)
operationB = (-)
