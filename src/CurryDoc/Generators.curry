{- |
     Author  : Kai-Oliver Prott
     Version : August 2018

     A module containing all available backends of CurryDoc
-}
module CurryDoc.Generators
  (module CurryDoc.Generators.JSON,
   module CurryDoc.Generators.TeX,
   module CurryDoc.Generators.Html) where

import CurryDoc.Generators.JSON
import CurryDoc.Generators.TeX
import CurryDoc.Generators.Html
