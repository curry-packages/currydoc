{- |
     Author  : Kai-Oliver Prott
     Version : May 2025

     A module containing all available backends of CurryDoc
-}
module CurryDoc.Generators
  (module CurryDoc.Generators.JSON,
   module CurryDoc.Generators.TeX,
   module CurryDoc.Generators.Html,
   module CurryDoc.Generators.CDoc) where

import CurryDoc.Generators.JSON
import CurryDoc.Generators.TeX
import CurryDoc.Generators.Html
import CurryDoc.Generators.CDoc
