----------------------------------------------------------------------
--- Functions to generate documentation in "CDoc" format.
--- JUST A PLACEHOLDER MODULE
---
--- @author Kai-Oliver Prott
--- @version June 2018
----------------------------------------------------------------------

module CurryDoc.Generators.CDoc (generateCDoc) where

import CurryDoc.Info (CurryDoc)

generateCDoc :: DocOptions -> CurryDoc -> IO String
generateCDoc _ _ = putStrLn e >> return e
  where e = "CDoc is currently not supported!"
