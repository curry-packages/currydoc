{- | Functions to generate documentation in "CDoc" format.
     JUST A PLACEHOLDER MODULE -}

module CurryDoc.Generators.CDoc (generateCDoc) where

import CurryDoc.Info (CurryDoc)

-- | Generates the documentation of a module in CDOC format 
generateCDoc :: DocOptions -> CurryDoc -> IO String
generateCDoc _ _ = putStrLn e >> return e
  where e = "CDoc is currently not supported!"
