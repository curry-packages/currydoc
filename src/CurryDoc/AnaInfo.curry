----------------------------------------------------------------------
--- Datatype and operations to handle analysis information in CurryDoc.
---
--- @author Michael Hanus
--- @version April 2016
----------------------------------------------------------------------

module CurryDoc.AnaInfo where

import FlatCurry.Types
import Analysis.TotallyDefined(Completeness(..))

-----------------------------------------------------------------------
-- Datatype for passing analysis results:

data AnaInfo =
   AnaInfo (QName -> Bool)          -- non-deterministic?
           (QName -> Completeness)  -- completely defined?
           (QName -> Bool)          -- indeterministically defined?
           (QName -> Bool)          -- solution complete?

getNondetInfo :: AnaInfo -> QName -> Bool
getNondetInfo (AnaInfo oi _ _ _) = oi

getCompleteInfo :: AnaInfo -> QName -> Completeness
getCompleteInfo (AnaInfo _ cdi _ _) = cdi

getIndetInfo :: AnaInfo -> QName -> Bool
getIndetInfo (AnaInfo _ _ idi _) = idi

getOpCompleteInfo :: AnaInfo -> QName -> Bool
getOpCompleteInfo (AnaInfo _ _ _ oci) = oci

-- Translate a standard analysis result into functional form:
getFunctionInfo :: [(QName,a)] -> QName -> a
getFunctionInfo [] n = error ("No analysis result for function "++show n)
getFunctionInfo ((fn,fi):fnis) n = if fn == n then fi
                                              else getFunctionInfo fnis n


--------------------------------------------------------------------------
