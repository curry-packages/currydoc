----------------------------------------------------------------------
--- Datatype and operations to handle analysis information in CurryDoc.
---
--- @author Michael Hanus
--- @version April 2016
----------------------------------------------------------------------

module CurryDoc.Data.AnaInfo
  (AnaInfo(..), readAnaInfo, getNondetInfo, getCompleteInfo, getIndetInfo,
   getOpCompleteInfo, getFunctionInfo,
   AnalysisInfo(..), Property(..)) where

import Analysis.Deterministic
import Analysis.TotallyDefined
import Analysis.Indeterministic
import Analysis.SolutionCompleteness
import Analysis.Types (analysisName)
import CASS.Server    (initializeAnalysisSystem, analyzeInterface)

import FlatCurry.Types
import AbstractCurry.Types (CFixity, CRule)

import CurryDoc.Info.Goodies

-----------------------------------------------------------------------
-- Datatype for passing analysis results:

data AnaInfo =
   AnaInfo (QName -> Bool)          -- non-deterministic?
           (QName -> Completeness)  -- completely defined?
           (QName -> Bool)          -- indeterministically defined?
           (QName -> Bool)          -- solution complete?

-- read and generate all analysis infos:
readAnaInfo :: String -> IO AnaInfo
readAnaInfo modname = do
  initializeAnalysisSystem
  nondet   <- analyzeAndCheck nondetAnalysis
  complete <- analyzeAndCheck patCompAnalysis
  indet    <- analyzeAndCheck indetAnalysis
  solcomp  <- analyzeAndCheck solcompAnalysis
  return (AnaInfo (\qn -> nondet qn == NDet) complete indet solcomp)
 where
   analyzeAndCheck ana =
     analyzeInterface ana modname >>= either
       (\results -> return (\q -> getFunctionInfo results q))
       (\err -> error $ "Analysis error: " ++ err)

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
getFunctionInfo ((fn,fi):fnis) n = if fn =~= n then fi
                                               else getFunctionInfo fnis n

--------------------------------------------------------------------------

data AnalysisInfo = AnalysisInfo { nondet, indet, opComplete, ext :: Bool,
                                   complete :: Completeness,
                                   precedence :: Maybe (CFixity, Int),
                                   property :: [(Property, CRule)]
                                 }
                  | NoAnalysisInfo
                  | PrecedenceInfo { precedence :: Maybe (CFixity, Int)}
  deriving Show

data Property = PreSpec | PostSpec | Spec | Prop
  deriving Show

instance Show Completeness where
  show   Complete   =   "Complete"
  show InComplete   = "InComplete"
  show InCompleteOr = "InCompleteOr"
