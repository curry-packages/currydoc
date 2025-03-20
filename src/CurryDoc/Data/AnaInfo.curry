{- |
     Author  : Michael Hanus, Kai-Oliver Prott
     Version : March 2025

     Datatype and operations to handle analysis information in CurryDoc.
-}
module CurryDoc.Data.AnaInfo
  ( AnaInfo(..), readAnaInfo, getNondetInfo, getCompleteInfo, getIndetInfo,
    getOpCompleteInfo, getFunctionInfo, AnalysisInfo(..), Property(..) )
 where

import Analysis.Deterministic
import Analysis.TotallyDefined
import Analysis.Indeterministic
import Analysis.SolutionCompleteness
import Analysis.Types                ( analysisName )
import CASS.Server                   ( initializeAnalysisSystem, analyzeInterface )

import AbstractCurry.Types           ( CFixity, CRule)
import FlatCurry.Types

import Data.Maybe ( fromMaybe )

-- | Datatype for passing analysis results.
data AnaInfo =
   AnaInfo (QName -> Bool)          -- non-deterministic?
           (QName -> Completeness)  -- completely defined?
           (QName -> Bool)          -- indeterministically defined?
           (QName -> Bool)          -- solution complete?

-- | Reads and generates all analysis infos.
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

-- | Returns infos about the nondeterminism of a function.
getNondetInfo :: AnaInfo -> QName -> Bool
getNondetInfo (AnaInfo oi _ _ _) = oi

-- | Returns infos about the completeness of a function.
getCompleteInfo :: AnaInfo -> QName -> Completeness
getCompleteInfo (AnaInfo _ cdi _ _) = cdi

-- | Returns infos about the indeterminism of a function.
getIndetInfo :: AnaInfo -> QName -> Bool
getIndetInfo (AnaInfo _ _ idi _) = idi

-- | Returns infos about the solution complenteness of a function.
getOpCompleteInfo :: AnaInfo -> QName -> Bool
getOpCompleteInfo (AnaInfo _ _ _ oci) = oci

-- Translates a standard analysis result into functional form.
getFunctionInfo :: [(QName,a)] -> QName -> a
getFunctionInfo fnis n =
  fromMaybe (error ("No analysis result for function "++show n))
            (lookup n fnis)

--------------------------------------------------------------------------

-- | Datatype for storing different Analysis results.
data AnalysisInfo = AnalysisInfo      { nondet, indet, opComplete, ext :: Bool,
                                        complete :: Completeness,
                                        precedence :: Maybe (CFixity, Int),
                                        property :: [(Property, CRule)]
                                      }
                  | ShortAnalysisInfo { ext :: Bool,
                                        precedence :: Maybe (CFixity, Int),
                                        property :: [(Property, CRule)]
                                      }
                  | PrecedenceInfo    { precedence :: Maybe (CFixity, Int) }
                  | NoAnalysisInfo
  deriving (Show)

-- | Types of Properties of a Function.
data Property = PreSpec | PostSpec | Spec | Prop
  deriving (Show, Read)
