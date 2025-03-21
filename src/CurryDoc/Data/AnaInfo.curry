{- |
     Author  : Michael Hanus, Kai-Oliver Prott
     Version : March 2025

     Datatype and operations to handle analysis information in CurryDoc.
-}
module CurryDoc.Data.AnaInfo
  ( AnaInfo(..), readAnaInfo, getNondetInfo, getCompleteInfo, getIndetInfo,
    getOpCompleteInfo, getFunctionInfo, AnalysisInfo(..), Property(..),
    nondet, indet, opComplete, ext, complete, precedence, property )
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
  nondet'   <- analyzeAndCheck nondetAnalysis
  complete' <- analyzeAndCheck patCompAnalysis
  indet'    <- analyzeAndCheck indetAnalysis
  solcomp'  <- analyzeAndCheck solcompAnalysis
  return (AnaInfo (\qn -> nondet' qn == NDet) complete' indet' solcomp')
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
data AnalysisInfo
    = AnalysisInfo 
        Bool                   -- ^ `nondet`: Indicates whether the analysis is nondeterministic.
        Bool                   -- ^ `indet`: Indicates whether the analysis is indeterminate.
        Bool                   -- ^ `opComplete`: Specifies whether the operation is complete.
        Bool                   -- ^ `ext`: Denotes whether the analysis is external.
        Completeness           -- ^ `complete`: Specifies the completeness of the analysis.
        (Maybe (CFixity, Int)) -- ^ `precedence`: Optional precedence information.
        [(Property, CRule)]    -- ^ `property`: List of properties and associated rules.
    | ShortAnalysisInfo 
        Bool                   -- ^ `ext`: Denotes whether the analysis is external.
        (Maybe (CFixity, Int)) -- ^ `precedence`: Optional precedence information.
        [(Property, CRule)]    -- ^ `property`: List of properties and associated rules.
    | PrecedenceInfo 
        (Maybe (CFixity, Int)) -- ^ `precedence`: Optional precedence information.
    | NoAnalysisInfo
    deriving (Show)

nondet :: AnalysisInfo -> Bool
nondet x = case x of
    AnalysisInfo n _ _ _ _ _ _ -> n
    _ -> error "nondet: Not available in this constructor"

indet :: AnalysisInfo -> Bool
indet x = case x of
    AnalysisInfo _ i _ _ _ _ _ -> i
    _ -> error "indet: Not available in this constructor"

opComplete :: AnalysisInfo -> Bool
opComplete x = case x of
    AnalysisInfo _ _ o _ _ _ _ -> o
    _ -> error "opComplete: Not available in this constructor"

ext :: AnalysisInfo -> Bool
ext x = case x of
    AnalysisInfo _ _ _ e _ _ _ -> e
    ShortAnalysisInfo  e _ _   -> e
    _ -> error "ext: Not available in this constructor"

complete :: AnalysisInfo -> Completeness
complete x = case x of
    AnalysisInfo _ _ _ _ c _ _ -> c
    _ -> error "complete: Not available in this constructor"

precedence :: AnalysisInfo -> Maybe (CFixity, Int)
precedence x = case x of
    AnalysisInfo _ _ _ _ _ p _ -> p
    ShortAnalysisInfo    _ p _ -> p
    PrecedenceInfo         p   -> p
    _ -> error "precedence: Not available in this constructor"

property :: AnalysisInfo -> [(Property, CRule)]
property x = case x of
    AnalysisInfo _ _ _ _ _ _ p -> p
    ShortAnalysisInfo    _ _ p -> p
    _ -> error "property: Not available in this constructor"

-- | Types of Properties of a Function.
data Property = PreSpec | PostSpec | Spec | Prop
  deriving (Show, Read)
