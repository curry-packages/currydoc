{- |
     Author  : Michael Hanus, Jan Tikovsky
     Version : September 2025

     This module contains some configuration parameters for the CurryDoc tool.
-}
module CurryDoc.Config where

import Curry.Compiler.Distribution ( curryCompiler )
import CurryDoc.PackageConfig      ( packageVersion )
import Language.Curry.Resources    ( pakcsURL, kics2URL )

-- | Version of currydoc
currydocVersion :: String
currydocVersion = "Version " ++ packageVersion ++ " of September 21, 2025"

-- | The URL of the base directory containing the styles, images, etc.
styleBaseURL :: String
styleBaseURL = "bt4"

-- | The URL of the base directory containing the styles, images, etc.
currySystemURL :: String
currySystemURL = if curryCompiler=="pakcs"
                  then pakcsURL
                  else kics2URL

-- | The name of this Curry system.
currySystem :: String
currySystem | curryCompiler == "pakcs" = "PAKCS"
            | curryCompiler == "kics2" = "KiCS2"
            | otherwise                = "???"

-- | The URL of CurryDoc
curryDocURL :: String
curryDocURL = "https://cpm.curry-lang.org/pkgs/currydoc.html"
