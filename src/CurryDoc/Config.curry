{- |
     Author  : Michael Hanus, Jan Tikovsky
     Version : May 2025

     This module contains some configuration parameters for the CurryDoc tool.
-}
module CurryDoc.Config where

import Curry.Compiler.Distribution ( curryCompiler )
import CurryDoc.PackageConfig      ( packageVersion )
import Language.Curry.Resources    ( curryWikiURL, pakcsURL, kics2URL )

-- | Version of currydoc
currydocVersion :: String
currydocVersion = "Version " ++ packageVersion ++ " of May, 2025"

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
currySystem = if curryCompiler=="pakcs" then "PAKCS" else "KiCS2"

-- | The URL of CurryDoc
curryDocURL :: String
curryDocURL = curryWikiURL ++ "/tools/currydoc"
