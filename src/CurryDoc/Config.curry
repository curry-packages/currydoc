----------------------------------------------------------------------
--- This module contains some configuration parameters for
--- the CurryDoc tool.
---
--- @author Michael Hanus, Jan Tikovsky
--- @version September 2024
----------------------------------------------------------------------

module CurryDoc.Config where

import Curry.Compiler.Distribution ( curryCompiler )
import CurryDoc.PackageConfig      ( packageVersion )
import Language.Curry.Resources    ( curryWikiURL, kics2URL, pakcsURL )

--- Version of currydoc
currydocVersion :: String
currydocVersion = "Version " ++ packageVersion ++ " of September 25, 2024"

--- The URL of the base directory containing the styles, images, etc.
styleBaseURL :: String
styleBaseURL = "bt4"

--- The URL of the base directory containing the styles, images, etc.
currySystemURL :: String
currySystemURL = if curryCompiler=="pakcs" then pakcsURL
                                           else kics2URL

--- The name of this Curry system.
currySystem :: String
currySystem | curryCompiler == "pakcs" = "PAKCS"
            | curryCompiler == "kics2" = "KiCS2"
            | otherwise                = "???"

--- The URL of the Curry Wiki
curryDocURL :: String
curryDocURL = curryWikiURL ++ "/tools/currydoc"
