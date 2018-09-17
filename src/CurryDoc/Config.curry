{- |
     Author  : Michael Hanus, Jan Tikovsky
     Version : May 2018

     This module contains some configuration parameters for the CurryDoc tool.
-}
module CurryDoc.Config where

import Distribution           (curryCompiler)
import CurryDoc.PackageConfig (packageVersion)

-- | Version of currydoc
currydocVersion :: String
currydocVersion = "Version " ++ packageVersion ++ " of September, 2018"

-- | The URL of the base directory containing the styles, images, etc.
styleBaseURL :: String
styleBaseURL = "bt3"
  --if curryCompiler=="pakcs"
  --  then "https://www.informatik.uni-kiel.de/~pakcs/bt3"
  --  else "https://www-ps.informatik.uni-kiel.de/kics2/bt3"

-- | The URL of the base directory containing the styles, images, etc.
currySystemURL :: String
currySystemURL = if curryCompiler=="pakcs"
                 then "https://www.informatik.uni-kiel.de/~pakcs"
                 else "https://www-ps.informatik.uni-kiel.de/kics2"

-- | The name of this Curry system.
currySystem :: String
currySystem = if curryCompiler=="pakcs" then "PAKCS" else "KiCS2"

-- | The URL of the API search
currygleURL :: String
currygleURL = "https://www-ps.informatik.uni-kiel.de/kics2/currygle/"

-- | The URL of the Curry homepage
curryHomeURL :: String
curryHomeURL = "http://www.curry-language.org"
