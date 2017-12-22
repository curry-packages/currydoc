-- Auxuiliaries to handle the options of the CurryDoc tool.

module CurryDoc.Options
  ( DocType(..), DocOptions(..), defaultCurryDocOptions )
 where

--------------------------------------------------------------------------
--- The kind of documentations which can be generated.
data DocType = HtmlDoc | TexDoc | CDoc
 deriving Eq

--- The options for the documentation generator.
--- docType - the target format of the documentation
--- withIndex - True if the index pages should also be generated
--- withMarkdown - True if the comments should be processed as markdown code
--- mainTitle    - the main title of the docmentation
--- useDirURL    - "--use" options (i.e., source dir and corresponding doc URL)
--- docMods      - the list of already documented modules, i.e., where the
---                documentation does not need generated
--- docURL       - the mapping from module names into the URL of their docs
data DocOptions = DocOptions
  { docType      :: DocType
  , withIndex    :: Bool
  , withMarkdown :: Bool
  , mainTitle    :: String
  , useDirURL    :: [(String,String)]
  , docMods      :: [String]
  , docURL       :: String -> String
  }

--- Default options
defaultCurryDocOptions :: DocOptions
defaultCurryDocOptions = DocOptions HtmlDoc True True "" [] [] id
