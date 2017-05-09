-- Auxuiliaries to handle the options of the CurryDoc tool.

module CurryDoc.Options
  ( DocType(..), DocOptions(..)
  , defaultCurryDocOptions, cdocFileOptions )
 where

--------------------------------------------------------------------------
--- The kind of documentations which can be generated.
data DocType = HtmlDoc | TexDoc | CDoc
 deriving Eq

--- The options for the documentation generator.
--- doctype - the target format of the documentation
--- withindex - True if the index pages should also be generated
--- withmarkdown - True if the comments should be processed as markdown code
data DocOptions = DocOptions
  { docType      :: DocType
  , withIndex    :: Bool
  , withMarkdown :: Bool
  , mainTitle    :: String
  }
 deriving Eq

--- Default options
defaultCurryDocOptions :: DocOptions
defaultCurryDocOptions = DocOptions HtmlDoc True True ""

--- Options for writing CDoc files.
cdocFileOptions :: DocOptions
cdocFileOptions = DocOptions CDoc False False ""
