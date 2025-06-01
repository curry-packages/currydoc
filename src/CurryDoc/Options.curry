{- |
    Description : Auxuiliaries to handle the options of the CurryDoc tool.
    Author      : Michael Hanus, Jan Tikovsky, Kai-Oliver Prott
    Version     : August 2018
-}
module CurryDoc.Options
  ( DocType(..), DocOptions(..), defaultCurryDocOptions )
 where

-- | The kind of documentations which can be generated.
data DocType = HtmlDoc | TexDoc | JSON | CDoc
  deriving Eq

-- | The options for the documentation generator.
data DocOptions = DocOptions
  { docType      :: DocType -- ^ the target format of the documentation
  , withIndex    :: Bool -- ^ True if index pages should also be generated
  , withMarkdown :: Bool -- ^ True if comments should be processed as markdown
  , withAnalysis :: Bool -- ^ True if extended analysis should be performed
  , recursive    :: Bool -- ^ True if documentation for imported modules
                         --   should be generated
  , mainTitle    :: String -- ^ the main title of the documentation
  , useDirURL    :: [(String,String)] -- ^ "--use" options
                                      --    (i.e., source dir and
                                      --    corresponding doc URL)
  , docMods      :: [String] -- ^  the list of already documented modules,
                             --    i.e., where the
                             --    documentation does not need generated
  , docURL       :: String -> String -- ^ the mapping from module names into
                                     --   the URL of their docs
  }

-- | Default options
defaultCurryDocOptions :: DocOptions
defaultCurryDocOptions = DocOptions HtmlDoc True True True True "" [] [] id
