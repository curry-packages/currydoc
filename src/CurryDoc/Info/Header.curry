{- |
     Author  : Kai-Oliver Prott
     Version : March 2025

     Operations to parse the module comments into a usable format.
-}
module CurryDoc.Info.Header
  ( ModuleHeader(..), HeaderField(..), readModuleHeader, getCategoryWithDefault )
  where

import CurryDoc.Info.Goodies
import CurryDoc.Info.Comments

import Data.Char ( isSpace )
import Data.List ( isPrefixOf, sort )

-- | The header of a Curry module.
data ModuleHeader = ModuleHeader [(HeaderField, String)] String
  deriving (Show, Read)

-- | The types of header fields.
data HeaderField = Description
                 | Category
                 | Author
                 | Version
  deriving (Eq, Show, Read, Enum, Ord)

-- | Reads the module header from a list of comments.
readModuleHeader :: [Comment] -> ModuleHeader
readModuleHeader = orderFields . readFullDesc . removeBorder . toStrings 

-- | Reads the full description of a module header.
--   
--   This implementation allows for an arbitrary structure of header fields
--   and descriptions, with no requirements regarding the order.
-- 
--   We have to take care of the indentation of a paragraph,
--   because if a line in the paragraph is indented even further,
--   then it might be markdown. This is why trimming space would be wrong, 
--   as markdown would be broken. Additionally, the comments should be concatenated with
--   something like a unlines, thus the newlines in between.
readFullDesc :: [String] -> ModuleHeader
readFullDesc strs = readFullDesc' (ModuleHeader [] "") (getIndentation strs) strs
 where
  readFullDesc' h _    []    = h
  readFullDesc' h intd (s:ss)
     -- Skip empty lines: 
     | all isSpace s      = readFullDesc' (addComment h "\n") (getIndentation ss) ss
     -- Read indented paragraph:
     | spaceAmount > intd = readFullDesc' (addComment h $ drop intd s ++ "\n") intd ss
     -- Read either field or comment:
     | otherwise          = case tryParseHeaderFieldValue intd s ss of
                              Just ((f, v), rest) -> readFullDesc' (addField h f v) intd rest
                              Nothing             -> readFullDesc' (addComment h $ text ++ "\n") spaceAmount ss
   where 
    (space, text) = span isSpace s
    spaceAmount   = length space

-----------------------------------------------------------
-- Header field parsing and handling
--
-- TODO: prettifify this. Instead of an `is..`- and `get...`-operation, 
--       we could incorporate that into, e.g., a `tryParse...`-operation 
--       that returns a Maybe. 

-- | Converts a string to a header field.
stringToHeaderField :: String -> Maybe HeaderField
stringToHeaderField str = case toLowerString str of
  "description" -> Just Description
  "category"    -> Just Category
  "author"      -> Just Author
  "version"     -> Just Version
  _             -> Nothing

-- | Checks if a given string is a module header field
--   of shape "Field: Value" or "@Field Value" for an 
--   arbitrary field.
isModuleHeaderField :: String -> Bool
isModuleHeaderField text = 
  ':' `elem` text
  ||
  "@" `isPrefixOf` text

-- | Reads a field-value pair from a string with a given indentation.
--   The indentation is used to determine the end of the field-value pair.
--
--   Returns the field-value pair and the rest of the comments.
getHeaderFieldValue :: Int -> String -> [String] -> ((String, String), [String])
getHeaderFieldValue intd s ss 
  | "@" `isPrefixOf` s 
    = let s' = dropTokens 1 s
      in  ((tail $ head $ words s, 
           unlines (s' : ssV)), rest) -- old-style
  | otherwise          
    = let s' = trimSpace $ safeTail $ dropWhile (/=':') s   -- TODO: use break instead 
      in ((trimSpace $ toLowerString $ takeWhile (/=':') s, -- of dropWhile&takeWhile
          unlines (s' : ssV)),  rest) -- new-style
 where
  (ssV, rest) = splitWhileIndented intd ss
  safeTail xs = if null xs then xs else tail xs

tryParseHeaderFieldValue :: Int -> String -> [String] -> Maybe ((HeaderField, String), [String])
tryParseHeaderFieldValue intd s ss = 
  if isModuleHeaderField s
    then let ((fstr, v), rest) = getHeaderFieldValue intd s ss
         in  case stringToHeaderField fstr of
               Nothing -> Nothing
               Just f  -> Just ((f, v), rest)
    else Nothing

-----------------------------------------------------------
-- Helper functions for indentation and comments

-- | Splits a list of strings into two lists, where the first list
--   contains the longest prefix of the input list, where each element
--   is indented by at least the given amount of spaces.
--   The second list contains the rest of the input list.
splitWhileIndented :: Int -> [String] -> ([String], [String])
splitWhileIndented _    []     = ([], [])
splitWhileIndented intd (s:ss) =
  if length sp > intd
    then let (ssV', rest) = splitWhileIndented intd ss
         in  (text : ssV', rest)
    else ([], s:ss)
  where (sp, text) = span isSpace s

-- | Returns the indentation of a list of strings.
-- 
--   The indentation is the number of leading spaces of the first 
--   non-empty line, or 0, if no such line exists.
getIndentation :: [String] -> Int
getIndentation []        = 0
getIndentation (s:ss)
   | not (all isSpace s) = length $ takeWhile isSpace s
   | otherwise           = getIndentation ss

-- | Converts a list of comments to a list of strings.
toStrings :: [Comment] -> [String]
toStrings = map commentString . concatMap splitNestedComment

-- | Removes the border of a list of strings.
--   The border is a sequence of lines that contain only '-' characters.
removeBorder :: [String] -> [String]
removeBorder = reverse . removeEmpty . reverse . removeEmpty

-- | Removes the first string iff it contains only '-' characters.
removeEmpty :: [String] -> [String]
removeEmpty []     = []
removeEmpty (s:ss) = if all (== '-') s then ss else s:ss

-----------------------------------------------------------
-- Auxiliary functions for header handling

-- | Adds a comment to the header.
addComment :: ModuleHeader -> String -> ModuleHeader
addComment (ModuleHeader fs cs) c = ModuleHeader fs (cs ++ c)

-- | Adds a field to the header. If the field already exists,
--   it is overwritten.
addField :: ModuleHeader -> HeaderField -> String -> ModuleHeader
addField (ModuleHeader fs cs) f v 
  = ModuleHeader ((f, v) : filter ((/=f) . fst) fs) cs

-- | Returns the category of the module, if existing.
--   Otherwise, the default value is returned.
getCategoryWithDefault :: String -> [(HeaderField, String)] -> String
getCategoryWithDefault def []             = def
getCategoryWithDefault def ((cat,v):rest) = case cat of
  Category -> v
  _        -> getCategoryWithDefault def rest

-- | Orders the fields of a module header.
orderFields :: ModuleHeader -> ModuleHeader
orderFields (ModuleHeader fs cs) = ModuleHeader (sort fs) cs