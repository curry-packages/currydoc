{- |
     Author  : Kai-Oliver Prott
     Version : May 2025

     Operations to parse the module comments into a usable format.
-}
module CurryDoc.Info.Header
  ( ModuleHeader(..), HeaderField(..), readModuleHeader
  , getCategoryWithDefault, getFieldWithDefault )
  where

import CurryDoc.Info.Goodies
import CurryDoc.Info.Comments

import Data.Char  ( isSpace )
import Data.Maybe ( fromMaybe )
import Data.List  ( isPrefixOf, sort, intercalate)

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
     -- Read either a field or a comment:
     | otherwise = 
        case readCommentLine intd s ss of
          (HeaderFieldLine f v, rest) 
            -> readFullDesc' (addField h f v) intd rest
          (Line t, rest)
            -> readFullDesc' (addComment h $ t ++ "\n") spaceAmount rest
   where 
    spaceAmount = countIndent s

-----------------------------------------------------------
-- Header field parsing and handling

data HeaderComment = Line String
                   | HeaderFieldLine HeaderField String
  deriving (Show, Read) 

-- | Converts a string to a header field.
stringToHeaderField :: String -> Maybe HeaderField
stringToHeaderField str = case toLowerString str of
  "description" -> Just Description
  "category"    -> Just Category
  "author"      -> Just Author
  "version"     -> Just Version
  _             -> Nothing

-- | Reads a header comment line. Parses the line as either a field-value
--   pair or a normal comment. The indentation is used to determine the
--   end of the field-value pair.
--
--   The function returns the parsed header comment and the remaining lines.
readCommentLine :: Int -> String -> [String] -> (HeaderComment, [String])
readCommentLine i s ss 
  | "@" `isPrefixOf` s = 
      let f  = tail $ head $ words s
          s' = dropTokens 1 s
          v  = unlines (s' : ssV)
      in makeField f v
  | otherwise =
      let (field, v) = break (== ':') s
      in case v of
        []     -> -- no field, treat as comment
          (Line $ dropSpaces s, ss) 
        (_:v') -> -- drop the colon and trim spaces
          makeField (trimSpace field) (unlines (trimSpace v' : ssV))
  where
    (ssV, rest) = splitWhileIndented i ss

    -- Tries to create a field-value pair from the given string.
    -- If the field is not recognized, the line is treated as a comment.
    makeField :: String -> String -> (HeaderComment, [String])
    makeField f v = case stringToHeaderField f of
      Just field -> (HeaderFieldLine field v, rest)
      Nothing    -> (Line $ dropSpaces s,       ss)

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
getCategoryWithDefault def = getFieldWithDefault' def Category

-- | Gets the value of a specific header field, or returns a default value.
getFieldWithDefault' :: String -> HeaderField -> [(HeaderField, String)] -> String
getFieldWithDefault' def f = fromMaybe def . getField f

-- | Gets the value of a specific header field.
getField :: HeaderField -> [(HeaderField, String)] -> Maybe String
getField = lookup

-- | Gets the value of a specific header field, or returns a default value.
getFieldWithDefault :: String -> HeaderField -> ModuleHeader -> String
getFieldWithDefault def f (ModuleHeader fs _) 
  = getFieldWithDefault' def f fs

-- | Orders the fields of a module header.
orderFields :: ModuleHeader -> ModuleHeader
orderFields (ModuleHeader fs cs) = ModuleHeader (sort fs) cs