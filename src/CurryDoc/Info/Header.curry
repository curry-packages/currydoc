{- |
     Author  : Kai-Oliver Prott
     Version : August 2018

     Operations to parse the module comments into a usable format.
-}
module CurryDoc.Info.Header
  (ModuleHeader(..), HeaderField(..), readModuleHeader, getCategoryWithDefault)
  where

import CurryDoc.Info.Goodies
import CurryDoc.Info.Comments

import Char (isSpace)
import List (isPrefixOf)

data ModuleHeader = ModuleHeader [(HeaderField, String)] String
  deriving (Show, Read)

data HeaderField = Description
                 | Category
                 | Author
                 | Version
  deriving (Show, Read)

-- | Parses a module header from comments
readModuleHeader :: [Comment] -> ModuleHeader
readModuleHeader cs =
  readHeaderField Description h1 ss1 (\h2 ss2 ->
  readHeaderField Category    h2 ss2 (\h3 ss3 ->
  readHeaderField Author      h3 ss3 (\h4 ss4 ->
  readHeaderField Version     h4 ss4 (\h5 ss5 ->
  readLongDescr               h5 ss5))))
  where ss1 = map commentString $ concatMap splitNestedComment cs
        h1 = ModuleHeader [] ""

-- | Generic reader function for given Field in CPS-Style. (Because why not? :)
readHeaderField :: HeaderField -> ModuleHeader -> [String]
                -> (ModuleHeader -> [String] -> ModuleHeader)
                -> ModuleHeader
readHeaderField _   m                      []     _    = m
readHeaderField typ m@(ModuleHeader fs cs) (s:ss) cont =
    if null text
      then readHeaderField typ m ss cont
      else if isModuleHeaderField (show typ) text
             then cont (ModuleHeader ((typ, field) : fs) cs) rest
             else cont m (s:ss)
  where (sp, text) = span isSpace s
        (field, rest) = getHeaderFieldValue (length sp) s ss

isModuleHeaderField :: String -> String -> Bool
isModuleHeaderField ty text =
  ty  `isPrefixOf` text &&
  ":" `isPrefixOf` (dropWhile isSpace (drop (length ty) text))

getHeaderFieldValue :: Int -> String -> [String] -> (String, [String])
getHeaderFieldValue intd s ss = (unlines (s' : ssV), rest)
  where
    s' = trimSpace $ safeTail $ dropWhile (/=':') s
    (ssV, rest) = splitWhileIndented intd ss
    safeTail xs = if null xs then xs else tail xs

splitWhileIndented :: Int -> [String] -> ([String], [String])
splitWhileIndented _    []     = ([], [])
splitWhileIndented intd (s:ss) =
  if length sp > intd
    then let (ssV', rest) = splitWhileIndented intd ss
         in  (text : ssV', rest)
    else ([], s:ss)
  where (sp, text) = span isSpace s

-- We have to take care of the indentation of a paragraph,
-- because if a line in the paragraph is indented even further,
-- then it might be markdown.
-- This is why trimming space would be wrong, as markdown would be broken.
-- Additionally, the comments should be concatenated with something like a
-- unlines, thus the newlines in between.
readLongDescr :: ModuleHeader -> [String] -> ModuleHeader
readLongDescr (ModuleHeader fs cs) rest =
  ModuleHeader fs (cs ++ readLongDescr' (getIndentation rest) rest)
  where getIndentation []        = 0
        getIndentation (s:ss)
           | not (all isSpace s) = length $ takeWhile isSpace s
           | otherwise           = getIndentation ss

        readLongDescr' _    []     = []
        readLongDescr' intd (s:ss)
           | all isSpace s = "\n" ++ readLongDescr' (getIndentation ss) ss
           | otherwise     = if spaceAmount > intd
                               then drop intd s ++ "\n"++
                                    readLongDescr' intd        ss
                               else text        ++ "\n" ++
                                    readLongDescr' spaceAmount ss
             where (space, text) = span isSpace s
                   spaceAmount   = length space

-- | Get the category of a module Header or return the default value
getCategoryWithDefault :: String -> [(HeaderField, String)] -> String
getCategoryWithDefault def []             = def
getCategoryWithDefault def ((cat,v):rest) = case cat of
  Category -> v
  _        -> getCategoryWithDefault def rest