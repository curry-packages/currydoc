module CurryDoc.Info.Header
  (ModuleHeader(..), HeaderField(..), readModuleHeader) where

import CurryDoc.Info.Goodies
import CurryDoc.Info.Comments

import Char (isSpace)
import List (isPrefixOf)

data ModuleHeader = ModuleHeader [(HeaderField, String)] String
  deriving Show

data HeaderField = Description
                 | Category
                 | Author
                 | Version
  deriving Show

-- Parses a module header from comments
readModuleHeader :: [Comment] -> ModuleHeader
readModuleHeader cs =
  readHeaderField Description h1 ss1 (\h2 ss2 ->
  readHeaderField Category    h2 ss2 (\h3 ss3 ->
  readHeaderField Author      h3 ss3 (\h4 ss4 ->
  readHeaderField Version     h4 ss4 (\h5 ss5 ->
  readLongDescr               h5 ss5))))
  where ss1 = map commentString $ concatMap splitNestedComment cs
        h1 = ModuleHeader [] ""

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
getHeaderFieldValue intd s ss = (s' ++ unlines ssV, rest)
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

readLongDescr :: ModuleHeader -> [String] -> ModuleHeader
readLongDescr (ModuleHeader fs cs) ss = ModuleHeader fs (cs ++
  concatCommentStrings ss)
