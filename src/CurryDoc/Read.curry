----------------------------------------------------------------------
--- Some auxiliary operations of CurryDoc to read programs.
---
--- @author Michael Hanus, Jan Tikovsky
--- @version November 2017
----------------------------------------------------------------------

module CurryDoc.Read where

import Data.Char
import Data.List ( intercalate, isSuffixOf )
import System.IO

import FlatCurry.Types

--------------------------------------------------------------------------
--- Reads the module comment of a Curry source file.
readModuleComment :: String -> IO String
readModuleComment filename = openFile filename ReadMode >>= readHeader []
 where
  readHeader cs h = do
    eof <- hIsEOF h
    if eof then hClose h >> return ""
           else do l <- hGetLine h
                   case classifyLine l of
                     ModDef    -> hClose h >> return (unlines (reverse cs))
                     FuncDef _ -> hClose h >> return "" -- no module header
                     DataDef _ -> hClose h >> return "" -- no module header
                     OtherLine -> readHeader cs h
                     Comment c -> readHeader (c:cs) h

--- Reads the comments of a source file to be put in the HTML documentation.
--- Returns the module comment and the list of (Func/DataDef,comment) pairs.
readComments :: String -> IO (String,[(SourceLine,String)])
readComments filename = do
  prog <- readFile filename
  return (groupLines . filter (/=OtherLine) . map classifyLine . lines $ prog)

--------------------------------------------------------------------------
--- This datatype is used to classify all input lines.
--- @cons Comment   - a comment for CurryDoc
--- @cons FuncDef   - a definition of a function
--- @cons DataDef   - a definition of a datatype
--- @cons ModDef    - a line containing a module definition
--- @cons OtherLine - a line not relevant for CurryDoc
data SourceLine = Comment String  -- a comment for CurryDoc
                | FuncDef String  -- a definition of a function
                | DataDef String  -- a definition of a datatype
                | ModDef          -- a line containing a module definition
                | OtherLine       -- a line not relevant for CurryDoc
 deriving Eq

--- This datatype is used to categorize Curry libraries
--- @cons General   - a general library
--- @cons Algorithm - a library which provides data structures and algorithms
--- @cons Database  - a library for database access
--- @cons Web       - a library for web applications
--- @cons Meta      - a library for meta-programming
data Category = General
              | Algorithm
              | Database
              | Web
              | Meta
 deriving (Eq,Ord)

type ModInfo = (Category, String, String)

--- Determine the category for a module
readCategory :: [String] -> Category
readCategory [] = General
readCategory (catcmt:_) = case cat of
  "general"   -> General
  "algorithm" -> Algorithm
  "database"  -> Database
  "web"       -> Web
  "meta"      -> Meta
  _           -> General
 where
  (cat,_) = span isIdChar catcmt

--- Show a category
showCategory :: Category -> String
showCategory General   = "General libraries"
showCategory Algorithm = "Data structures and algorithms"
showCategory Database  = "Database access and manipulation"
showCategory Web       = "Libraries for web applications"
showCategory Meta      = "Libraries for meta-programming"

--- ID for a category
getCategoryID :: Category -> String
getCategoryID General   = "general"
getCategoryID Algorithm = "algorithm"
getCategoryID Database  = "database"
getCategoryID Web       = "web"
getCategoryID Meta      = "meta"

-- classify a line of the source program:
-- here we replace blank line comments by a "breakline" tag
classifyLine :: String -> SourceLine
classifyLine line
 | take 3 line == "---"  && all isSpace (drop 3 line) = Comment "" --"<br/>"
 | take 4 line == "--- " && head (drop 4 line) /= '-' = Comment (drop 4 line)
 | take 7 line == "module "  = ModDef
 | take 7 line == "import "  = ModDef
 | otherwise = let id1 = getFirstId line
               in if null id1
                    then OtherLine
                    else if id1 == "data" || id1 == "type" || id1 == "newtype"
                           then DataDef (getDatatypeName line)
                           else if "'default" `isSuffixOf` id1
                                  then OtherLine -- ignore default rules
                                  else FuncDef id1
 where
   getDatatypeName = takeWhile isIdChar . dropWhile (==' ') . dropWhile isIdChar

-- get the first identifier (name or operator in brackets) in a string:
getFirstId :: String -> String
getFirstId [] = ""
getFirstId (c:cs)
  | isAlpha c = takeWhile isIdChar (c:cs)
  | c == '('  = let bracketid = takeWhile (/=')') cs
                 in if all (`elem` infixIDs) bracketid
                    then bracketid
                    else ""
  | otherwise = ""

-- is an alphanumeric character, underscore, or apostroph?
isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_' || c == '\''

-- All characters occurring in infix operators.
infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

-- group the classified lines into module comment and list of
-- (Func/DataDef,comment) pairs:
groupLines :: [SourceLine] -> (String,[(SourceLine,String)])
groupLines sls =
  let (modcmts,progcmts) = break (==ModDef) sls
  in if null progcmts
       then ("", groupProgLines sls)
       else (intercalate "\n" (map getComment modcmts),
             groupProgLines (filter (/=ModDef) (tail progcmts)))
 where
  getComment src = case src of
    Comment cmt -> cmt
    _           -> "" -- this case should usually not occur

groupProgLines :: [SourceLine] -> [(SourceLine,String)]
groupProgLines []                  = []
groupProgLines (Comment cmt : sls) = groupComment cmt sls
groupProgLines (FuncDef f   : sls) = (FuncDef f, "") : skipFuncDefs f sls
groupProgLines (DataDef d   : sls) = (DataDef d, "") : skipDataDefs d sls
groupProgLines (ModDef      : sls) = groupProgLines sls
groupProgLines (OtherLine   : sls) = groupProgLines sls

groupComment :: String -> [SourceLine] -> [(SourceLine,String)]
groupComment _ [] = []  -- comment not followed by definition -> ignore
groupComment cmt (Comment cmt1 : sls) = groupComment (cmt ++ "\n" ++ cmt1) sls
groupComment cmt (FuncDef f    : sls) = (FuncDef f, cmt) : skipFuncDefs f sls
groupComment cmt (DataDef d    : sls) = (DataDef d, cmt) : skipDataDefs d sls
groupComment cmt (ModDef       : sls) = groupComment cmt sls
groupComment cmt (OtherLine    : sls) = groupComment cmt sls

skipFuncDefs :: String -> [SourceLine] -> [(SourceLine,String)]
skipFuncDefs _ [] = []
skipFuncDefs _ (Comment cmt : sls) = groupProgLines (Comment cmt : sls)
skipFuncDefs _ (DataDef d   : sls) = groupProgLines (DataDef d   : sls)
skipFuncDefs f (FuncDef f1  : sls) =
  if f == f1 then skipFuncDefs f sls
             else groupProgLines (FuncDef f1 : sls)
skipFuncDefs f (ModDef      : sls) = skipFuncDefs f sls
skipFuncDefs f (OtherLine   : sls) = skipFuncDefs f sls

skipDataDefs :: String -> [SourceLine] -> [(SourceLine,String)]
skipDataDefs _ [] = []
skipDataDefs _ (Comment cmt : sls) = groupProgLines (Comment cmt : sls)
skipDataDefs _ (FuncDef f   : sls) = groupProgLines (FuncDef f   : sls)
skipDataDefs d (DataDef d1  : sls) =
  if d == d1 then skipDataDefs d sls
             else groupProgLines (DataDef d1 : sls)
skipDataDefs d (ModDef      : sls) = skipDataDefs d sls
skipDataDefs d (OtherLine   : sls) = skipDataDefs d sls

--------------------------------------------------------------------------
-- get comment for a function name:
getFuncComment :: String -> [(SourceLine,String)] -> String
getFuncComment _ [] = ""
getFuncComment fname ((def, cmt):fdcmts) = case def of
  FuncDef f -> if fname == f then cmt else getFuncComment fname fdcmts
  _         -> getFuncComment fname fdcmts

-- get comment for a constructor or a field name
getConsComment :: [String] -> String -> Maybe ((String,String))
getConsComment [] _ = Nothing
getConsComment (conscmt:conscmts) cname =
  let (consname,rconscmt) = span isIdChar conscmt
   in if consname == cname
      then let (conscall,callcmt) = break (=='-') conscmt
            in Just (if null callcmt then (consname,rconscmt)
                                     else (conscall,callcmt))
      else getConsComment conscmts cname

-- get comment for a type name:
getDataComment :: String -> [(SourceLine,String)] -> String
getDataComment _ [] = ""
getDataComment n ((def, cmt):fdcmts) = case def of
  DataDef d -> if n == d then cmt else getDataComment n fdcmts
  _         -> getDataComment n fdcmts


-- get all comments of a particular type (e.g., "param", "cons"):
getCommentType :: Eq a => a -> [(a,b)] -> [b]
getCommentType ctype cmts = map snd (filter (\c -> fst c == ctype) cmts)



--------------------------------------------------------------------------
-- Split a comment into its main part and parts preceded by "@...":
-- Example: splitComment "aaaa\nbbbb\n@param xxxx\n@return yyyy"
--          = ("aaaa\nbbbb",[("param","xxxx"),("return","yyyy")])

splitComment :: String -> (String,[(String,String)])
splitComment cmt = splitCommentMain (lines cmt)

splitCommentMain :: [String] -> (String,[(String,String)])
splitCommentMain []     = ("",[])
splitCommentMain (l:ls) =
  if null l || head l /= '@'
    then let (maincmt,rest) = splitCommentMain ls
         in if null maincmt then (l, rest)
                            else (l ++ '\n':maincmt, rest)
    else ([], splitCommentParams (takeWhile isAlpha (tail l))
                                 (dropWhile isAlpha (tail l)) ls)

splitCommentParams :: String -> String -> [String] -> [(String,String)]
splitCommentParams param paramcmt [] = [(param,skipWhiteSpace paramcmt)]
splitCommentParams param paramcmt (l:ls) =
  if null l || head l /= '@'
    then splitCommentParams param (paramcmt++('\n':l)) ls
    else ((param,skipWhiteSpace paramcmt)
          : splitCommentParams (takeWhile isAlpha (tail l))
                               (dropWhile isAlpha (tail l)) ls)

-----------------------------------------------------------------------
-- auxiliaries:

isFunctionType :: TypeExpr -> Bool
isFunctionType te = case te of FuncType _ _ -> True
                               _            -> False

-- skip leading blanks or CRs in a string:
skipWhiteSpace :: String -> String
skipWhiteSpace = dropWhile isWhiteSpace

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c == ' ' || c == '\n'

-- enclose a non-letter identifier in brackets:
showId :: String -> String
showId name = if isAlpha (head name) then name
                                     else ('(':name)++")"

-- if first argument is True, put brackets around second argument:
brackets :: Bool -> String -> String
brackets False s = s
brackets True  s = "("++s++")"

-- extract last name from a path name:
getLastName :: String -> String
getLastName = reverse . takeWhile (/='/') . reverse

--------------------------------------------------------------------------
