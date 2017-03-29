----------------------------------------------------------------------
--- Implementation of CurryDoc, a utility for the automatic
--- generation of HTML documentation from Curry programs.
---
--- @author Michael Hanus, Jan Tikovsky
--- @version June 2015
----------------------------------------------------------------------

-- * All comments to be put into the HTML documentation must be
--   prefixed by "--- " (also in literate programs!).
--
-- * The comment of a module must occur before the first "module" or
--   "import" line of this module.
--
-- * The comment of a function or datatype must occur before the
--   first definition of this function or datatype.
--
-- * The comments can contain at the end several special comments:
--   @cons id comment   --> a comment for a constructor of a datatype
--   @param id comment  --> comment for function parameter id
--                          (list all parameters in left-to-right order)
--   @return comment    --> comments for the return value of a function
--   @author comment    --> the author of a module (only in module comments)
--   @version comment   --> the version of a module (only in module comments)
--
-- * Current restriction: doesn't properly work for infix operator definitions
--   without a type definition (so it should be always included)

module CurryDoc.Main where

import AbstractCurry.Files
import Directory
import Distribution
import FileGoodies
import FilePath ((</>), (<.>), dropFileName, takeFileName)
import FlatCurry.Types
import FlatCurry.Files
import FlatCurry.Read(readFlatCurryWithImports)
import Function
import List
import Maybe(fromJust)
import System
import Time

import Analysis.Deterministic
import Analysis.TotallyDefined
import Analysis.Indeterministic
import Analysis.SolutionCompleteness
import CASS.Server(initializeAnalysisSystem,analyzeInterface)

import CurryDoc.AnaInfo
import CurryDoc.Params
import CurryDoc.Read
import CurryDoc.Html
import CurryDoc.TeX
import CurryDoc.CDoc
import CurryDoc.Config
import CurryDoc.PackageConfig (packagePath)

--------------------------------------------------------------------------
-- Global definitions:

greeting :: String
greeting =
  "CurryDoc (" ++ currydocVersion ++ ") - the Curry Documentation Tool\n"

-- Directory where include files for generated documention (e.g., icons,
-- css, tex includes) are stored:
includeDir :: String
includeDir = packagePath </> "include"

--------------------------------------------------------------------------
-- Check arguments and call main function:
main :: IO ()
main = do
  args <- getArgs
  processArgs defaultCurryDocParams args

processArgs :: DocParams -> [String] -> IO ()
processArgs params args = case args of
  -- no markdown
  ("--nomarkdown":margs) -> processArgs (setMarkDown False  params) margs
  -- documentation type
  ("--html"      :margs) -> processArgs (setDocType HtmlDoc params) margs
  ("--tex"       :margs) -> processArgs (setDocType TexDoc  params) margs
  ("--cdoc"      :margs) -> processArgs (setDocType CDoc    params) margs
  -- HTML without index
  ["--noindexhtml",docdir,modname] ->
      makeCompleteDoc (setIndex False (setDocType HtmlDoc params))
                      True docdir (stripCurrySuffix modname)
  -- HTML index only
  ("--onlyindexhtml":docdir:modnames) ->
                      makeIndexPages docdir (map stripCurrySuffix modnames)
  ("--libsindexhtml":docdir:modnames) ->
                      makeSystemLibsIndex docdir modnames
  (('-':_):_) -> putStrLn usageMessage
  -- module
  [modname] ->
      makeCompleteDoc params (docType params == HtmlDoc)
                      ("DOC_" ++ stripCurrySuffix (takeFileName modname))
                      (stripCurrySuffix modname)
  -- docdir + module
  [docdir,modname] ->
      makeCompleteDoc params (docType params == HtmlDoc) docdir
                      (stripCurrySuffix modname)
  _ -> putStrLn usageMessage

usageMessage :: String
usageMessage = unlines
 [ "ERROR: Illegal arguments for CurryDoc"
 , "Usage: curry-doc [--nomarkdown] [--html|--tex|--cdoc] [<doc directory>] <module_name>"
 , "       curry-doc [--nomarkdown] --noindexhtml <doc directory> <module_name>"
 , "       curry-doc --onlyindexhtml <doc directory> <module_names>"
 , "       curry-doc --libsindexhtml <doc directory> <module_names>"
 ]



-- create directory if not existent:
createDir :: String -> IO ()
createDir dir = do
  exdir <- doesDirectoryExist dir
  unless exdir $ system ("mkdir " ++ dir) >> done

--------------------------------------------------------------------------
--- The main function of the CurryDoc utility.
--- @param withindex - True if the index pages should also be generated
--- @param recursive - True if the documentation for the imported modules
---                    should be also generated (if necessary)
--- @param docdir - the directory name containing all documentation files
--- @param modname - the name of the main module to be documented
makeCompleteDoc :: DocParams -> Bool -> String -> String -> IO ()
makeCompleteDoc docparams recursive reldocdir modpath = do
  putStrLn greeting
  docdir <- makeAbsolute reldocdir
  prepareDocDir (docType docparams) docdir
  lookupModuleSourceInLoadPath modpath >>=
   maybe (error $ "Source code of module '"++modpath++"' not found!")
    (\ (moddir,_) -> do
      let modname = takeFileName modpath
      setCurrentDirectory moddir
      -- parsing source program:
      callFrontend FCY modname
      -- generate abstract curry representation
      callFrontend ACY modname
      -- when constructing CDOC the imported modules don't have to be read
      -- from the FlatCurry file
      (alltypes,allfuns) <- getProg modname $ docType docparams
      makeDocIfNecessary docparams recursive docdir modname
      when (withIndex docparams) $ do
        genMainIndexPage     docdir [modname]
        genFunctionIndexPage docdir allfuns
        genConsIndexPage     docdir alltypes
      -- change access rights to readable for everybody:
      system ("chmod -R go+rX "++docdir)
      putStrLn ("Documentation files written into directory "++docdir) )
 where
  getProg modname HtmlDoc = readTypesFuncsWithImports modname
  getProg modname TexDoc  = readTypesFuncsWithImports modname
  getProg modname CDoc    = do (Prog _ _ types funs _) <- readFlatCurry modname
                               return (types,funs)

--- Transform a file path into an absolute file path:
makeAbsolute :: String -> IO String
makeAbsolute f =
  if isAbsolute f
  then return f
  else do curdir <- getCurrentDirectory
          return (curdir </> f)

--- Generate only the index pages for a list of (already compiled!) modules:
makeIndexPages :: String -> [String] -> IO ()
makeIndexPages docdir modnames = do
  putStrLn greeting
  prepareDocDir HtmlDoc docdir
  (alltypes,allfuns) <- mapIO readTypesFuncs modnames >>= return . unzip
  genMainIndexPage     docdir modnames
  genFunctionIndexPage docdir (concat allfuns)
  genConsIndexPage     docdir (concat alltypes)
  -- change access rights to readable for everybody:
  system ("chmod -R go+rX "++docdir)
  done
 where
  readTypesFuncs modname = do
    fcyfile <- getFlatCurryFileInLoadPath modname
    (Prog _ _ types funs _) <- readFlatCurryFile fcyfile
    return (types,funs)

--- Generate a system library index page categorizing the given
--- (already compiled!) modules
makeSystemLibsIndex :: String -> [String] -> IO ()
makeSystemLibsIndex docdir modnames = do
  -- generate index pages (main index, function index, constructor index)
  makeIndexPages docdir modnames
  putStrLn ("Categorizing modules ...")
  modInfos <- mapIO getModInfo modnames
  putStrLn ("Grouping modules by categories ...")
  let grpMods = map sortByName $ groupByCategory $ sortByCategory modInfos
      cats    = sortBy (<=) $ nub $ map fst3 modInfos
  genSystemLibsPage docdir cats grpMods
 where
  fst3 (x,_,_)    = x
  snd3 (_,y,_)    = y
  sortByCategory  = sortBy ((<=) `on` fst3)
  groupByCategory = groupBy ((==) `on` fst3)
  sortByName      = sortBy ((<=) `on` snd3)

getModInfo :: String -> IO (Category,String,String)
getModInfo modname = do
  mmodsrc <- lookupModuleSourceInLoadPath modname
  case mmodsrc of
    Nothing           -> error $ "Source code of module '"++modname++"' not found!"
    Just (_,progname) -> do
      (modcmts,_) <- readComments progname
      let (modcmt,catcmts) = splitComment modcmts
          category         = readCategory $ getCommentType "category" catcmts
      return (category,modname,firstPassage modcmt)

-- create documentation directory (if necessary) with gifs and stylesheets:
prepareDocDir :: DocType -> String -> IO ()
prepareDocDir HtmlDoc docdir = do
  createDir docdir
  -- copy style sheet:
  copyIncludeIfPresent docdir "currydoc.css"
prepareDocDir TexDoc docdir = do
  createDir docdir
  putStrLn $ "Copy macros into documentation directory \""++docdir++"\"..."
  copyIncludeIfPresent docdir "currydoc.tex"
prepareDocDir CDoc docdir = do
  createDir docdir
  putStrLn ("Directory was succesfully created")

copyIncludeIfPresent :: String -> String -> IO ()
copyIncludeIfPresent docdir inclfile = do
  existIDir <- doesDirectoryExist includeDir
  when existIDir $
    system ("cp "++includeDir++"/"++inclfile++" "++docdir) >> done

-- read and generate all analysis infos:
readAnaInfo :: String -> IO AnaInfo
readAnaInfo modname = do
  initializeAnalysisSystem
  nondet   <- analyzeInterface nondetAnalysis  modname >>= stopIfError
  complete <- analyzeInterface patCompAnalysis modname >>= stopIfError
  indet    <- analyzeInterface indetAnalysis   modname >>= stopIfError
  solcomp  <- analyzeInterface solcompAnalysis modname >>= stopIfError
  return (AnaInfo (\qn -> nondet qn == NDet) complete indet solcomp)
 where
   stopIfError (Right err) = error ("Analysis error: "++err)
   stopIfError (Left results) =
     return (\qn -> maybe (error $ "No analysis result for function "++show qn)
                          id
                          (lookup qn results))

-- generate documentation for a single module:
makeDoc :: DocParams -> Bool -> String -> String -> IO ()
makeDoc docparams recursive docdir modname = do
  Just (_,progname) <- lookupModuleSourceInLoadPath modname
  putStrLn ("Reading comments from file '"++progname++"'...")
  (modcmts,progcmts) <- readComments progname
  putStrLn ("Reading analysis information for module \""++modname++"\"...")
  anainfo <- readAnaInfo modname
  makeDocWithComments (docType docparams) docparams recursive docdir
                      anainfo modname modcmts progcmts


makeDocWithComments :: DocType -> DocParams -> Bool -> String -> AnaInfo
                    -> String -> String -> [(SourceLine,String)] -> IO ()
makeDocWithComments HtmlDoc docparams recursive docdir anainfo modname
                    modcmts progcmts = do
  -- ensure that the AbstractCurry file for the module exists
  loadpath <- getLoadPathForModule modname
  modpath <- lookupFileInPath (abstractCurryFileName modname) [""] loadpath
  unless (modpath /= Nothing) $ callFrontend ACY modname
  writeOutfile docparams recursive docdir modname
               (generateHtmlDocs docparams anainfo modname modcmts progcmts)
  translateSource2ColoredHtml docdir modname
  writeOutfile (DocParams CDoc False False) False docdir modname
               (generateCDoc modname modcmts progcmts anainfo)


makeDocWithComments TexDoc docparams recursive docdir anainfo modname
                    modcmts progcmts = do
  writeOutfile docparams recursive docdir modname
               (generateTexDocs docparams anainfo modname modcmts progcmts)


makeDocWithComments CDoc docparams recursive docdir anainfo modname
                    modcmts progcmts = do
  writeOutfile docparams recursive docdir modname
               (generateCDoc modname modcmts progcmts anainfo)


--- Generates the documentation for a module if it is necessary.
--- I.e., the documentation is generated if no previous documentation
--- file exists or if the existing documentation file is older than
--- the FlatCurry file.
makeDocIfNecessary :: DocParams -> Bool -> String -> String -> IO ()
makeDocIfNecessary docparams recursive docdir modname = do
  let docfile = docdir </> modname ++
                (if docType docparams == HtmlDoc then ".html" else ".tex")
  docexists <- doesFileExist docfile
  if not docexists
   then copyOrMakeDoc docparams recursive docdir modname
   else do
     ctime  <- getFlatCurryFileInLoadPath modname >>= getModificationTime
     dftime <- getModificationTime docfile
     if compareClockTime ctime dftime == GT
      then copyOrMakeDoc docparams recursive docdir modname
      else when recursive $ do
             imports <- getImports modname
             mapIO_ (makeDocIfNecessary docparams recursive docdir) imports

-- get imports of a module by reading the interface, if possible:
getImports :: String -> IO [String]
getImports modname = do
  mbfintfile <- getLoadPathForModule modname >>=
                lookupFileInPath (flatCurryIntName modname) [""]
  (Prog _ imports _ _ _) <- maybe
                             (getFlatCurryFileInLoadPath modname >>=
                              readFlatCurryFile)
                             readFlatCurryFile
                             mbfintfile
  return imports

copyOrMakeDoc :: DocParams -> Bool -> String -> String -> IO ()
copyOrMakeDoc docparams recursive docdir modname = do
  hasCopied <- copyDocIfPossible docparams docdir modname
  unless hasCopied $ makeDoc docparams recursive docdir modname

--- Copy the documentation file from standard documentation directoy "CDOC"
--- (used for documentation of system libraries) if possible.
--- Returns true if the copy was possible.
copyDocIfPossible :: DocParams -> String -> String -> IO Bool
copyDocIfPossible docparams docdir modname =
  if docType docparams == TexDoc
  then return False -- ignore copying for TeX docs
  else do
    mdir <- lookupModuleSourceInLoadPath modname >>= return . fst . fromJust
    let docprogname = mdir </> "CDOC" </> modname
        docHtmlFile = docprogname <.> "html"
    docexists <- doesFileExist docHtmlFile
    if not docexists
      then return False
      else do
        ctime <- getModificationTime (mdir </> flatCurryFileName modname)
        htime <- getModificationTime docHtmlFile
        if compareClockTime ctime htime == GT
          then return False
          else do
            putStrLn ("Copying doc file from " ++ docHtmlFile)
            system ("cp " ++ docHtmlFile ++ ' ':docdir)
            system ("cp " ++ docprogname ++ "_curry.html "++docdir)
            return True

-----------------------------------------------------------------------
-- auxiliaries:

-- reads all types and function declarations (also imported ones) of
-- a module:
readTypesFuncsWithImports :: String -> IO ([TypeDecl],[FuncDecl])
readTypesFuncsWithImports modname = do
  allprogs <- readFlatCurryWithImports modname
  let (ts,fs) = unzip (map (\ (Prog _ _ types funs _) -> (types,funs)) allprogs)
  return (concat ts, concat fs)

-- get the associated file extenstion from DocType
fileExtension :: DocType -> String
fileExtension HtmlDoc = "html"
fileExtension TexDoc  = "tex"
fileExtension CDoc    = "cdoc"

-- harmonized writeFile function for all docType
writeOutfile :: DocParams -> Bool -> String -> String -> IO String -> IO ()
writeOutfile docparams recursive docdir modname generate = do
  doc     <- generate
  imports <- getImports modname
  let outfile = docdir </> modname <.> fileExtension (docType docparams)
  putStrLn ("Writing documentation to \"" ++ outfile ++ "\"...")
  writeFile outfile doc
  when recursive $
    mapIO_ (makeDocIfNecessary docparams recursive docdir) imports

-- -----------------------------------------------------------------------
