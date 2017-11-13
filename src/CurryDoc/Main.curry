----------------------------------------------------------------------
--- Implementation of CurryDoc, a utility for the automatic
--- generation of HTML documentation from Curry programs.
---
--- @author Michael Hanus, Jan Tikovsky
--- @version November 2017
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
import Char            (toUpper)
import Directory
import Distribution
import FileGoodies
import FilePath        ((</>), (<.>), dropFileName, takeFileName)
import FlatCurry.Types
import FlatCurry.Files
import FlatCurry.Read  (readFlatCurryWithImports)
import Function
import List
import Maybe           (fromJust)
import System
import Time

import Analysis.Deterministic
import Analysis.TotallyDefined
import Analysis.Indeterministic
import Analysis.SolutionCompleteness
import Analysis.Types (analysisName)
import CASS.Server    (initializeAnalysisSystem, analyzeInterface)

import CurryDoc.AnaInfo
import CurryDoc.Options
import CurryDoc.Read
import CurryDoc.Html
import CurryDoc.TeX
import CurryDoc.CDoc
import CurryDoc.Config
import CurryDoc.PackageConfig (packagePath)

--------------------------------------------------------------------------
-- Global definitions:

banner :: String
banner = unlines [bannerLine,bannerText,bannerLine]
 where
 bannerText =
  "CurryDoc (" ++ currydocVersion ++ ") - the Curry Documentation Tool"
 bannerLine = take (length bannerText) (repeat '-')

-- Directory where include files for generated documention (e.g., icons,
-- css, tex includes) are stored:
includeDir :: String
includeDir = packagePath </> "include"

--------------------------------------------------------------------------
-- Check arguments and call main function:
main :: IO ()
main = do
  args <- getArgs
  processArgs defaultCurryDocOptions args

processArgs :: DocOptions -> [String] -> IO ()
processArgs params args = do
  putStrLn banner
  case args of
    -- no markdown
    ("--nomarkdown":margs) -> processArgs params { withMarkdown = False } margs
    -- documentation type
    ("--title" : t :margs) -> processArgs params { mainTitle = t } margs
    ("--html"      :margs) -> processArgs params { docType = HtmlDoc } margs
    ("--tex"       :margs) ->
      processArgs params { docType = TexDoc, withIndex = False } margs
    ("--cdoc"      :margs) ->
      processArgs params { docType = CDoc,   withIndex = False } margs
    -- HTML without index
    ["--noindexhtml",docdir,modname] ->
        makeCompleteDoc params { withIndex = False, docType = HtmlDoc }
                        True docdir (stripCurrySuffix modname)
    -- HTML index only
    ("--onlyindexhtml":docdir:modnames) ->
        makeIndexPages params docdir (map stripCurrySuffix modnames)
    ("--libsindexhtml":docdir:modnames) ->
        makeSystemLibsIndex params docdir modnames
    (('-':_):_) -> printUsageMessage
    -- module
    [modname] ->
        makeCompleteDoc params (docType params == HtmlDoc)
                        ("DOC_" ++ stripCurrySuffix (takeFileName modname))
                        (stripCurrySuffix modname)
    -- docdir + module
    [docdir,modname] ->
        makeCompleteDoc params (docType params == HtmlDoc) docdir
                        (stripCurrySuffix modname)
    _ -> printUsageMessage

printUsageMessage :: IO ()
printUsageMessage = do
  args <- getArgs
  putStrLn $ unlines
   [ "ERROR: Illegal arguments for CurryDoc: " ++ unwords args
   , ""
   , "Usage:"
   , "curry-doc [--title s] [--nomarkdown] [--html|--tex|--cdoc] [<doc_dir>] <module>"
   , "curry-doc [--title s] [--nomarkdown] --noindexhtml <doc_dir> <module>"
   , "curry-doc [--title s] --onlyindexhtml <doc_dir> <modules>"
   , "curry-doc [--title s] --libsindexhtml <doc_dir> <modules>"
   ]


-- create directory if not existent:
createDir :: String -> IO ()
createDir dir = do
  exdir <- doesDirectoryExist dir
  unless exdir $ system ("mkdir " ++ dir) >> done

--- Recursively copies a directory structure.
copyDirectory :: String -> String -> IO ()
copyDirectory src dst = do
  retCode <- system $ "cp -pR \"" ++ src ++ "\" \"" ++ dst ++ "\""
  when (retCode /= 0) $
    error $ "Copy failed with return code " ++ show retCode

--------------------------------------------------------------------------
--- The main function of the CurryDoc utility.
--- @param withindex - True if the index pages should also be generated
--- @param recursive - True if the documentation for the imported modules
---                    should be also generated (if necessary)
--- @param docdir - the directory name containing all documentation files
--- @param modname - the name of the main module to be documented
makeCompleteDoc :: DocOptions -> Bool -> String -> String -> IO ()
makeCompleteDoc docopts recursive reldocdir modpath = do
  docdir <- makeAbsolute reldocdir
  prepareDocDir (docType docopts) docdir
  lookupModuleSourceInLoadPath modpath >>=
   maybe (error $ "Source code of module '"++modpath++"' not found!")
    (\ (moddir,_) -> do
      let modname = takeFileName modpath
      setCurrentDirectory moddir
      -- parsing source program:
      callFrontendWithTarget FCY modname
      -- generate abstract curry representation
      callFrontendWithTarget ACY modname
      -- when constructing CDOC the imported modules don't have to be read
      -- from the FlatCurry file
      (alltypes,allfuns) <- getProg modname $ docType docopts
      makeDocIfNecessary docopts recursive docdir modname
      when (withIndex docopts) $ do
        genMainIndexPage     docopts docdir [modname]
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
makeIndexPages :: DocOptions -> String -> [String] -> IO ()
makeIndexPages docopts docdir modnames = do
  prepareDocDir HtmlDoc docdir
  (alltypes,allfuns) <- mapIO readTypesFuncs modnames >>= return . unzip
  genMainIndexPage     docopts docdir modnames
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
makeSystemLibsIndex :: DocOptions -> String -> [String] -> IO ()
makeSystemLibsIndex docopts docdir modnames = do
  -- generate index pages (main index, function index, constructor index)
  makeIndexPages docopts docdir modnames
  putStrLn ("Categorizing modules ...")
  modInfos <- mapIO getModInfo modnames
  putStrLn ("Grouping modules by categories ...")
  let grpMods = map sortByName $ groupByCategory $ sortByCategory modInfos
      cats    = sortBy (<=) $ nub $ map fst3 modInfos
  genSystemLibsPage docdir cats grpMods
 where
  fst3 (x,_,_)    = x
  snd3 (_,y,_)    = y
  sortByCategory  = sortBy  ((<=) `on` fst3)
  groupByCategory = groupBy ((==) `on` fst3)
  sortByName      = sortBy  ((<=) `on` snd3)

getModInfo :: String -> IO (Category,String,String)
getModInfo modname = do
  mmodsrc <- lookupModuleSourceInLoadPath modname
  case mmodsrc of
    Nothing -> error $ "Source code of module '"++modname++"' not found!"
    Just (_,progname) -> do
      modcmts <- readModuleComment progname
      let (modcmt,catcmts) = splitComment modcmts
          category         = readCategory $ getCommentType "category" catcmts
      return (category,modname,firstPassage modcmt)

-- create documentation directory (if necessary) with gifs and stylesheets:
prepareDocDir :: DocType -> String -> IO ()
prepareDocDir HtmlDoc docdir = do
  createDir docdir
  -- copy style sheets etc:
  let docstyledir = docdir </> "bt3"
  exdir <- doesDirectoryExist docstyledir
  unless exdir $ copyDirectory (includeDir </> "bt3") docstyledir
prepareDocDir TexDoc docdir = do
  createDir docdir
  putStrLn $ "Copy macros into documentation directory '"++docdir++"'..."
  copyIncludeIfPresent docdir "currydoc.tex"
prepareDocDir CDoc docdir = do
  createDir docdir
  putStrLn "Directory was succesfully created"

copyIncludeIfPresent :: String -> String -> IO ()
copyIncludeIfPresent docdir inclfile = do
  existIDir <- doesDirectoryExist includeDir
  when existIDir $
    system (unwords ["cp", includeDir </> inclfile, docdir]) >> done

-- read and generate all analysis infos:
readAnaInfo :: String -> IO AnaInfo
readAnaInfo modname = do
  initializeAnalysisSystem
  nondet   <- analyzeAndCheck nondetAnalysis
  complete <- analyzeAndCheck patCompAnalysis
  indet    <- analyzeAndCheck indetAnalysis
  solcomp  <- analyzeAndCheck solcompAnalysis
  return (AnaInfo (\qn -> nondet qn == NDet) complete indet solcomp)
 where
   analyzeAndCheck ana =
     analyzeInterface ana modname >>= either
       (\results ->
           return (\qn -> maybe (error $ "No '" ++ analysisName ana ++
                                 "' analysis result for function " ++ show qn)
                                id
                                (lookup qn results)))
       (\err -> error $ "Analysis error: " ++ err)

-- generate documentation for a single module:
makeDoc :: DocOptions -> Bool -> String -> String -> IO ()
makeDoc docopts recursive docdir modname = do
  Just (_,progname) <- lookupModuleSourceInLoadPath modname
  putStrLn ("Reading comments from file '"++progname++"'...")
  (modcmts,progcmts) <- readComments progname
  putStrLn ("Reading analysis information for module \""++modname++"\"...")
  anainfo <- readAnaInfo modname
  makeDocWithComments (docType docopts) docopts recursive docdir
                      anainfo modname modcmts progcmts


makeDocWithComments :: DocType -> DocOptions -> Bool -> String -> AnaInfo
                    -> String -> String -> [(SourceLine,String)] -> IO ()
makeDocWithComments HtmlDoc docopts recursive docdir anainfo modname
                    modcmts progcmts = do
  -- ensure that the AbstractCurry file for the module exists
  Just (dir,_) <- lookupModuleSourceInLoadPath modname
  let acyfile = dir </> abstractCurryFileName modname
  exacy <- doesFileExist acyfile
  unless exacy $ callFrontendWithTarget ACY modname
  writeOutfile docopts recursive docdir modname
               (generateHtmlDocs docopts anainfo modname modcmts progcmts)
  translateSource2ColoredHtml docdir modname
  writeOutfile cdocFileOptions False docdir modname
               (generateCDoc modname modcmts progcmts anainfo)


makeDocWithComments TexDoc docopts recursive docdir anainfo modname
                    modcmts progcmts = do
  writeOutfile docopts recursive docdir modname
               (generateTexDocs docopts anainfo modname modcmts progcmts)


makeDocWithComments CDoc docopts recursive docdir anainfo modname
                    modcmts progcmts = do
  writeOutfile docopts recursive docdir modname
               (generateCDoc modname modcmts progcmts anainfo)


--- Generates the documentation for a module if it is necessary.
--- I.e., the documentation is generated if no previous documentation
--- file exists or if the existing documentation file is older than
--- the FlatCurry file.
makeDocIfNecessary :: DocOptions -> Bool -> String -> String -> IO ()
makeDocIfNecessary docopts recursive docdir modname = do
  let docfile = docdir </> modname ++
                (if docType docopts == HtmlDoc then ".html" else ".tex")
  docexists <- doesFileExist docfile
  if not docexists
   then copyOrMakeDoc docopts recursive docdir modname
   else do
     ctime  <- getFlatCurryFileInLoadPath modname >>= getModificationTime
     dftime <- getModificationTime docfile
     if compareClockTime ctime dftime == GT
      then copyOrMakeDoc docopts recursive docdir modname
      else when recursive $ do
             imports <- getImports modname
             mapIO_ (makeDocIfNecessary docopts recursive docdir) imports

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

copyOrMakeDoc :: DocOptions -> Bool -> String -> String -> IO ()
copyOrMakeDoc docopts recursive docdir modname = do
  hasCopied <- copyDocIfPossible docopts docdir modname
  unless hasCopied $ makeDoc docopts recursive docdir modname

--- Copy the documentation file from standard documentation directoy "CDOC"
--- (used for documentation of system libraries) if possible.
--- Returns true if the copy was possible.
copyDocIfPossible :: DocOptions -> String -> String -> IO Bool
copyDocIfPossible docopts docdir modname =
  if docType docopts == TexDoc
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
writeOutfile :: DocOptions -> Bool -> String -> String -> IO String -> IO ()
writeOutfile docopts recursive docdir modname generate = do
  doc     <- generate
  imports <- getImports modname
  let outfile = docdir </> modname <.> fileExtension (docType docopts)
  putStrLn ("Writing documentation to \"" ++ outfile ++ "\"...")
  writeFile outfile doc
  when recursive $
    mapIO_ (makeDocIfNecessary docopts recursive docdir) imports

callFrontendWithTarget :: FrontendTarget -> String -> IO ()
callFrontendWithTarget t mp =
  callFrontendWithParams t (setDefinitions defs defaultParams) mp
 where
  defs = [( "__" ++ map toUpper curryCompiler ++ "__"
          , curryCompilerMajorVersion * 100 + curryCompilerMinorVersion )]

-- -----------------------------------------------------------------------
