----------------------------------------------------------------------
--- Implementation of CurryDoc, a utility for the automatic
--- generation of HTML documentation from Curry programs.
---
--- @author Michael Hanus, Jan Tikovsky
--- @version November 2020
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

import Control.Monad      ( unless, when )
import Data.Function
import Data.List
import Data.Maybe         ( fromJust )
import Data.Time
import System.Environment
import System.Directory
import System.FilePath
import System.Process

import AbstractCurry.Files
import FlatCurry.Types
import FlatCurry.Files
import FlatCurry.Read     ( readFlatCurryWithImports )

import Analysis.Deterministic
import Analysis.TotallyDefined
import Analysis.Indeterministic
import Analysis.SolutionCompleteness
import Analysis.Types      ( analysisName )
import CASS.Server         ( initializeAnalysisSystem, analyzeInterface )
import System.CurryPath    ( stripCurrySuffix, lookupModuleSourceInLoadPath
                           , getLoadPathForModule )
import System.FrontendExec ( FrontendTarget (..), callFrontend )

import CurryDoc.AnaInfo
import CurryDoc.Files   ( generateModuleDocMapping )
import CurryDoc.Options
import CurryDoc.Read
import CurryDoc.Html
import CurryDoc.TeX
import CurryDoc.CDoc
import CurryDoc.Config
import CurryDoc.PackageConfig ( packagePath )

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
  putStrLn banner
  processArgs defaultCurryDocOptions args

processArgs :: DocOptions -> [String] -> IO ()
processArgs opts args = do
  case args of
    -- no markdown
    ("--nomarkdown" : margs) -> processArgs opts { withMarkdown = False } margs
    -- documentation type
    ("--title" : t : margs) -> processArgs opts { mainTitle = t } margs
    ("--use"   : t : margs) ->
       let (src,url) = break (=='@') t
       in if null url
            then error "URL missing in --use option!"
            else processArgs opts { useDirURL = useDirURL opts ++
                                                    [(src,tail url)] } margs
    ("--html"      : margs) -> processArgs opts { docType = HtmlDoc } margs
    ("--tex"       : margs) ->
      processArgs opts { docType = TexDoc, withIndex = False } margs
    ("--cdoc"      : margs) ->
      processArgs opts { docType = CDoc,   withIndex = False } margs
    -- HTML without index
    ["--noindexhtml",docdir,modname] -> do
        opts' <- processOpts opts { withIndex = False, docType = HtmlDoc }
        makeCompleteDoc opts' True docdir (stripCurrySuffix modname)
    -- HTML index only
    ("--onlyindexhtml":docdir:modnames) -> do
        opts' <- processOpts opts
        makeIndexPages opts' docdir (map stripCurrySuffix modnames)
    ("--libsindexhtml":docdir:modnames) -> do
        opts' <- processOpts opts
        makeSystemLibsIndex opts' docdir modnames
    (('-':_):_) -> printUsageMessage
    -- module
    [modname] -> do
        opts' <- processOpts opts
        makeCompleteDoc opts' (docType opts == HtmlDoc)
                        ("DOC_" ++ stripCurrySuffix (takeFileName modname))
                        (stripCurrySuffix modname)
    -- docdir + module
    [docdir,modname] -> do
        opts' <- processOpts opts
        makeCompleteDoc opts' (docType opts == HtmlDoc) docdir
                        (stripCurrySuffix modname)
    _ -> printUsageMessage

-- Process the original user options into the form required by CurryDoc.
processOpts :: DocOptions -> IO DocOptions
processOpts opts = do
  modurls <- generateModuleDocMapping (useDirURL opts)
  return $ opts { docMods = map fst modurls
                , docURL  = \m -> maybe m (\b -> b </> m) (lookup m modurls) }

printUsageMessage :: IO ()
printUsageMessage = do
  args <- getArgs
  putStrLn $ unlines
   [ "ERROR: Illegal arguments for CurryDoc: " ++ unwords args
   , ""
   , "Usage:"
   , "curry-doc <options> [--html|--tex|--cdoc] [<doc_dir>] <module>"
   , "curry-doc <options> --noindexhtml   <doc_dir> <module>"
   , "curry-doc <options> --onlyindexhtml <doc_dir> <modules>"
   , "curry-doc <options> --libsindexhtml <doc_dir> <modules>"
   , ""
   , "where <options> can be:"
   , "  --title s    : Title of the main HTML documentation page"
   , "  --use dir@url: use for all Curry programs in <dir> the documentation"
   , "                 already stored at <url>"
   , "  --nomarkdown : do not process markdown code in comments"
   ]


-- create directory if not existent:
createDir :: String -> IO ()
createDir dir = do
  exdir <- doesDirectoryExist dir
  unless exdir $ system ("mkdir -p " ++ dir) >> return ()

--- Recursively copies a directory structure.
copyDirectory :: String -> String -> IO ()
copyDirectory src dst = do
  retCode <- system $ "cp -pR \"" ++ src ++ "\" \"" ++ dst ++ "\""
  when (retCode /= 0) $
    error $ "Copy failed with return code " ++ show retCode

--------------------------------------------------------------------------
--- The main function of the CurryDoc utility.
--- @param docopts   - the options for CurryDoc
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
      callFrontend FCY modname
      -- generate abstract curry representation
      callFrontend ACY modname
      -- when constructing CDOC the imported modules don't have to be read
      -- from the FlatCurry file
      (alltypes,allfuns) <- getProg modname $ docType docopts
      makeDocIfNecessary docopts recursive docdir modname
      when (withIndex docopts) $ do
        genMainIndexPage     docopts docdir [modname]
        genFunctionIndexPage docopts docdir allfuns
        genConsIndexPage     docopts docdir alltypes
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
  (alltypes,allfuns) <- mapM readTypesFuncs modnames >>= return . unzip
  genMainIndexPage     docopts docdir modnames
  genFunctionIndexPage docopts docdir (concat allfuns)
  genConsIndexPage     docopts docdir (concat alltypes)
  -- change access rights to readable for everybody:
  system ("chmod -R go+rX "++docdir)
  return ()
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
  modInfos <- mapM getModInfo modnames
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
    system (unwords ["cp", includeDir </> inclfile, docdir]) >> return ()

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
  unless exacy $ callFrontend ACY modname
  writeOutfile docopts recursive docdir modname
               (generateHtmlDocs docopts anainfo modname modcmts progcmts)
  translateSource2ColoredHtml docdir modname
  writeOutfile docopts { docType = CDoc, withIndex = False
                       , withMarkdown = False }
               False docdir modname
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
makeDocIfNecessary docopts recursive docdir modname =
 when (modname `notElem` docMods docopts) $ do
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
             mapM_ (makeDocIfNecessary docopts recursive docdir) imports

-- get imports of a module by reading the interface, if possible:
getImports :: String -> IO [String]
getImports modname = do
  mbfintfile <- getLoadPathForModule modname >>=
                findFileWithSuffix (flatCurryIntName modname) [""]
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
    mapM_ (makeDocIfNecessary docopts recursive docdir) imports

-- -----------------------------------------------------------------------
