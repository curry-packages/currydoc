{- |

    Description : Implementation of CurryDoc, a utility for the automatic
                  generation of HTML documentation from Curry programs.
    Author      : Michael Hanus, Jan Tikovsky, Kai-Oliver Prott
    Version     : August 2018
-}
--  * All comments prefixed by a CurryDoc comment ("-- |", "{- |",
--    "-- ^" or "{- ^") are are considered for documentation.
--
--  * Any other comment on a line following a CurryDoc comment
--    will also be considered
--
--  * The comment of a module must occur before the first "module" or
--    "import" line of this module.
--
--  * Headings and sub-headings can be inserted in the documentation via
--    Comments starting with
--    "-- \*" or "-- \*\*", ...
--
--  * The exact rules on how comments are associated with syntactic elements
--    (similar to Haddock) are documented at: TODO

module CurryDoc.Main (main, debug) where

import Directory
import Distribution
import FileGoodies  (getFileInPath, lookupFileInPath)
import FilePath
import Function
import List
import Maybe        (fromJust)
import System
import Time
import ReadShowTerm
import AnsiCodes    (red, blue)

import AbstractCurry.Files
import AbstractCurry.Types
import AbstractCurry.Select
import FlatCurry.Files
import FlatCurry.Types (Prog(..))

import CurryDoc.Data.AnaInfo
import CurryDoc.Data.Type
import CurryDoc.Files         (generateModuleDocMapping)
import CurryDoc.Options
import CurryDoc.Generators
import CurryDoc.Info
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

-- | Check arguments and call main function:
main :: IO ()
main = do
  args <- getArgs
  putStrLn banner
  processArgs defaultCurryDocOptions args

-- | call CurryDoc with given parameters
debug :: [String] -> IO ()
debug opts = do
  dir <- getCurrentDirectory
  processArgs defaultCurryDocOptions opts
  setCurrentDirectory dir

processArgs :: DocOptions -> [String] -> IO ()
processArgs opts args = do
  case args of
    -- no markdown
    ("--nomarkdown" : margs) -> processArgs opts { withMarkdown = False } margs
    -- no analysis
    ("--noanalysis" : margs) -> processArgs opts { withAnalysis = False } margs
    -- do not generate documentation for imported modules
    ("--norecursive" : margs) -> processArgs opts { recursive = False } margs
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
        makeCompleteDoc opts' docdir (stripCurrySuffix modname)
    -- HTML index only
    ("--onlyindexhtml":docdir:modnames) -> do
        opts' <- processOpts opts
        let modpaths = map stripCurrySuffix modnames
        prepareDocDir HtmlDoc docdir
        prepareWithTargets [ACY] modpaths
        genIndexPages opts' docdir modpaths
    ("--libsindexhtml":docdir:modnames) -> do
        opts' <- processOpts opts
        let modpaths = map stripCurrySuffix modnames
        prepareDocDir HtmlDoc docdir
        prepareWithTargets [ACY, SAST, COMMS] modpaths
        genSystemLibsIndex opts' docdir modnames
    (('-':_):_) -> printUsageMessage
    -- module
    [modname] -> do
        opts' <- processOpts opts
        makeCompleteDoc opts'
                        ("DOC_" ++ stripCurrySuffix (takeFileName modname))
                        (stripCurrySuffix modname)
    -- docdir + module
    [docdir,modname] -> do
        opts' <- processOpts opts
        makeCompleteDoc opts' docdir (stripCurrySuffix modname)
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
   , "curry-doc --version"
   , ""
   , "where <options> can be:"
   , "  --title s     : Title of the main HTML documentation page"
   , "  --use dir@url : use for all Curry programs in <dir> the documentation"
   , "                  already stored at <url>"
   , "  --nomarkdown  : do not process markdown code in comments"
   , "  --noanalysis  : do not generate any further analysis information for functions"
   , "  --norecursive : do not generate documentation for imported modules"
   ]


-- create directory if not existent:
createDir :: String -> IO ()
createDir dir = do
  exdir <- doesDirectoryExist dir
  unless exdir $ system ("mkdir -p " ++ dir) >> done

-- Recursively copies a directory structure.
copyDirectory :: String -> String -> IO ()
copyDirectory src dst = do
  retCode <- system $ "cp -pR \"" ++ src ++ "\" \"" ++ dst ++ "\""
  when (retCode /= 0) $
    error $ "Copy failed with return code " ++ show retCode

-- The main function of the CurryDoc utility.
makeCompleteDoc :: DocOptions -> String -> String -> IO ()
makeCompleteDoc docopts reldocdir modpath = do
    docdir <- makeAbsolute reldocdir
    prepareDocDir (docType docopts) docdir
    prepareWithTargets targets [modpath]
    putStrLn "Start generating documentation"
    let modname = takeFileName modpath
    when (recursive docopts) $ (makeRecursiveDoc docopts docdir modname []
                                >> done)
    makeDocIfNecessary docopts docdir modname
    when (withIndex docopts) $ genIndexPages docopts docdir [modname]
    -- change access rights to readable for everybody:
    system ("chmod -R go+rX " ++ docdir)
    putStrLn ("Documentation files written into directory " ++ docdir)
  where targets = [ACY, SAST, COMMS, FCY, FINT]

-- Transform a file path into an absolute file path:
makeAbsolute :: String -> IO String
makeAbsolute f =
  if isAbsolute f
  then return f
  else do curdir <- getCurrentDirectory
          return (curdir </> f)

-- compile to the specified targets
prepareWithTargets :: [FrontendTarget] -> [String] -> IO ()
prepareWithTargets targets modnames = do
    putStrLn "Compiling modules..."
    flip mapIO modnames (\modpath -> lookupModuleSourceInLoadPath modpath >>=
      maybe (error $ "Source code of module '"++modpath++"' not found!")
        (\ (moddir,_) -> do
          let modname = takeFileName modpath
          setCurrentDirectory moddir
          -- parsing source program
          callFrontendFor modname targets))
    done
  where callFrontendFor _       []             = return ()
        callFrontendFor modname (target:other) = do
          params <- rcParams
          let paramsTargets = foldr addTarget params other
          callFrontendWithParams target (setQuiet True paramsTargets) modname

-- Generate only the index pages for a list of (already compiled!) modules:
genIndexPages :: DocOptions -> String -> [String] -> IO ()
genIndexPages docopts docdir modnames = do
  putStrLn "Generating index pages ..."
  (alltypes,allfuns,allclasses) <-
    mapIO readTypesFuncsClassesWithImports modnames >>= return . unzip3
  genMainIndexPage     docopts docdir modnames
  genFunctionIndexPage docopts docdir (concat allfuns)
  genConsIndexPage     docopts docdir (concat alltypes)
  genClassesIndexPage  docopts docdir (concat allclasses)
  -- change access rights to readable for everybody:
  system ("chmod -R go+rX "++docdir)
  done

-- Generate a system library index page categorizing the given
-- (already compiled!) modules
genSystemLibsIndex :: DocOptions -> String -> [String] -> IO ()
genSystemLibsIndex docopts docdir modnames = do
  -- generate index pages (main index, function index, constructor index)
  genIndexPages docopts docdir modnames
  putStrLn ("Reading module infos ...")
  cmts <- mapIO readComments modnames
  prog <- mapIO readShortAST modnames
  putStrLn ("Grouping modules by categories ...")
  let
      modInfos = zip modnames (map genModHeader (zip cmts prog))
      grpMods  = map sortByName $ groupByCategory $ sortByCategory modInfos
      cats     = sortBy (<=) $ nub $ map category modInfos
  genSystemLibsPage docdir cats grpMods
 where
  sortByCategory         = sortBy  ((<=) `on` category)
  groupByCategory        = groupBy ((==) `on` category)
  sortByName             = sortBy  ((<=) `on` fst)
  genModHeader (cmt, pr) = readModuleHeader $ snd3 $ associateCurryDoc cmt pr
  category (_, ModuleHeader xs _) = getCategoryWithDefault "general" xs
  snd3 (_, b, _) = b

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

-- generate documentation for a single module:
makeDoc :: DocOptions -> String -> String -> IO ()
makeDoc docopts docdir modname = do
  res <- makeAbstractDoc docopts modname
  makeDocForType (docType docopts) docopts docdir modname res

-- generate abstract CurryDoc for a single module
makeAbstractDoc :: DocOptions -> MName -> IO CurryDoc
makeAbstractDoc docopts modname = do
  putStrLn ("Reading comments for module \"" ++ modname ++ "\"...")
  cmts <- readComments modname
  when (any (isOldStyleComment . snd) cmts)
    (putStrLn (red
      "Warning: The CurryDoc comment-style \"--- \" is deprecated"))
  putStrLn ("Reading short-ast for module \"" ++ modname ++ "\"...")
  prog <- readShortAST modname
  putStrLn ("Reading flatcurry for module \"" ++ modname ++ "\"...")
  fprog <- readFlatCurry modname
  putStrLn ("Reading abstract curry for module \"" ++ modname ++ "\"...")
  acyname <- getLoadPathForModule modname >>=
             getFileInPath (abstractCurryFileName modname) [""]
  acy <- readAbstractCurryFile acyname
  putStrLn ("Recursively reading imported modules of \"" ++ modname ++ "\"...")
  allProg <- readCurryWithImports modname
  importsDoc <- mapIO (readOrGenerateCurryDoc docopts . progName) $ tail allProg
  res <- if withAnalysis docopts
         then do putStrLn ("Reading analysis information for module \""
                           ++ modname ++ "\"...")
                 ana <- readAnaInfo modname
                 return $ generateCurryDocInfosWithAnalysis
                            ana modname cmts prog acy fprog importsDoc
         else    return $ generateCurryDocInfos
                                modname cmts prog acy fprog importsDoc
  putStrLn ("Generating abstract CurryDoc for module \"" ++ modname ++ "\"...")
  writeFile (replaceExtension acyname "cydoc") (showQTerm res)
  return res

-- get the abstract CurryDoc for a file or generate it if necessary
readOrGenerateCurryDoc :: DocOptions -> String -> IO (String, CurryDoc)
readOrGenerateCurryDoc docopts modname =
  do cydoc <- getLoadPathForModule modname >>=
              lookupFileInPath (curryDocFileName modname) [""]
     case cydoc of
       Just doc -> do
           ctime <- getModificationTime (replaceExtension doc "fcy")
           htime <- getModificationTime doc
           if compareClockTime ctime htime == GT
             then do regenerate "outdated"
             else do content <- readFile doc
                     return (modname, readQTerm content)
       Nothing -> do regenerate "missing"
  where regenerate reason = do
         putStrLn (blue ("Note: Abstract CurryDoc for \"" ++ modname ++
                         "\" is " ++ reason ++ " and will be regenerated..."))
         res <- makeAbstractDoc docopts modname
         return (modname, res)

-- convert abstract CurryDoc to the respective target type
makeDocForType :: DocType -> DocOptions -> String -> String
               -> CurryDoc -> IO ()
makeDocForType HtmlDoc docopts docdir modname cdoc = do
  writeOutfile docopts docdir modname (generateHtmlDocs docopts cdoc)
  translateSource2ColoredHtml docdir modname

makeDocForType TexDoc docopts docdir modname cdoc = do
  writeOutfile docopts docdir modname (generateTexDocs docopts cdoc)

makeDocForType CDoc docopts docdir modname cdoc = do
  writeOutfile docopts docdir modname (generateCDoc docopts cdoc)


-- Generates the documentation for a module if it is necessary.
-- I.e., the documentation is generated if no previous documentation
-- file exists or if the existing documentation file is older than
-- the FlatCurry file.
makeDocIfNecessary :: DocOptions -> String -> String -> IO ()
makeDocIfNecessary docopts docdir modname = do
  when (modname `notElem` docMods docopts) $ do
    let docfile = docdir </> modname ++
                  (if docType docopts == HtmlDoc then ".html" else ".tex")
    docexists <- doesFileExist docfile
    if not docexists
     then do copyOrMakeDoc docopts docdir modname
     else do
       ctime  <- getFlatCurryFileInLoadPath modname >>= getModificationTime
       dftime <- getModificationTime docfile
       if compareClockTime ctime dftime == GT
        then copyOrMakeDoc docopts docdir modname
        else done

makeRecursiveDoc :: DocOptions -> String -> String -> [String] -> IO [String]
makeRecursiveDoc docopts docdir parentname processed = do
  imports <- getImports parentname
  foldM (\proc modname ->
           if modname `elem` proc
             then do return proc
             else do proc' <- makeRecursiveDoc docopts docdir modname proc
                     makeDocIfNecessary docopts docdir modname
                     return (modname : proc')) processed imports


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

copyOrMakeDoc :: DocOptions -> String -> String -> IO ()
copyOrMakeDoc docopts docdir modname = do
  hasCopied <- copyDocIfPossible docopts docdir modname
  unless hasCopied $ makeDoc docopts docdir modname

-- Copy the documentation file from standard documentation directoy "CDOC"
-- (used for documentation of system libraries) if possible.
-- Returns true if the copy was possible.
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

-- auxiliaries:

-- reads all types and function declarations (also imported ones) of
-- a module:
readTypesFuncsClassesWithImports :: String
                                 -> IO ([CTypeDecl],[CFuncDecl],[CClassDecl])
readTypesFuncsClassesWithImports modname = do
  allprogs <- readCurryWithImports modname
  let (ts,fs,cs) = unzip3 (map (\ (CurryProg _ _ _ cls _ types funs _)
                                  -> (types,funs,cls)) allprogs)
  return (concat ts, concat fs, concat cs)

-- get the associated file extenstion from DocType
fileExtension :: DocType -> String
fileExtension HtmlDoc = "html"
fileExtension TexDoc  = "tex"
fileExtension CDoc    = "cdoc"

-- harmonized writeFile function for all docType
writeOutfile :: DocOptions -> String -> String -> IO String -> IO ()
writeOutfile docopts docdir modname generate = do
  doc     <- generate
  let outfile = docdir </> modname <.> fileExtension (docType docopts)
  putStrLn ("Writing documentation to \"" ++ outfile ++ "\"...")
  writeFile outfile doc

-- transform the path of a curry programm to the path of the abstract CurryDoc
curryDocFileName :: FilePath -> FilePath
curryDocFileName modname = inCurrySubdir (stripCurrySuffix modname) <.> "cydoc"
