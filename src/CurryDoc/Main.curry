{- |

    Description : Implementation of CurryDoc, a utility for the automatic
                  generation of HTML documentation from Curry programs.
    Author      : Michael Hanus, Jan Tikovsky, Kai-Oliver Prott
    Version     : September 2025
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
--    (similar to Haddock) are documented at: docs/manual.tex

module CurryDoc.Main ( main, debug ) where

import System.Directory    ( createDirectoryIfMissing
                           , findFileWithSuffix, getFileWithSuffix
                           , getAbsolutePath, getHomeDirectory
                           , getCurrentDirectory, setCurrentDirectory
                           , doesDirectoryExist, doesFileExist
                           , getModificationTime )
import System.Environment  ( getArgs )
import System.CurryPath    ( getPackageVersionOfDirectory
                           , lookupModuleSourceInLoadPath, getLoadPathForModule
                           , inCurrySubdir, setCurryPathIfNecessary
                           , stripCurrySuffix )
import System.FrontendExec ( FrontendParams, FrontendTarget (..), addTarget
                           , rcParams, setQuiet, callFrontendWithParams )
import System.IO
import System.Process      ( system )
import System.FilePath
import Data.Time           ( compareClockTime )
import Data.Maybe          ( fromJust )
import Data.Function
import Data.List
import ReadShowTerm
import Control.Applicative ( when )
import Control.Monad       ( unless, foldM )

import AbstractCurry.Files
import AbstractCurry.Types
import AbstractCurry.Select
import FlatCurry.Files
import FlatCurry.Types (Prog(..))
import Curry.Types
import Curry.Files
import System.Console.ANSI.Codes ( red, green )

import CurryDoc.Data.AnaInfo
import CurryDoc.Files         ( generateModuleDocMapping )
import CurryDoc.PackageConfig ( packagePath )
import CurryDoc.Options
import CurryDoc.Generators
import CurryDoc.Info
import CurryDoc.Config

--------------------------------------------------------------------------
-- Global definitions:

-- | Banner of the CurryDoc tool.
banner :: String
banner = unlines [bannerLine,bannerText,bannerLine]
 where
 bannerText =
  "CurryDoc (" ++ currydocVersion ++ ") - the Curry Documentation Tool"
 bannerLine = take (length bannerText) (repeat '-')

-- | Directory where include files for generated documention (e.g., icons,
--   css, tex includes) are stored:
includeDir :: String
includeDir = packagePath </> "include"

-- | Checks and processes the args before running CurryDoc.
main :: IO ()
main = do
  args <- getArgs
  putStrLn banner
  processArgs defaultCurryDocOptions args

-- | Calls CurryDoc with the given parameters.
debug :: [String] -> IO ()
debug opts = do
  dir <- getCurrentDirectory
  processArgs defaultCurryDocOptions opts
  setCurrentDirectory dir

-- | Processes the command line arguments.
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
    ("--json"      : margs) ->
      processArgs opts { docType = JSON,   withIndex = False } margs
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

-- | Processes the original user options into the form required by CurryDoc.
processOpts :: DocOptions -> IO DocOptions
processOpts opts = do
  modurls <- generateModuleDocMapping (useDirURL opts)
  return $ opts { docMods = map fst modurls
                , docURL  = \m -> maybe m (\b -> b </> m) (lookup m modurls) }

-- | Prints a usage message.
printUsageMessage :: IO ()
printUsageMessage = do
  printError

  putStrLn $ unlines
   [ "Usage:"
   , "curry-doc <options> [--html|--tex|--json|--cdoc] [<doc_dir>] <module>"
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
 where 
  printError = do
    args <- getArgs
    putStrLn $ "ERROR: " ++ 
        if null args 
          then "No arguments given!"
          else "Illegal arguments for CurryDoc: " ++ unwords args

-- | Creates a directory if it's not existent.
createDir :: String -> IO ()
createDir dir = do
  exdir <- doesDirectoryExist dir
  unless exdir $ system ("mkdir -p " ++ dir) >> return ()

-- | Recursively copies a directory structure.
copyDirectory :: String -> String -> IO ()
copyDirectory src dst = do
  retCode <- system $ "cp -pR \"" ++ src ++ "\" \"" ++ dst ++ "\""
  when (retCode /= 0) $
    error $ "Copy failed with return code " ++ show retCode

-- | The main function of the CurryDoc utility.
makeCompleteDoc :: DocOptions -> String -> String -> IO ()
makeCompleteDoc docopts reldocdir modpath = do
    docdir <- getAbsolutePath reldocdir
    prepareDocDir (docType docopts) docdir
    prepareWithTargets targets [modpath]
    putStrLn "Start generating documentation"
    let modname = takeFileName modpath
    when (recursive docopts) $
      (makeRecursiveDoc docopts docdir modname []) >> return ()
    makeDocIfNecessary docopts docdir modname
    when (withIndex docopts) $ genIndexPages docopts docdir [modname]
    -- change access rights to readable for everybody:
    system ("chmod -R go+rX " ++ docdir)
    putStrLn ("Documentation files written into directory " ++ docdir)
  where targets = [ACY, SAST, COMMS, FCY, FINT]

-- | Compiles to the specified targets.
prepareWithTargets :: [FrontendTarget] -> [String] -> IO ()
prepareWithTargets targets modnames = do
  putStrLn $ "Compiling module" ++ (if length modnames > 1 then "s" else "") ++
             concatMap (" " ++) modnames ++ "..."
  flip mapM_ modnames (\modpath -> lookupModuleSourceInLoadPath modpath >>=
    maybe (error $ "Source code of module '"++modpath++"' not found!")
      (\ (moddir,_) -> do
        let modname = takeFileName modpath
        setCurrentDirectory moddir
        -- parsing source program
        callFrontendFor modname targets))
 where
  callFrontendFor _       []             = return ()
  callFrontendFor modname (target:other) = do
    params <- rcParams
    let paramsTargets = foldr addTarget params other
    callFrontendWithParams target (setQuiet True paramsTargets) modname

-- | Generates only the index pages for a list of (already compiled!) modules.
genIndexPages :: DocOptions -> String -> [String] -> IO ()
genIndexPages docopts docdir modnames = do
  putStrLn "Generating index pages ..."
  (alltypes,allfuns,allclasses) <-
    mapM readTypesFuncsClassesWithImports modnames >>= return . unzip3
  genMainIndexPage     docopts docdir modnames
  genFunctionIndexPage docopts docdir (concat allfuns)
  genConsIndexPage     docopts docdir (concat alltypes)
  genClassesIndexPage  docopts docdir (concat allclasses)
  -- change access rights to readable for everybody:
  system ("chmod -R go+rX "++docdir) >> return ()

-- | Generate a system library index page categorizing the given
--   (already compiled!) modules.
genSystemLibsIndex :: DocOptions -> String -> [String] -> IO ()
genSystemLibsIndex docopts docdir modnames = do
  -- generate index pages (main index, function index, constructor index)
  genIndexPages docopts docdir modnames
  putStrLn ("Reading module infos ...")
  cmts <- mapM readComments modnames
  prog <- mapM readShortAST modnames
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

-- | Creates documentation directory (if necessary) with GIFs and stylesheets.
prepareDocDir :: DocType -> String -> IO ()
prepareDocDir HtmlDoc docdir = do
  createDir docdir
  -- copy style sheets etc:
  let docstyledir = docdir </> "bt4"
  exdir <- doesDirectoryExist docstyledir
  unless exdir $ copyDirectory (includeDir </> "bt4") docstyledir
prepareDocDir TexDoc docdir = do
  createDir docdir
  putStrLn $ "Copy macros into documentation directory '"++docdir++"'..."
  copyIncludeIfPresent docdir "currydoc.tex"
prepareDocDir JSON docdir = do
  createDir docdir
  putStrLn "Directory was created succesfully"
prepareDocDir CDoc docdir = do
  createDir docdir
  putStrLn "Directory was created succesfully"

copyIncludeIfPresent :: String -> String -> IO ()
copyIncludeIfPresent docdir inclfile = do
  existIDir <- doesDirectoryExist includeDir
  when existIDir $
    system (unwords ["cp", includeDir </> inclfile, docdir]) >> return ()

-- | Generates documentation for a single module.
makeDoc :: DocOptions -> String -> String -> IO ()
makeDoc docopts docdir modname = do
  res <- getAbstractCurryDoc docopts modname
  makeDocForType (docType docopts) docopts docdir modname res
  -- generate also CDoc:
  unless (docType docopts == CDoc) $ do
    makeDocForType CDoc (docopts { docType    = CDoc
                                 , withIndex   = False
                                 , withMarkdown = False
                                 }) 
                   docdir modname res

-- | Returns the CurryDoc infos for a module or generate it if necessary.
getAbstractCurryDoc :: DocOptions -> MName -> IO CurryDoc
getAbstractCurryDoc docopts modname =
 tryReadCurryDocFromCache modname >>= either regenerate return
 where
  regenerate reason = do
    putStrLn $ green $ "Note: CurryDoc infos of module '" ++ modname ++
                      "' is " ++ reason ++ " and will be regenerated..."
    makeAbstractDoc docopts modname

-- | Generates CurryDoc infos for a single module.
makeAbstractDoc :: DocOptions -> MName -> IO CurryDoc
makeAbstractDoc docopts modname = do
  putStrLn $ "Generating CurryDoc info of module '" ++ modname ++ "'..."
  putStrLn $ "Reading comments for module '" ++ modname ++ "'..."
  cmts <- readComments modname
  when (any (isOldStyleComment . snd) cmts) $
    putStrLn $ red "Warning: The CurryDoc comment-style \"--- \" is deprecated"
  putStrLn $ "Reading short-ast for module '" ++ modname ++ "'..."
  prog <- readShortAST modname
  putStrLn $ "Reading AbstractCurry for module '" ++ modname ++ "'..."
  acy <- readCurry modname
  let impmods = imports acy
  putStrLn $ "Getting CurryDoc infos of imported modules" ++
             concatMap (' ':) impmods ++ "..."
  importsDoc <- fmap (zip impmods) $ mapM (getAbstractCurryDoc docopts) impmods
  res <- if withAnalysis docopts
           then do putStrLn $ "Getting analysis information for module '" ++
                              modname ++ "'..."
                   ana <- readAnaInfo modname
                   return $ generateCurryDocInfosWithAnalysis
                              ana modname cmts prog acy importsDoc
           else return $ generateCurryDocInfos modname cmts prog acy importsDoc
  tryWriteCurryDocToCache modname res
  return res

-- | Converts CurryDoc infos to the respective target type.
makeDocForType :: DocType -> DocOptions -> String -> MName -> CurryDoc -> IO ()
makeDocForType HtmlDoc docopts docdir modname cdoc = do
  writeOutfile docopts docdir modname (generateHtmlDocs docopts cdoc)
  translateSource2ColoredHtml docdir modname
makeDocForType TexDoc  docopts docdir modname cdoc = do
  writeOutfile docopts docdir modname (generateTexDocs docopts cdoc)
makeDocForType JSON    docopts docdir modname cdoc = do
  writeOutfile docopts docdir modname (generateJSON cdoc)
makeDocForType CDoc    docopts docdir modname cdoc = do
  writeOutfile docopts docdir modname (generateCDoc cdoc)

-- | Generates the documentation for a module if it is necessary.
--   I.e., the documentation is generated if no previous documentation
--   file exists or if the existing documentation file is older than
--   the FlatCurry file.
makeDocIfNecessary :: DocOptions -> String -> MName -> IO ()
makeDocIfNecessary docopts docdir modname = do
  when (modname `notElem` docMods docopts) $ do
    let docfile = docdir </> modname ++
                  (if docType docopts == HtmlDoc then ".html" else ".tex")
    docexists <- doesFileExist docfile
    if not docexists
      then makeDoc docopts docdir modname
      else do
        ctime  <- getFlatCurryFileInLoadPath modname >>= getModificationTime
        dftime <- getModificationTime docfile
        when (compareClockTime ctime dftime == GT) $ 
          makeDoc docopts docdir modname

makeRecursiveDoc :: DocOptions -> String -> String -> [String] -> IO [String]
makeRecursiveDoc docopts docdir parentname processed = do
  imports <- getImports parentname
  foldM (\proc modname ->
           if modname `elem` proc
             then do return proc
             else do proc' <- makeRecursiveDoc docopts docdir modname proc
                     makeDocIfNecessary docopts docdir modname
                     return (modname : proc'))
        processed imports

------------------------------------------------------------------------------
-- Auxiliaries:

-- get imports of a module by reading the interface, if possible:
getImports :: String -> IO [String]
getImports modname = do
  mbfintfile <- getLoadPathForModule modname >>=
                findFileWithSuffix (flatCurryIntName modname) [""]
  (Prog _ imports _ _ _) <- maybe (getFlatCurryFileInLoadPath modname >>=
                                   readFlatCurryFile)
                                  readFlatCurryFile
                                  mbfintfile
  return imports

-- | Reads all types and function declarations (also imported ones) of
--   a module:
readTypesFuncsClassesWithImports :: String
                                 -> IO ([CTypeDecl],[CFuncDecl],[CClassDecl])
readTypesFuncsClassesWithImports modname = do
  allprogs <- readCurryWithImports modname
  let (ts,fs,cs) = unzip3 (map (\ (CurryProg _ _ _ cls _ types funs _)
                                  -> (types,funs,cls)) allprogs)
  return (concat ts, concat fs, concat cs)

-- | Returns the associated file extenstion from DocType.
fileExtension :: DocType -> String
fileExtension HtmlDoc = "html"
fileExtension TexDoc  = "tex"
fileExtension JSON    = "json"
fileExtension CDoc    = "cdoc"

-- | Harmonized writeFile function for all docType.
writeOutfile :: DocOptions -> String -> String -> IO String -> IO ()
writeOutfile docopts docdir modname generate = do
  doc <- generate
  let dtype   = docType docopts
      outfile = docdir </> modname <.> fileExtension dtype
  putStrLn $ "Writing " ++ show dtype ++ " to '" ++ outfile ++ "'..."
  writeFile outfile doc

------------------------------------------------------------------------------
-- Operations to read/write CurryDoc infos into the CurryDoc cache.
-- The CurryDoc infos of module `M` is stored in cache file
-- * `$HOME/.curry_doc_cache/packages/PKG-VERS/M.cydoc` if module `M`
--   is defined in package `PKG` with version `VERS`
-- * otherwise in the local directory `.curry_doc_cache/M.cydoc`
--   (which is typically inside the directory `src` in local packages)

-- Gets the name of the cache file for a module defined in a package version.
getCurryDocCache4PkgMod :: String -> String -> String -> IO (Maybe FilePath)
getCurryDocCache4PkgMod pname vers mname = do
  homedir <- getHomeDirectory
  if null homedir
    then return Nothing
    else return $ Just $ homedir </> ".curry_doc_cache" </> "packages" </>
                         pname ++ "-" ++ vers </> mname <.> ".cydoc"

-- Gets the name of the cache file for a module not defined in a package.
getCurryDocCache4Mod :: String -> IO (Maybe FilePath)
getCurryDocCache4Mod mname = do
  curdir <- getCurrentDirectory
  if null curdir
    then return Nothing
    else return $ Just $ curdir </> ".curry_doc_cache" </> mname <.> ".cydoc"

-- Try to write the CurryDoc infos for a module into the CurryDoc cache.
tryWriteCurryDocToCache :: String -> CurryDoc -> IO ()
tryWriteCurryDocToCache modname cdoc = do
  setCurryPathIfNecessary
  mbsrc <- lookupModuleSourceInLoadPath modname
  case mbsrc of
    Nothing          -> return ()
    Just (dirname,_) ->
      getPackageVersionOfDirectory dirname >>= maybe
        (getCurryDocCache4Mod modname >>= maybe (return ()) writeCDocFile)
        (\(pname,vers) -> getCurryDocCache4PkgMod pname vers modname >>=
                            maybe (return ()) writeCDocFile )
 where
  writeCDocFile cdocfile = do
    createDirectoryIfMissing True (dropFileName cdocfile)
    putStrLn $ "Writing CurryDoc infos to " ++ cdocfile ++ "..."
    writeFile cdocfile (show cdoc)

-- Try to read the CurryDoc infos of a module from the CurryDoc cache.
-- The result is either a short reason why there is no actual CurryDoc file
-- or the actual CurryDoc file.
tryReadCurryDocFromCache :: String -> IO (Either String CurryDoc)
tryReadCurryDocFromCache modname = do
  setCurryPathIfNecessary
  mbsrc <- lookupModuleSourceInLoadPath modname
  case mbsrc of
    Nothing -> return (Left "missing")
    Just (dirname,srcname) ->
      getPackageVersionOfDirectory dirname >>= maybe
        (getCurryDocCache4Mod modname >>= tryReadCDocFile srcname)
        (\(pname,vers) ->
         getCurryDocCache4PkgMod pname vers modname >>= tryReadCDocFile srcname)
 where
  tryReadCDocFile _       Nothing         = return (Left "missing")
  tryReadCDocFile srcfile (Just cdocfile) = do
    excdoc <- doesFileExist cdocfile
    if not excdoc
      then return (Left "missing")
      else do
        stime <- getModificationTime srcfile
        ctime <- getModificationTime cdocfile
        if compareClockTime stime ctime == GT
          then return (Left "outdated")
          else fmap Right (readCurryDocFile cdocfile)

  -- Reads a file containing a CurryDoc term.
  readCurryDocFile :: FilePath -> IO CurryDoc
  readCurryDocFile fname = do
    putStrLn $ "Reading CurryDoc info from '" ++ fname ++ "'..."
    content <- openFile fname ReadMode >>= hGetContents
    return (readUnqualifiedTerm
              [ "Prelude"
              , "CurryDoc.Data.CurryDoc" 
              , "CurryDoc.Data.AnaInfo"
              , "CurryDoc.Info.Header"
              , "CurryDoc.Info.Comments"
              , "AbstractCurry.Types"
              , "Analysis.TotallyDefined"] content)

------------------------------------------------------------------------------
