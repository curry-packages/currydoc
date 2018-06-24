----------------------------------------------------------------------
--- Some auxiliary operations to lookup Curry source files.
---
--- @author Michael Hanus
--- @version December 2017
----------------------------------------------------------------------

module CurryDoc.Files ( generateModuleDocMapping )
 where

import Directory    ( doesDirectoryExist, getDirectoryContents )
import Distribution ( stripCurrySuffix )
import FilePath     ( (</>), takeExtension )

--- Constructs a mapping from module names into locations where
--- the documentation is stored. The argument is a list of pairs
--- (root dir of Curry sources / documentation dir for these sources).
generateModuleDocMapping :: [(String,String)] -> IO [(String,String)]
generateModuleDocMapping pkglocs =
  mapIO genPkgMapping pkglocs >>= return . concat
 where
  genPkgMapping (srcroot,docroot) = do
    mods <- curryModulesInDir srcroot
    return $ map (\m -> (m,docroot)) mods

--- Gets the names of all Curry modules contained in a directory.
--- Modules in subdirectories are returned as hierarchical modules.
curryModulesInDir :: String -> IO [String]
curryModulesInDir dir = getModules "" dir
 where
  getModules p d = do
    exdir <- doesDirectoryExist d
    entries <- if exdir then getDirectoryContents d else return []
    let realentries = filter (\f -> length f >= 1 && head f /= '.') entries
        newprogs    = filter (\f -> takeExtension f == ".curry") realentries
    subdirs <- mapIO (\e -> doesDirectoryExist (d </> e) >>=
                            \b -> return $ if b then [e] else []) realentries
               >>= return . concat
    subdirentries <- mapIO (\s -> getModules (p ++ s ++ ".") (d </> s)) subdirs
    return $ map ((p ++) . stripCurrySuffix) newprogs ++ concat subdirentries
