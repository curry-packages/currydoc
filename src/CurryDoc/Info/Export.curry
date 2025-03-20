{- |
     Author  : Kai-Oliver Prott
     Version : March 2025

     Operations to inline module re-exports in export lists.
-}
module CurryDoc.Info.Export ( genExportList, inlineExport, flattenExport ) where

import Data.Maybe

import AbstractCurry.Types ( QName, MName, CurryProg(..) )
import AbstractCurry.Select
import Curry.Types
import Curry.Ident

import CurryDoc.Data.CurryDoc
import CurryDoc.Info.Comments
import CurryDoc.Info.Goodies

-- | Generates a default export list in case it is missing.
genExportList :: CurryProg -> [ExportEntry QName]
genExportList acy =
  (if null types
     then []
     else [ExportSection (LineComment "Exported Datatypes"  ) 1 $
             map ExportEntry types]) ++
  (if null funcs
     then []
     else [ExportSection (LineComment "Exported Functions"  ) 1 $
             map ExportEntry funcs]) ++
  (if null classes
     then []
     else [ExportSection (LineComment "Exported Classes"  ) 1 $
             map ExportEntry classes])
  where types   = publicTypeNames  acy
        funcs   = publicFuncNames  acy
        classes = publicClassNames acy

-- | Inlines module re-exports if the module is not imported completely.
inlineExport :: [ImportDecl]        -- ^ ImportDecls from AST
             -> [(MName, CurryDoc)] -- ^ CurryDoc for all imports (inc. Prelude)
             -> MName               -- ^ Name of current module
             -> [QName]             -- ^ Declarations of current module
             -> ExportEntry QName   -- ^ ExportStructure
             -> [ExportEntry QName] -- ^ Inlined ExportStructure
inlineExport _  _   _ _  e@(ExportEntry _          ) = [e]
inlineExport im imD m ds   (ExportSection c i ex   ) =
    [ExportSection c i (concatMap (inlineExport im imD m ds) ex)]
inlineExport im imD m ds e@(ExportEntryModule mname)
  | mname == m            = map ExportEntry ds
  | isFullImport im mname = [e]
  -- ^ This branch is taken, if the Prelude is not explicitly imported
  | otherwise             = case getRealModuleNameAndSpec im mname of
    Just (real, spec) -> maybe [] (inlineFromSpec spec) (lookup real imD)
    Nothing           -> error $ "CurryDoc.Info.Export.inlineExport: "
                                  ++ "Missing import for \"" ++ mname ++ "\""

inlineFromSpec :: ImportSpec -> CurryDoc -> [ExportEntry QName]
inlineFromSpec (Importing _ im) (CurryDoc _ _ ex _) =
  map ExportEntry $
  filter (\e ->       any (=~=e) qnames) $
  map curryDocDeclName $
  flattenExport ex
 where qnames = map importQName im
inlineFromSpec (Hiding    _ im) (CurryDoc _ _ ex _) =
  map ExportEntry $
  filter (\e -> not $ any (=~=e) qnames) $
  map curryDocDeclName $
  flattenExport ex
 where qnames = map importQName im

-- | Extracts the then name of an import entry.
importQName :: Import -> QName
importQName (Import         _ i  ) = identToQName i
importQName (ImportTypeAll  _ i  ) = identToQName i
importQName (ImportTypeWith _ i _) = identToQName i

-- | Extracts all "normal" entries inside an export list.
flattenExport :: [ExportEntry a] -> [a]
flattenExport = concatMap flattenEntry
 where flattenEntry (ExportEntry        a) = [a]
       flattenEntry (ExportSection _ _ ex) = flattenExport ex
       flattenEntry (ExportEntryModule  _) = []

-- | Returns true iff the module is not imported partially.
isFullImport :: [ImportDecl] -> MName -> Bool
isFullImport (ImportDecl _ _   _ (Just mid) spec : im) mname
  | mname == mIdentToMName mid = isNothing spec
  | otherwise                  = isFullImport im mname
isFullImport (ImportDecl _ mid _ Nothing    spec : im) mname
  | mname == mIdentToMName mid = isNothing spec
  | otherwise                  = isFullImport im mname
isFullImport [] _              = True

-- | Disambiguates alias (if present) and gets the `ImportSpec`.
getRealModuleNameAndSpec :: [ImportDecl] -> MName -> Maybe (MName, ImportSpec)
getRealModuleNameAndSpec (ImportDecl _ real _ (Just mid) spec : im) mname
  | mname == mIdentToMName mid = (,) <$> Just (mIdentToMName real) <*> spec
  | otherwise                  = getRealModuleNameAndSpec im mname
getRealModuleNameAndSpec (ImportDecl _ mid  _ Nothing    spec : im) mname
  | mname == mIdentToMName mid = (,) <$> Just (mIdentToMName mid)  <*> spec
  | otherwise                  = getRealModuleNameAndSpec im mname
getRealModuleNameAndSpec [] _  = Nothing
