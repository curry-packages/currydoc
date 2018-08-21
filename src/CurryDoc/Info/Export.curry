{- |
     Author  : Kai-Oliver Prott
     Version : August 2018

     Operations to inline module re-exports in export lists.
-}
module CurryDoc.Info.Export (genExportList, inlineExport, flattenExport) where

import Maybe

import AbstractCurry.Types (QName, MName, CurryProg(..))
import AbstractCurry.Select

import CurryDoc.Data.Type
import CurryDoc.Data.Ident
import CurryDoc.Data.CurryDoc
import CurryDoc.Info.Comments
import CurryDoc.Info.Goodies

-- | Generate a default export list in case it is missing
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

-- | inline module re-exports if the module is not imported completely
inlineExport :: [ImportDecl] -> [(MName, CurryDoc)] -> [QName]
             -> ExportEntry QName
             -> [ExportEntry QName]
inlineExport _  _   _  e@(ExportEntry _          ) = [e]
inlineExport im imD ds   (ExportSection c i ex   ) =
    [ExportSection c i (concatMap (inlineExport im imD ds) ex)]
inlineExport im imD ds e@(ExportEntryModule mname)
  | isFullImport im mname = [e]
  | otherwise             = case getRealModuleNameAndSpec im mname of
    Just (real, spec) -> maybe [] (inlineFromSpec spec) (lookup real imD)
    Nothing           -> map ExportEntry ds -- has to be current module

inlineFromSpec :: ImportSpec -> CurryDoc -> [ExportEntry QName]
inlineFromSpec (Importing _ im) (CurryDoc mname _ ex _) =
  map ExportEntry $
  filter (      (`elem` importQNames mname im)) $
  map curryDocDeclName $
  flattenExport ex
inlineFromSpec (Hiding    _ im) (CurryDoc mname _ ex _) =
  map ExportEntry $
  filter (not . (`elem` importQNames mname im)) $
  map curryDocDeclName $
  flattenExport ex

-- HACK the qualifier might be wrong if the identifier is re-exported
-- and originally defined in another module.
-- This has the nice side-effect, that those will not be in the
-- documentation, even if mname is re-exported fully.
-- (PACKS and KICS2 will not export them either)
importQNames :: MName -> [Import] -> [QName]
importQNames mname = map (importQName mname)

importQName :: MName -> Import -> QName
importQName mname (Import         _ (Ident _ s _)  ) = (mname, s)
importQName mname (ImportTypeAll  _ (Ident _ s _)  ) = (mname, s)
importQName mname (ImportTypeWith _ (Ident _ s _) _) = (mname, s)

-- | Get all "normal" entries inside an export list
flattenExport :: [ExportEntry a] -> [a]
flattenExport = concatMap flattenEntry

flattenEntry :: ExportEntry a -> [a]
flattenEntry (ExportEntry        a) = [a]
flattenEntry (ExportSection _ _ ex) = flattenExport ex
flattenEntry (ExportEntryModule  _) = []

isFullImport :: [ImportDecl] -> MName -> Bool
isFullImport (ImportDecl _ _   _ (Just mid) spec : im) mname
  | mname == mIdentToMName mid = isNothing spec
  | otherwise                  = isFullImport im mname
isFullImport (ImportDecl _ mid _ Nothing    spec : im) mname
  | mname == mIdentToMName mid = isNothing spec
  | otherwise                  = isFullImport im mname
isFullImport [] _              = False

getRealModuleNameAndSpec :: [ImportDecl] -> MName -> Maybe (MName, ImportSpec)
getRealModuleNameAndSpec (ImportDecl _ real _ (Just mid) spec : im) mname
  | mname == mIdentToMName mid    = Just (mIdentToMName real, fromJust spec)
  | otherwise                     = getRealModuleNameAndSpec im mname
getRealModuleNameAndSpec (ImportDecl _ mid  _ Nothing    spec : im) mname
  | mname == mIdentToMName mid    = Just (mIdentToMName mid, fromJust spec)
  | otherwise                     = getRealModuleNameAndSpec im mname
getRealModuleNameAndSpec [] _     = Nothing
