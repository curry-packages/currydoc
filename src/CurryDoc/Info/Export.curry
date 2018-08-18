module CurryDoc.Info.Export (genExportList, inlineExport, flattenExport) where

import Maybe

import AbstractCurry.Types (QName, MName, CurryProg(..))
import AbstractCurry.Select

import CurryDoc.Data.Type
import CurryDoc.Data.Ident
import CurryDoc.Data.CurryDoc
import CurryDoc.Info.Comments
import CurryDoc.Info.Goodies

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

inlineExport :: [ImportDecl] -> [(MName, CurryDoc)] -> [QName]
             -> ExportEntry QName
             -> [ExportEntry QName]
inlineExport _  _   ds e@(ExportEntry _          ) = [e]
inlineExport im imD ds   (ExportSection c i ex   ) =
    [ExportSection c i (concatMap (inlineExport im imD ds) ex)]
inlineExport im imD ds e@(ExportEntryModule mname)
  | isFullExport im mname = [e]
  | otherwise             = case getRealModuleNameAndSpec im mname of
    Just (real, spec) -> maybe [] (inlineFromSpec spec) (lookup real imD)
    Nothing           -> ds -- has to be current module, insert decls of it

inlineFromSpec :: ImportSpec -> CurryDoc -> [ExportEntry QName]
inlineFromSpec (Importing _ im) (CurryDoc mname _ ex _) =
  map ExportEntry $
  filter (      (`elem` importQNames mname im))
  map curryDocDeclName $
  flattenExport ex
inlineFromSpec (Hiding    _ im) (CurryDoc mname _ ex _) =
  map ExportEntry $
  filter (not . (`elem` importQNames mname im))
  map curryDocDeclName $
  flattenExport ex

importQNames :: MName -> [Import] -> [QName]
importQNames mname = map (importQName mname)

importQName :: MName -> Import -> QName
importQName mname (Import         _ (Ident _ s _)  ) = (mname, s)
importQName mname (ImportTypeAll  _ (Ident _ s _)  ) = (mname, s)
importQName mname (ImportTypeWith _ (Ident _ s _) _) = (mname, s)

flattenExport :: [ExportEntry a] -> [a]
flattenExport = concatMap flattenEntry

flattenEntry :: ExportEntry a -> [a]
flattenEntry (ExportEntry        a) = [a]
flattenEntry (ExportSection _ _ ex) = flattenExport ex
flattenEntry (ExportEntryModule  _) = []

isFullExport :: [ImportDecl] -> MName -> Bool
isFullExport (ImportDecl _ _   _ (Just mid) spec : im) mname
  | mname == mIdentToMName mid = isNothing spec
  | otherwise                  = isFullExport im mname
isFullExport (ImportDecl _ mid _ Nothing    spec : im) mname
  | mname == mIdentToMName mid = isNothing spec
  | otherwise                  = isFullExport im mname
isFullExport [] _              = True

getRealModuleNameAndSpec :: [ImportDecl] -> MName -> Maybe (MName, ImportSpec)
getRealModuleNameAndSpec (ImportDecl _ real _ (Just mid) spec : im) mname
  | mname == mIdentToMName mid    = Just (mIdentToMName real, fromJust spec)
  | otherwise                     = getRealModuleNameAndSpec im mname
getRealModuleNameAndSpec (ImportDecl _ mid  _ Nothing    spec : im) mname
  | mname == mIdentToMName mid    = Just (mIdentToMName mid, fromJust spec)
  | otherwise                     = getRealModuleNameAndSpec im mname
getRealModuleNameAndSpec [] _     = Nothing
