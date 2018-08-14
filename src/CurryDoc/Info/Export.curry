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

inlineExport :: [ImportDecl] -> [(MName, CurryDoc)] -> ExportEntry CurryDocDecl
             -> [ExportEntry CurryDocDecl]
inlineExport _  _   e@(ExportEntry _          ) = [e]
inlineExport im imD   (ExportSection c i ex   ) =
    [ExportSection c i (concatMap (inlineExport im imD) ex)]
inlineExport im imD e@(ExportEntryModule mname)
  | isFullExport im mname = [e]
  | otherwise             = let (real, spec) = getRealModuleNameAndSpec im mname
                            in  maybe (error ("No CurryDoc for module \""
                                              ++ real ++ "\" found"))
                                      (inlineFromSpec spec)
                                      (lookup real imD)

inlineFromSpec :: ImportSpec -> CurryDoc -> [ExportEntry CurryDocDecl]
inlineFromSpec (Importing _ im) (CurryDoc mname _ ex _) =
  map ExportEntry $
  filter (      (`elem` importQNames mname im) . curryDocDeclName)
         (flattenExport ex)
inlineFromSpec (Hiding    _ im) (CurryDoc mname _ ex _) =
  map ExportEntry $ 
  filter (not . (`elem` importQNames mname im) . curryDocDeclName)
         (flattenExport ex)

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

getRealModuleNameAndSpec :: [ImportDecl] -> MName -> (MName, ImportSpec)
getRealModuleNameAndSpec (ImportDecl _ real _ (Just mid) spec : im) mname
  | mname == mIdentToMName mid    = (mIdentToMName real, fromJust spec)
  | otherwise                     = getRealModuleNameAndSpec im mname
getRealModuleNameAndSpec (ImportDecl _ mid  _ Nothing    spec : im) mname
  | mname == mIdentToMName mid    = (mIdentToMName mid, fromJust spec)
  | otherwise                     = getRealModuleNameAndSpec im mname
getRealModuleNameAndSpec [] mname = error ("No imported module \"" ++ mname ++
                                           "\" found")
