module CurryDoc.Info
  (generateCurryDocInfosWithAnalysis, generateCurryDocInfos,
   module CurryDoc.Info.Comments,
   module CurryDoc.Info.Goodies,
   module CurryDoc.Info.Header,
   module CurryDoc.Data.CurryDoc) where

import CurryDoc.Data.Span
import CurryDoc.Data.Type
import CurryDoc.Data.AnaInfo
import CurryDoc.Data.CurryDoc
import CurryDoc.Info.Comments
import CurryDoc.Info.Analysis
import CurryDoc.Info.Header
import CurryDoc.Info.Export
import CurryDoc.Info.AbstractCurry
import CurryDoc.Info.Goodies

import FlatCurry.Types     (Prog)
import AbstractCurry.Types (QName, MName, CurryProg(..))
import AbstractCurry.Select

import List  (partition, find)
import Maybe (fromMaybe)

generateCurryDocInfosWithAnalysis :: AnaInfo -> String -> [(Span, Comment)]
                                  -> Module a -> CurryProg -> Prog
                                  -> [(String, CurryDoc)] -> CurryDoc
generateCurryDocInfosWithAnalysis ai mn cs m acy@(CurryProg _ _ _ _ _ _ fun ops)
  = genCDoc (addAnaInfoToCurryDocDecls ai ops fun) mn cs m acy

generateCurryDocInfos ::            String -> [(Span, Comment)]
                      -> Module a -> CurryProg -> Prog
                      -> [(String, CurryDoc)] -> CurryDoc
generateCurryDocInfos mn cs m acy@(CurryProg _ _ _ _ _ _ fun ops)
  = genCDoc (addShortAnaInfoToCurryDocDecls ops fun) mn cs m acy

genCDoc :: ([CurryDocDecl] -> [CurryDocDecl])
        -> String -> [(Span, Comment)]
        -> Module a -> CurryProg -> Prog
        -> [(String, CurryDoc)] -> CurryDoc
genCDoc f mname cs m acy fprog importDoc =
  CurryDoc mname mhead exportStructure (imports acy)
  where
    (declsC, moduleC, exportList) = associateCurryDoc cs m
    decls = addAbstractCurryProg acy fprog declsC
    mhead = readModuleHeader moduleC
    exportStructure = concatMap (inlineExport (getImports m) importDoc) $
                      structureDecls (f decls) importDoc mname $
                      fromMaybe (genExportList acy) exportList

structureDecls :: [CurryDocDecl] -> [(String, CurryDoc)] -> MName
               -> [ExportEntry QName]
               -> [ExportEntry CurryDocDecl]
structureDecls _  _  _     []                            = []
structureDecls ds im mname (ExportSection c n ex : rest) =
  ExportSection c n (structureDecls ds im mname ex)
  : structureDecls ds im mname rest
structureDecls ds im mname (ExportEntry q        : rest) =
  maybe (maybe [] (:[]) (getFromImports im q))
        ((:[]) . ExportEntry)
        (lookupCurryDocDecl q ds) ++
  structureDecls ds im mname rest
structureDecls ds im mname (ExportEntryModule m  : rest)
  | mname == m = map ExportEntry ds ++ structureDecls ds im mname rest
  | otherwise  = ExportEntryModule m : structureDecls ds im mname rest

getFromImports :: [(String, CurryDoc)] -> QName -> Maybe (ExportEntry CurryDocDecl)
getFromImports im qname@(mname, _) =
  lookup mname im >>= getFromCurryDocWithName qname

getFromCurryDocWithName :: QName -> CurryDoc -> Maybe (ExportEntry CurryDocDecl)
getFromCurryDocWithName qname (CurryDoc _ _ ex _) =
  fmapM ExportEntry (find ((=~= qname) . curryDocDeclName) (flattenExport ex))
  where fmapM _ Nothing  = Nothing
        fmapM f (Just x) = Just (f x)

lookupCurryDocDecl :: QName -> [CurryDocDecl] -> Maybe CurryDocDecl
lookupCurryDocDecl _ []     = Nothing
lookupCurryDocDecl n (d:ds) = case d of
  CurryDocTypeDecl n' _ _ _
    | n =~= n'  -> Just d
    | otherwise -> lookupCurryDocDecl n ds
  CurryDocDataDecl n' _ _ _ _ _
    | n =~= n'  -> Just d
    | otherwise -> lookupCurryDocDecl n ds
  CurryDocNewtypeDecl n' _ _ _ _
    | n =~= n'  -> Just d
    | otherwise -> lookupCurryDocDecl n ds
  CurryDocClassDecl n' _ _ _ _
    | n =~= n'  -> Just d
    | otherwise -> lookupCurryDocDecl n ds
  CurryDocFunctionDecl n' _ _ _ _
    | n =~= n'  -> Just d
    | otherwise -> lookupCurryDocDecl n ds
