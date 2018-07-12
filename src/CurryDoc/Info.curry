module CurryDoc.Info
  (generateCurryDocInfosWithAnalysis, generateCurryDocInfos,
   module CurryDoc.Info.Comments,
   module CurryDoc.Info.Header,
   module CurryDoc.Info.AbstractCurry,
   module CurryDoc.Info.Goodies,
   module CurryDoc.Data.CurryDoc) where

import CurryDoc.Data.Span
import CurryDoc.Data.Type
import CurryDoc.Data.AnaInfo
import CurryDoc.Data.CurryDoc
import CurryDoc.Info.Comments
import CurryDoc.Info.Analysis
import CurryDoc.Info.Header
import CurryDoc.Info.AbstractCurry
import CurryDoc.Info.Goodies

import FlatCurry.Types     (Prog)
import AbstractCurry.Types (QName, MName, CurryProg(..))
import AbstractCurry.Select

import List  (partition)
import Maybe (fromMaybe)

generateCurryDocInfosWithAnalysis :: String -> [(Span, Comment)] -> Module a
                                  -> CurryProg -> Prog -> AnaInfo
                                  -> CurryDoc
generateCurryDocInfosWithAnalysis mn cs m acy@(CurryProg _ _ _ _ _ _ fs os) fprog ai
  = genCDoc (addAnaInfoToCurryDocDecls ai os fs) mn cs m acy fprog

generateCurryDocInfos :: String -> [(Span, Comment)] -> Module a -> CurryProg
                      -> Prog -> CurryDoc
generateCurryDocInfos mn cs m acy@(CurryProg _ _ _ _ _ _ fs os) fprog
  = genCDoc (addShortAnaInfoToCurryDocDecls os fs) mn cs m acy fprog

genCDoc :: ([CurryDocDecl] -> [CurryDocDecl])
        -> String -> [(Span, Comment)] -> Module a -> CurryProg -> Prog
        -> CurryDoc
genCDoc f mname cs m acy fprog =
  CurryDoc mname mhead (structureDecls (f decls)
                         (fromMaybe (genExportList acy) exportList))
           (imports acy)
  where
    (declsC, moduleC, exportList) = associateCurryDoc cs m
    decls = addAbstractCurryProg acy fprog declsC
    mhead = readModuleHeader moduleC

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

structureDecls :: [CurryDocDecl] -> [ExportEntry QName]
               -> [ExportEntry CurryDocDecl]
structureDecls _  [] = []
structureDecls ds (ExportSection c n ex:rest) =
  ExportSection c n (structureDecls ds ex) : structureDecls ds rest
structureDecls ds (ExportEntry q : rest) =
  maybe [] ((:[]) . ExportEntry) (lookupCurryDocDecl q ds) ++
  structureDecls ds rest
structureDecls ds (ExportEntryModule m : rest) =
  ExportEntryModule m : structureDecls ds rest

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
