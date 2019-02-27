{- |
     Author  : Kai-Oliver Prott
     Version : August 2018

     Datastructures and operations to generate abstract CurryDoc.
-}
module CurryDoc.Info
  (generateCurryDocInfosWithAnalysis, generateCurryDocInfos,
   module CurryDoc.Info.Comments,
   module CurryDoc.Info.Goodies,
   module CurryDoc.Info.Header,
   module CurryDoc.Data.CurryDoc) where

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
import Curry.Span
import Curry.Types

import List  (partition, find)
import Maybe (fromMaybe)

-- | Generate the abstract CurryDoc with extended analysis
generateCurryDocInfosWithAnalysis :: AnaInfo -> String -> [(Span, Comment)]
                                  -> Module a -> CurryProg -> Prog
                                  -> [(String, CurryDoc)] -> CurryDoc
generateCurryDocInfosWithAnalysis ai mn cs m acy@(CurryProg _ _ _ _ _ _ fun ops)
  = genCDoc (addAnaInfoToCurryDocDecls ai ops fun) mn cs m acy

-- | Generate the abstract CurryDoc with simple analysis
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
  CurryDoc mname mhead export (imports acy)
  where
    (declsC, moduleC, exportList) = associateCurryDoc cs m
    importDecls = map (\(x,CurryDoc _ _ ex _) -> (x,flattenExport ex)) importDoc
    decls  = addAbstractCurryProg acy fprog declsC
    mhead  = readModuleHeader moduleC
    export = structureDecls ((mname, f decls) : importDecls) mname $
             concatMap (inlineExport (getImports m) importDoc
                                     mname (map curryDocDeclName decls))
                       (fromMaybe (genExportList acy) exportList)

-- insert the declarations into the export structure
structureDecls :: [(String, [CurryDocDecl])] -> MName
               -> [ExportEntry QName]
               -> [ExportEntry CurryDocDecl]
structureDecls _  _     []                                 = []
structureDecls ds mname (ExportSection c n ex : rest) =
  ExportSection c n (structureDecls ds mname ex)
  : structureDecls ds mname rest
structureDecls ds mname (ExportEntry q        : rest) =
  maybe [] (:[]) (getFromModule ds q) ++
  structureDecls ds mname rest
structureDecls ds mname (ExportEntryModule m  : rest) =
  ExportEntryModule m : structureDecls ds mname rest

-- lookup the required declaration
getFromModule :: [(String, [CurryDocDecl])] -> QName
              -> Maybe (ExportEntry CurryDocDecl)
getFromModule im qname@(mname, _) =
  lookup mname im >>= lookupCurryDocDecl qname >>= return . ExportEntry

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
