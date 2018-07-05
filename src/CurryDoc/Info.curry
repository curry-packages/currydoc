module CurryDoc.Info
  (generateCurryDocInfosWithAnalysis, generateCurryDocInfos, CurryDoc(..),
   module CurryDoc.Info.Comments,
   module CurryDoc.Info.Analysis,
   module CurryDoc.Info.Header,
   module CurryDoc.Info.AbstractCurry,
   module CurryDoc.Info.Goodies) where

import CurryDoc.Data.Span
import CurryDoc.Data.Type
import CurryDoc.Data.AnaInfo
import CurryDoc.Info.Comments
import CurryDoc.Info.Analysis
import CurryDoc.Info.Header
import CurryDoc.Info.AbstractCurry
import CurryDoc.Info.Goodies

import AbstractCurry.Types (QName, MName, CurryProg(..))
import AbstractCurry.Select

import List  (partition)
import Maybe (fromMaybe)

--- CurryDoc mName mhead instances typesigs exports imports
data CurryDoc =
  CurryDoc String ModuleHeader [CommentedDecl] [CommentedDecl]
    [ExportEntry CommentedDecl] [MName]
  deriving Show

generateCurryDocInfosWithAnalysis :: String -> [(Span, Comment)] -> Module a
                                  -> CurryProg -> AnaInfo
                                  -> CurryDoc
generateCurryDocInfosWithAnalysis mn cs m acy@(CurryProg _ _ _ _ _ _ fs os) ai
  = genCDoc (addAnaInfoToCommentDecls ai os fs) mn cs m acy

generateCurryDocInfos :: String -> [(Span, Comment)] -> Module a -> CurryProg
                      -> CurryDoc
generateCurryDocInfos = genCDoc id -- TODO: dont skip all analysis

genCDoc :: ([CommentedDecl] -> [CommentedDecl])
        -> String -> [(Span, Comment)] -> Module a -> CurryProg
        -> CurryDoc
genCDoc f mname cs m acy@(CurryProg _ _ _ _ _ types _ _) =
  CurryDoc mname mhead instances sigs
    (structureDecls (f decls)
      (fromMaybe (genExportList acy) exportList))
    (imports acy)
  where
    (declsC, moduleC, exportList) = associateCurryDoc cs m
    (sigs, other) = partition isCommentedTypeSig declsC
    (inst, decls) = partition isCommentedInstanceDecl $
                              addAbstractCurryProg acy other
    instances = collectInstanceInfo types inst
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

structureDecls :: [CommentedDecl] -> [ExportEntry QName]
               -> [ExportEntry CommentedDecl]
structureDecls _  [] = []
structureDecls ds (ExportSection c n ex:rest) =
  ExportSection c n (structureDecls ds ex) : structureDecls ds rest
structureDecls ds (ExportEntry q : rest) =
  maybe [] ((:[]) . ExportEntry) (lookupClassDataFuncDecl q ds) ++ -- TODO: can 'Nothing' actually be a result of the lookup?
  structureDecls ds rest
structureDecls ds (ExportEntryModule m : rest) =
  ExportEntryModule m : structureDecls ds rest
