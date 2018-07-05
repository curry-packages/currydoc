module CurryDoc.Info
  (module CurryDoc.Info,
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

import List (partition)

--- CurryDoc mName mhead instances typesigs decls exports imports
data CurryDoc =
  CurryDoc String ModuleHeader [CommentedDecl] [CommentedDecl] [CommentedDecl] Exports [MName]
  deriving Show

--- DataTypes Constructors Fields Functions Typeclasses
type Exports = ([QName], [QName], [QName], [QName], [QName])

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
  CurryDoc mname mhead instances sigs (f decls)
    (publicTypeNames  acy, publicConsNames acy,
     publicFieldNames acy, publicFuncNames acy,
     publicClassNames acy)
    (imports acy)
  where
    (declsC, moduleC) = associateCurryDoc cs m
    (sigs, other) = partition isCommentedTypeSig declsC
    (inst, decls) = partition isCommentedInstanceDecl $
                              addAbstractCurryProg acy other
    instances = collectInstanceInfo types inst
    mhead = readModuleHeader moduleC
