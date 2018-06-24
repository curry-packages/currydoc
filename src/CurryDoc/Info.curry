module CurryDoc.Info
  (module CurryDoc.Info,
   module CurryDoc.Info.Comments,
   module CurryDoc.Info.Analysis,
   module CurryDoc.Info.Header,
   module CurryDoc.Info.AbstractCurry) where

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

--- CurryDoc mhead instances typesigs decls exports imports
data CurryDoc =
  CurryDoc ModuleHeader [CommentedDecl] [CommentedDecl] [CommentedDecl] [QName] [MName]
  deriving Show

generateCurryDocInfosWithAnalysis :: [(Span, Comment)] -> Module a
                                  -> CurryProg -> AnaInfo
                                  -> CurryDoc
generateCurryDocInfosWithAnalysis cs m acy@(CurryProg _ _ _ _ _ typ funs ops) ai 
  = CurryDoc mhead instances sigs withAnaInfo
    (publicTypeNames acy ++ publicConsNames  acy ++
     publicFuncNames acy ++ publicFieldNames acy)
    (imports acy)
  where
    (declsC, moduleC) = associateCurryDoc cs m
    (inst, sigs, decls) = partition3 isCommentedInstanceDecl
                                     isCommentedTypeSig $
                                     addAbstractCurryProg acy declsC
    withAnaInfo = addAnaInfoToCommentDecls ai ops funs decls
    instances = collectInstanceInfo typ inst
    mhead = readModuleHeader moduleC

generateCurryDocInfos :: [(Span, Comment)] -> Module a -> CurryProg
                      -> CurryDoc
generateCurryDocInfos cs m acy@(CurryProg _ _ _ _ _ types funs ops) =
  CurryDoc mhead instances sigs decls
    (publicTypeNames acy ++ publicConsNames  acy ++
     publicFuncNames acy ++ publicFieldNames acy)
    (imports acy)
  where
    (declsC, moduleC) = associateCurryDoc cs m
    (inst, sigs, decls) = partition3 isCommentedInstanceDecl
                                     isCommentedTypeSig $
                                     addAbstractCurryProg acy declsC
    instances = collectInstanceInfo types inst
    mhead = readModuleHeader moduleC

partition3 :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a], [a])
partition3 p1 p2 xs = (a, b, c)
  where (a, bc) = partition p1 xs
        (b, c)  = partition p2 bc
