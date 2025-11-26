module CurryDoc.Generators.JSON ( generateJSON ) where

import CurryDoc.Info
import CurryDoc.Info.Export
import CurryDoc.Data.CurryDoc

import JSON.Data
import JSON.Pretty
import AbstractCurry.Types
import AbstractCurry.Pretty
import Text.Pretty

import Data.List

class ToJSON a where
  toJSON :: a -> JValue

-- | Converts a CurryDoc to a JSON string.
generateJSON :: CurryDoc -> IO String
generateJSON = return . ppJSON . toJSON

instance ToJSON CurryDoc where
  toJSON (CurryDoc name (ModuleHeader header _) ds _) = JObject
    ([("module", JString name)] ++
     map jsonHeader  header     ++
     [jsonFunctions fs, jsonTypes ts, jsonTypeclasses cs])
    where ds' = flattenExport ds
          fs  = filter isCurryDocFuncDecl  ds'
          ts  = filter isCurryDocTypeDecl  ds'
          cs  = filter isCurryDocClassDecl ds'

instance ToJSON CurryDocDecl where
  toJSON (CurryDocTypeDecl    (mname, name) vs      ty   cs) = JObject
    [ (       "datatype", JString "type"          )
    , (    "module-name", JString mname           )
    , ( "type-variables", jsonVars vs             )
    , (           "name", JString  name           )
    , (           "type", toJSON ty               )
    , (       "comments", toJSON cs               )]
  toJSON (CurryDocDataDecl    (mname, name) vs _ ex cons cs) = JObject
    [ (       "datatype", JString "data"          )
    , (    "module-name", JString mname           )
    , (           "name", JString  name           )
    , ( "type-variables", jsonVars vs             )
    , (       "external", toJSON ex               )
    , (   "constructors", JArray (map (toJSONCons r) cons ))
    , (       "comments", toJSON cs               )]
    where r     = foldr (\v t -> CTApply t (CTVar v)) (CTCons (mname, name)) vs
  toJSON (CurryDocNewtypeDecl (mname, name) vs _    cons cs) = JObject
    [ (       "datatype", JString "newtype"       )
    , (    "module-name", JString mname           )
    , (           "name", JString  name           )
    , ( "type-variables", jsonVars vs             )
    , (   "constructors", JArray (map (toJSONCons r) cons'))
    , (       "comments", toJSON cs               )]
    where r     = foldr (\v t -> CTApply t (CTVar v)) (CTCons (mname, name)) vs
          cons' = case cons of
                    Just c  -> [c]
                    Nothing -> []

  toJSON (CurryDocClassDecl (mname, name) cx vs fdeps ds cs) = JObject
    [ (     "module-name", JString mname          )
    , (            "name", JString  name          )
    , (         "context", toJSON cx              )
    , (  "type-variables", JArray $ map jsonVar vs)
    , ( "class-functions", toJSON ds              )
    , ( "functional-deps", toJSON fdeps           )
    , (        "comments", toJSON cs              )]

  toJSON (CurryDocFunctionDecl (mname, name) ty _ _ cs) = JObject
    [ ("module-name", JString mname)
    , (       "name", JString  name)
    , (       "type", toJSON ty    )
    , (   "comments", toJSON cs    )]

instance ToJSON CurryDocFunDep where
  toJSON (CurryDocFunDep (xs, ys)) = JObject 
    [ ("lhs", jsonVars xs) 
    , ("rhs", jsonVars ys)]

instance ToJSON a => ToJSON [a] where
  toJSON = JArray . map toJSON

instance ToJSON Bool where
  toJSON True  = JTrue
  toJSON False = JFalse

instance ToJSON CContext where
  toJSON (CContext cs) = JArray (map (JString . ppConstraint) cs)

instance ToJSON CQualTypeExpr where
  toJSON (CQualType (CContext []        ) ty) =
    JString (ppType ty)
  toJSON (CQualType (CContext [x]       ) ty) =
    JString (ppConstraint x ++ " => " ++ ppType ty)
  toJSON (CQualType (CContext xs@(_:_:_)) ty) =
    JString ("(" ++ intercalate ", " (map ppConstraint xs) ++ ") => "
                 ++ ppType ty)

instance ToJSON CTypeExpr where
  toJSON = JString . ppType

instance ToJSON Comment where
  toJSON (NestedComment c) = JString c
  toJSON (LineComment   c) = JString c

toJSONCons :: CTypeExpr -> CurryDocCons -> JValue
toJSONCons ty (CurryDocConstr (mname, name) args        _ cs) = JObject
    [ ("constructor", JString "constructor"         )
    , ("module-name", JString mname                 )
    , (       "name", JString  name                 )
    , (       "type", jsonListTypes (args ++ [ty])  )
    , (   "comments", toJSON cs                     )]
toJSONCons ty (CurryDocConsOp (mname, name) arg1 arg2   _ cs) = JObject
    [ ("constructor", JString "operator"            )
    , ("module-name", JString mname                 )
    , (       "name", JString  name                 )
    , (       "type", jsonListTypes [arg1, arg2, ty])
    , (   "comments", toJSON cs                     )]
toJSONCons ty (CurryDocRecord (mname, name) args fields _ cs) = JObject
    [ ("constructor", JString "constructor"         )
    , ("module-name", JString mname                 )
    , (       "name", JString  name                 )
    , (       "type", jsonListTypes (args ++ [ty])  )
    , (     "fields", toJSON fields                 )
    , (   "comments", toJSON cs                     )]

instance ToJSON CurryDocField where
  toJSON (CurryDocField (mname, name) ty _ cs) = JObject
    [ ("module-name", JString mname)
    , (       "name", JString  name)
    , (       "type", toJSON ty    )
    , (   "comments", toJSON cs    )]


jsonListTypes :: [CTypeExpr] -> JValue
jsonListTypes = JString . intercalate " -> " . map ppType

jsonVar :: CTVarIName -> JValue
jsonVar = JString . snd

jsonVars :: [CTVarIName] -> JValue
jsonVars = JArray . map jsonVar

ppConstraint :: CConstraint -> String
ppConstraint ((_, name), ts) = name ++ " " ++ unwords (map ppType ts)

ppType :: CTypeExpr -> String
ppType = unwords . concatMap words . lines
       . pPrint . ppCTypeExpr (setNoQualification defaultOptions)

jsonHeader :: (HeaderField, String) -> (String, JValue)
jsonHeader (Description, d) = ("description", JString d)
jsonHeader (Category   , d) = ("category"   , JString d)
jsonHeader (Author     , d) = ("author"     , JString d)
jsonHeader (Version    , d) = ("version"    , JString d)

jsonFunctions   :: [CurryDocDecl] -> (String, JValue)
jsonFunctions   ds = ("functions"  , toJSON ds)

jsonTypes       :: [CurryDocDecl] -> (String, JValue)
jsonTypes       ds = ("types"      , toJSON ds)

jsonTypeclasses :: [CurryDocDecl] -> (String, JValue)
jsonTypeclasses ds = ("typeclasses", toJSON ds)
