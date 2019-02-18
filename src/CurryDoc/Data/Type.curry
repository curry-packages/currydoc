-- |
-- Description: AST for curry code
-- Author     : Kai-Oliver Prott
-- Version    : August 2018
--
-- An implementation of the curry AST from curry-frontend
-- without some types that cannot be in shortAST files
module CurryDoc.Data.Type where

import CurryDoc.Data.SpanInfo
import CurryDoc.Data.Ident
import CurryDoc.Data.Position

import Directory           ( doesFileExist )
import FileGoodies         ( getFileInPath, lookupFileInPath )
import FilePath            ( takeFileName, (</>), (<.>) )
import System.CurryPath    ( lookupModuleSourceInLoadPath, getLoadPathForModule
                           , inCurrySubdir, stripCurrySuffix )
import System.FrontendExec ( FrontendParams, FrontendTarget (..), defaultParams
                           , setQuiet, callFrontend, callFrontendWithParams )

-- | Reads the short-AST from a specified module
readShortAST :: String -> IO (Module ())
readShortAST progname =
   readShortASTWithParseOptions progname (setQuiet True defaultParams)

-- | Reads the short-AST with further options from a specified module
readShortASTWithParseOptions :: String -> FrontendParams -> IO (Module ())
readShortASTWithParseOptions progname options = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find shortAST file in load path
      loadpath <- getLoadPathForModule progname
      filename <- getFileInPath (shortASTFileName (takeFileName progname))
                                [""] loadpath
      readShortASTFile filename
    Just (dir,_) -> do
      callFrontendWithParams SAST options progname
      readShortASTFile (shortASTFileName (dir </> takeFileName progname))

-- | Get the short-AST filename of a curry programm
shortASTFileName :: String -> String
shortASTFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "sast"

-- | Reads the short-AST from a specified file
readShortASTFile :: String -> IO (Module ())
readShortASTFile filename = do
  filecontents <- readShortASTFileRaw filename
  return (read filecontents)

-- | Reads the text from a specified file containing ashort-AST
readShortASTFileRaw :: String -> IO String
readShortASTFileRaw filename = do
  extfcy <- doesFileExist filename
  if extfcy
   then readFile filename
   else do let subdirfilename = inCurrySubdir filename
           exdirtfcy <- doesFileExist subdirfilename
           if exdirtfcy
            then readFile subdirfilename
            else error ("EXISTENCE ERROR: Short AST file '" ++ filename ++
                        "' does not exist")

-- | This datatype is copied from curry-base.
data Module a = Module SpanInfo [ModulePragma] ModuleIdent
                       (Maybe ExportSpec) [ImportDecl] [Decl a]
    deriving (Eq, Read, Show)

data ModulePragma
  = LanguagePragma SpanInfo [Extension]
  | OptionsPragma  SpanInfo (Maybe Tool) String
    deriving (Eq, Read, Show)

data ExportSpec = Exporting SpanInfo [Export]
    deriving (Eq, Read, Show)

data Export
  = Export         SpanInfo QualIdent
  | ExportTypeWith SpanInfo QualIdent [Ident]
  | ExportTypeAll  SpanInfo QualIdent
  | ExportModule   SpanInfo ModuleIdent
    deriving (Eq, Read, Show)

data ImportDecl = ImportDecl SpanInfo ModuleIdent Qualified
                             (Maybe ModuleIdent) (Maybe ImportSpec)
    deriving (Eq, Read, Show)

type Qualified = Bool

data ImportSpec
  = Importing SpanInfo [Import]
  | Hiding    SpanInfo [Import]
    deriving (Eq, Read, Show)

data Import
  = Import         SpanInfo Ident
  | ImportTypeWith SpanInfo Ident [Ident]
  | ImportTypeAll  SpanInfo Ident
    deriving (Eq, Read, Show)

data Decl a
  = InfixDecl        SpanInfo Infix (Maybe Precedence) [Ident]
  | DataDecl         SpanInfo Ident [Ident] [ConstrDecl] [QualIdent]
  | ExternalDataDecl SpanInfo Ident [Ident]
  | NewtypeDecl      SpanInfo Ident [Ident] NewConstrDecl [QualIdent]
  | TypeDecl         SpanInfo Ident [Ident] TypeExpr
  | TypeSig          SpanInfo [Ident] QualTypeExpr
  | FunctionDecl     SpanInfo a Ident [Equation a]
  | ExternalDecl     SpanInfo [Var a]
  | DefaultDecl      SpanInfo [TypeExpr]
  | ClassDecl        SpanInfo Context Ident Ident [Decl a]
  | InstanceDecl     SpanInfo Context QualIdent InstanceType [Decl a]
    deriving (Eq, Read, Show)

type Precedence = Int

data Infix
  = InfixL
  | InfixR
  | Infix
    deriving (Eq, Read, Show)

data ConstrDecl
  = ConstrDecl SpanInfo [Ident] Context Ident [TypeExpr]
  | ConOpDecl  SpanInfo [Ident] Context TypeExpr Ident TypeExpr
  | RecordDecl SpanInfo [Ident] Context Ident [FieldDecl]
    deriving (Eq, Read, Show)

data NewConstrDecl
  = NewConstrDecl SpanInfo Ident TypeExpr
  | NewRecordDecl SpanInfo Ident (Ident, TypeExpr)
   deriving (Eq, Read, Show)

data FieldDecl = FieldDecl SpanInfo [Ident] TypeExpr
  deriving (Eq, Read, Show)

data TypeExpr
  = ConstructorType SpanInfo QualIdent
  | ApplyType       SpanInfo TypeExpr TypeExpr
  | VariableType    SpanInfo Ident
  | TupleType       SpanInfo [TypeExpr]
  | ListType        SpanInfo TypeExpr
  | ArrowType       SpanInfo TypeExpr TypeExpr
  | ParenType       SpanInfo TypeExpr
  deriving (Eq, Read, Show)

data QualTypeExpr = QualTypeExpr SpanInfo Context TypeExpr
    deriving (Eq, Read, Show)

type Context = [Constraint]

data Constraint = Constraint SpanInfo QualIdent TypeExpr
  deriving (Eq, Read, Show)

type InstanceType = TypeExpr

data Equation a = Equation SpanInfo (Lhs a) (Rhs a)
  deriving (Eq, Read, Show)

data Lhs a
  = FunLhs SpanInfo Ident [Pattern a]
  | OpLhs  SpanInfo (Pattern a) Ident (Pattern a)
  | ApLhs  SpanInfo (Lhs a) [Pattern a]
  deriving (Eq, Read, Show)

data Rhs a
  = SimpleRhs  SpanInfo (Expression a) [Decl a]
  | GuardedRhs SpanInfo [CondExpr a] [Decl a]
  deriving (Eq, Read, Show)

data CondExpr a = CondExpr SpanInfo (Expression a) (Expression a)
  deriving (Eq, Read, Show)

data Literal
  = Char   Char
  | Int    Int
  | Float  Float
  | String String
  deriving (Eq, Read, Show)

data Pattern a
  = LiteralPattern     SpanInfo a Literal
  | NegativePattern    SpanInfo a Literal
  | VariablePattern    SpanInfo a Ident
  | ConstructorPattern SpanInfo a QualIdent [Pattern a]
  | InfixPattern       SpanInfo a (Pattern a) QualIdent (Pattern a)
  | ParenPattern       SpanInfo (Pattern a)
  | RecordPattern      SpanInfo a QualIdent [Field (Pattern a)]
  | TuplePattern       SpanInfo [Pattern a]
  | ListPattern        SpanInfo a [Pattern a]
  | AsPattern          SpanInfo Ident (Pattern a)
  | LazyPattern        SpanInfo (Pattern a)
  | FunctionPattern    SpanInfo a QualIdent [Pattern a]
  | InfixFuncPattern   SpanInfo a (Pattern a) QualIdent (Pattern a)
  deriving (Eq, Read, Show)

data Expression a = Variable SpanInfo a QualIdent
  deriving (Eq, Read, Show)

data Field a = Field SpanInfo QualIdent a
  deriving (Eq, Read, Show)

data Var a = Var a Ident
  deriving (Eq, Read, Show)

data Extension
  = KnownExtension   Position KnownExtension
  | UnknownExtension Position String
  deriving (Eq, Read, Show)

data KnownExtension
  = AnonFreeVars
  | CPP
  | ExistentialQuantification
  | FunctionalPatterns
  | NegativeLiterals
  | NoImplicitPrelude
  deriving (Eq, Read, Show)

data Tool = KICS2 | PAKCS | CYMAKE | FRONTEND | UnknownTool String
  deriving (Eq, Read, Show)

instance HasSpanInfo ImportDecl where
  getSpanInfo (ImportDecl spi _ _ _ _) = spi

instance HasSpanInfo (Decl a) where
  getSpanInfo (InfixDecl        spi _ _ _  ) = spi
  getSpanInfo (DataDecl         spi _ _ _ _) = spi
  getSpanInfo (ExternalDataDecl spi _ _    ) = spi
  getSpanInfo (NewtypeDecl      spi _ _ _ _) = spi
  getSpanInfo (TypeDecl         spi _ _ _  ) = spi
  getSpanInfo (TypeSig          spi _ _    ) = spi
  getSpanInfo (FunctionDecl     spi _ _ _  ) = spi
  getSpanInfo (ExternalDecl     spi _      ) = spi
  getSpanInfo (DefaultDecl      spi _      ) = spi
  getSpanInfo (ClassDecl        spi _ _ _ _) = spi
  getSpanInfo (InstanceDecl     spi _ _ _ _) = spi

instance HasSpanInfo NewConstrDecl where
  getSpanInfo (NewConstrDecl spi _ _) = spi
  getSpanInfo (NewRecordDecl spi _ _) = spi

instance HasSpanInfo ConstrDecl where
  getSpanInfo (ConstrDecl spi _ _ _ _  ) = spi
  getSpanInfo (ConOpDecl  spi _ _ _ _ _) = spi
  getSpanInfo (RecordDecl spi _ _ _ _  ) = spi

instance HasSpanInfo FieldDecl where
  getSpanInfo (FieldDecl spi _ _) = spi

instance HasSpanInfo TypeExpr where
  getSpanInfo (ArrowType spi _ _) = spi
  getSpanInfo (ApplyType spi _ _) = spi
  getSpanInfo (ConstructorType spi _) = spi
  getSpanInfo (VariableType spi _) = spi
  getSpanInfo (TupleType spi _) = spi
  getSpanInfo (ListType spi _) = spi
  getSpanInfo (ParenType spi _) = spi

instance HasSpanInfo Export where
  getSpanInfo (ExportTypeAll spi _) = spi
  getSpanInfo (ExportTypeWith spi _ _) = spi
  getSpanInfo (ExportModule spi _) = spi
  getSpanInfo (Export spi _) = spi
