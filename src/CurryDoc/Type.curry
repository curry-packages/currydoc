-- | An implementation of the curry AST from curry-frontend without some types
-- that cannot be in shortAST files
module CurryDoc.Type where

import CurryDoc.SpanInfo
import CurryDoc.Ident
import CurryDoc.Position

import Distribution

data Module a = Module SpanInfo [ModulePragma] ModuleIdent (Maybe ExportSpec)
                       [ImportDecl] [Decl a]
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

-- | Context
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

-- | KnownExtension
data KnownExtension
  = AnonFreeVars
  |   {- | CPP-} CPP
  | ExistentialQuantification
  | FunctionalPatterns
  | NegativeLiterals
  | NoImplicitPrelude
  deriving (Eq, Read, Show)

data Test = Test { a, b :: KnownExtension, -- ^ a and b
                   {- | c  -} c :: KnownExtension} -- ^ constr

newtype Test2 = Test2 { {- | startNew -}lmao :: Extension -- ^ endNew
                      }

data Tool = KICS2 | PAKCS | CYMAKE | FRONTEND | UnknownTool String
  deriving (Eq, Read, Show)

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

-- | HasSpanInfo TypeExpr
instance HasSpanInfo TypeExpr where
  -- | First
  getSpanInfo (ArrowType spi _ _) = spi
  getSpanInfo (ApplyType spi _ _) = spi
  getSpanInfo (ConstructorType spi _) = spi
  getSpanInfo (VariableType spi _) = spi
  getSpanInfo (TupleType spi _) = spi
  getSpanInfo (ListType spi _) = spi
  getSpanInfo (ParenType spi _) = spi
  -- ^ Last

readASTFile :: String -> IO (Module ())
readASTFile s = readFile s >>= (return . read)

readShortAST :: String -> IO (Module ())
readShortAST modl = do ss <- readFile (".curry/" ++ modNameToPath modl ++ ".sast")
                       return (read ss :: Module ())
