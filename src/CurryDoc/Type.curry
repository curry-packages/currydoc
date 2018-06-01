-- | An implementation of the curry AST from curry-frontend without some types
-- that cannot be in shortAST files
module CurryDoc.Type where

import CurryDoc.SpanInfo
import CurryDoc.Ident
import CurryDoc.Position

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

getConstrName   :: ConstrDecl -- ^ ConstrDecl
  {- | Ident -} -> Ident
getConstrName (ConstrDecl _ _ _   idt _) = idt
getConstrName (ConOpDecl  _ _ _ _ idt _) = idt
getConstrName (RecordDecl _ _ _   idt _) = idt

{-
instance Functor Module where
  fmap f (Module spi ps mid ex im ds) =
    Module spi ps mid ex im (map (fmap f) ds)

instance Functor Decl where
  fmap f (FunctionDecl spi a idt eqs) =
    FunctionDecl spi (f a) idt (map (fmap f) eqs)
  fmap f (ExternalDecl spi vs) = ExternalDecl spi (map (fmap f) vs)
  fmap f (ClassDecl spi cx cls tyv ds) =
    ClassDecl spi cx cls tyv (map (fmap f) ds)
  fmap f (InstanceDecl spi cx cls ty ds) =
    InstanceDecl spi cx cls ty (map (fmap f) ds)
  fmap f d = d

instance Functor Lhs where
  fmap f (FunLhs spi f ps) = FunLhs spi f (map (fmap f) ps)
  fmap f (OpLhs spi p1 f p2) = OpLhs spi (fmap f p1) f (fmap f p2)
  fmap f (ApLhs spi lhs ps) = ApLhs spi (fmap f lhs) (map (fmap f) ps)

instance Functor Rhs where
  fmap f (SimpleRhs spi e ds) = SimpleRhs spi (fmap f e) (map (fmap f) ds)
  fmap f (GuardedRhs spi cs ds) =
    GuardedRhs spi (map (fmap f) cs) (map (fmap f) ds)

instance Functor CondExpr where
  fmap f (CondExpr spi e1 e2) = CondExpr spi (fmap f e1) (fmap f e2)

instance Functor Expression where
  fmap f (Variable spi a qid) = Variable spi (f a) qid

instance Functor Field where
  fmap f (Field spi qid a) = Field spi qid (f a)

instance Functor Var where
  fmap f (Var a idt) = Var (f a) idt

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
-}
