{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Vehicle.Core.Type where

-- * Abstract syntax tree for Vehicle Core

-- | Syntactic sorts used in Vehicle Core syntax.
data Sort = KIND | TYPE | EXPR | DECL | PROG | TARG | EARG

-- | Kind of annotations used in Vehicle Core syntax.
type Ann = Sort -> * -> * -> *

-- | Kind of symbols used in Vehicle Core syntax, i.e., variable and builtin names.
type Sym = Sort -> *

-- | Type of Vehicle Core kinds.
data Kind (name :: Sym) (builtin :: Sym) (ann :: Ann)
  = KApp
    (ann 'KIND (name 'KIND) (builtin 'KIND)) -- ^ Annotation.
    (Kind name builtin ann)                  -- ^ Function.
    (Kind name builtin ann)                  -- ^ Argument.
  | KCon
    (ann 'KIND (name 'KIND) (builtin 'KIND)) -- ^ Annotation.
    (builtin 'KIND)                          -- ^ Builtin name.
  | KMeta
    (ann 'KIND (name 'KIND) (builtin 'KIND)) -- ^ Annotation.
    Integer                                  -- ^ Meta variable.

-- | Type of Vehicle Core types.
data Type (name :: Sym) (builtin :: Sym) (ann :: Ann)
  = TForall
    (ann 'TYPE (name 'TYPE) (builtin 'TYPE)) -- ^ Annotation.
    (TArg name builtin ann)                  -- ^ Bound type name.
    (Type name builtin ann)                  -- ^ Type body.
  | TApp
    (ann 'TYPE (name 'TYPE) (builtin 'TYPE)) -- ^ Annotation.
    (Type name builtin ann)                  -- ^ Function.
    (Type name builtin ann)                  -- ^ Argument.
  | TVar
    (ann 'TYPE (name 'TYPE) (builtin 'TYPE)) -- ^ Annotation.
    (name 'TYPE)                             -- ^ Variable name.
  | TCon
    (ann 'TYPE (name 'TYPE) (builtin 'TYPE)) -- ^ Annotation.
    (builtin 'TYPE)                          -- ^ Builtin name.
  | TLitDim
    (ann 'TYPE (name 'TYPE) (builtin 'TYPE)) -- ^ Annotation.
    Integer                                  -- ^ Dimension literal.
  | TLitList
    (ann 'TYPE (name 'TYPE) (builtin 'TYPE)) -- ^ Annotation.
    [Type name builtin ann]                  -- ^ List of types.
  | TMeta
    (ann 'TYPE (name 'TYPE) (builtin 'TYPE)) -- ^ Annotation.
    Integer                                  -- ^ Meta variable

-- | Type of Vehicle Core expressions.
data Expr (name :: Sym) (builtin :: Sym) (ann :: Ann)
  = EAnn
    (ann 'EXPR (name 'EXPR) (builtin 'EXPR)) -- ^ Annotation.
    (Expr name builtin ann)                  -- ^ Expression.
    (Type name builtin ann)                  -- ^ Expression type.
  | ELet
    (ann 'EXPR (name 'EXPR) (builtin 'EXPR)) -- ^ Annotation.
    (EArg name builtin ann)                  -- ^ Bound expression name.
    (Expr name builtin ann)                  -- ^ Bound expression body.
    (Expr name builtin ann)                  -- ^ Expression body.
  | ELam
    (ann 'EXPR (name 'EXPR) (builtin 'EXPR)) -- ^ Annotation.
    (EArg name builtin ann)                  -- ^ Bound expression name.
    (Expr name builtin ann)                  -- ^ Expression body.
  | EApp
    (ann 'EXPR (name 'EXPR) (builtin 'EXPR)) -- ^ Annotation.
    (Expr name builtin ann)                  -- ^ Function.
    (Expr name builtin ann)                  -- ^ Argument.
  | EVar
    (ann 'EXPR (name 'EXPR) (builtin 'EXPR)) -- ^ Annotation.
    (name 'EXPR)                             -- ^ Variable name.
  | ETyApp
    (ann 'EXPR (name 'EXPR) (builtin 'EXPR)) -- ^ Annotation.
    (Expr name builtin ann)                  -- ^ Type function.
    (Type name builtin ann)                  -- ^ Type argument.
  | ETyLam
    (ann 'EXPR (name 'EXPR) (builtin 'EXPR)) -- ^ Annotation.
    (TArg name builtin ann)                  -- ^ Bound type name.
    (Expr name builtin ann)                  -- ^ Expression body.
  | ECon
    (ann 'EXPR (name 'EXPR) (builtin 'EXPR)) -- ^ Annotation.
    (builtin 'EXPR)                          -- ^ Builtin name.
  | ELitInt
    (ann 'EXPR (name 'EXPR) (builtin 'EXPR)) -- ^ Annotation.
    Integer                                  -- ^ Integer literal.
  | ELitReal
    (ann 'EXPR (name 'EXPR) (builtin 'EXPR)) -- ^ Annotation.
    Double                                   -- ^ "Real" literal.
  | ELitSeq
    (ann 'EXPR (name 'EXPR) (builtin 'EXPR)) -- ^ Annotation.
    [Expr name builtin ann]                  -- ^ List of expressions.

-- | Type of Vehicle Core declaration.
data Decl (name :: Sym) (builtin :: Sym) (ann :: Ann)
  = DeclNetw
    (ann 'DECL (name 'DECL) (builtin 'DECL)) -- ^ Annotation.
    (EArg name builtin ann)                  -- ^ Network name.
    (Type name builtin ann)                  -- ^ Network type.
  | DeclData
    (ann 'DECL (name 'DECL) (builtin 'DECL)) -- ^ Annotation.
    (EArg name builtin ann)                  -- ^ Dataset name.
    (Type name builtin ann)                  -- ^ Dataset type.
  | DefType
    (ann 'DECL (name 'DECL) (builtin 'DECL)) -- ^ Annotation.
    (TArg name builtin ann)                  -- ^ Bound type synonym name.
    [TArg name builtin ann]                  -- ^ Bound type synonym arguments.
    (Type name builtin ann)                  -- ^ Bound type synonym body.
  | DefFun
    (ann 'DECL (name 'DECL) (builtin 'DECL)) -- ^ Annotation.
    (EArg name builtin ann)                  -- ^ Bound function name.
    (Type name builtin ann)                  -- ^ Bound function type.
    (Expr name builtin ann)                  -- ^ Bound function body.

-- | Type of Vehicle Core programs.
data Prog (name :: Sym) (builtin :: Sym) (ann :: Ann)
  = Main
    (ann 'PROG (name 'PROG) (builtin 'PROG)) -- ^ Annotation.
    [Decl name builtin ann]                  -- ^ List of declarations.

-- | Type of Vehicle Core type-level name-binding sites.
data TArg (name :: Sym) (builtin :: Sym) (ann :: Ann)
  = TArg
  (ann 'TARG (name 'TARG) (builtin 'TARG))   -- ^ Annotation.
  (name 'TARG)                               -- ^ Type name.

-- | Type of Vehicle Core expression-level name-binding sites.
data EArg (name :: Sym) (builtin :: Sym) (ann :: Ann)
  = EArg
  (ann 'EARG (name 'EARG) (builtin 'EARG))   -- ^ Annotation.
  (name 'EARG)                               -- ^ Expression name.
