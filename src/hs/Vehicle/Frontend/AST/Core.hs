{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Vehicle.Frontend.AST.Core where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List.NonEmpty (NonEmpty)

import Vehicle.Prelude

-- | Variable binding sites.
data Binder ann
  = Binder
    Provenance         -- Location in the source file (includes visibility brackets)
    Visibility         -- Variable visibility
    Symbol             -- Variable name
    (Maybe (Expr ann)) -- Variable typing annotation (optional)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance HasVisibility (Binder ann) where
  vis (Binder _ v _ _) = v

-- | Arguments to function applications
data Arg ann
  = Arg
    Provenance         -- Location in the source file (includes visibility brackets)
    Visibility         -- Argument visibility
    (Expr ann)         -- Argument expression
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance HasVisibility (Arg ann) where
  vis (Arg _ v _) = v

-- | An individual let declaration
data LetDecl ann
  = LetDecl
    Provenance         -- Location in the source file
    (Binder ann)       -- Variable name
    (Expr   ann)       -- Bound expression
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | The core Tree structure, parameterised by an annotation type so different
-- types of data can be associated with it's parts.
data Expr ann
  -- Core
  = Forall   ann (NonEmpty (Binder ann)) (Expr ann)
  | Fun      ann (Expr ann) (Expr ann)
  | Ann      ann (Expr ann) (Expr ann)
  | App      ann (Expr ann) (Arg ann)
  | Lam      ann (NonEmpty (Binder ann)) (Expr ann)
  | Let      ann (NonEmpty (LetDecl ann)) (Expr ann)
  | Var      ann Symbol
  | Literal  ann Literal
  | Hole     Provenance Symbol
  | PrimDict (Expr ann)

  -- Kinds
  | Type Level

  -- Types
  | Bool     ann
  | Prop     ann
  | Real     ann
  | Int      ann
  | Nat      ann
  | List     ann (Expr ann)
  | Tensor   ann (Expr ann) (Expr ann)

  -- Type classes
  | HasEq       ann (Expr ann) (Expr ann)
  | HasOrd      ann (Expr ann) (Expr ann)
  | IsContainer ann (Expr ann) (Expr ann)
  | IsTruth     ann (Expr ann)
  | IsQuant     ann (Expr ann)
  | IsNatural   ann (Expr ann)
  | IsIntegral  ann (Expr ann)
  | IsRational  ann (Expr ann)
  | IsReal      ann (Expr ann)

  -- Terms
  | If       ann (Expr ann) (Expr ann) (Expr ann)
  | Impl     ann (Expr ann) (Expr ann)
  | And      ann (Expr ann) (Expr ann)
  | Or       ann (Expr ann) (Expr ann)
  | Not      ann (Expr ann)
  | Eq       ann (Expr ann) (Expr ann)
  | Neq      ann (Expr ann) (Expr ann)
  | Le       ann (Expr ann) (Expr ann)
  | Lt       ann (Expr ann) (Expr ann)
  | Ge       ann (Expr ann) (Expr ann)
  | Gt       ann (Expr ann) (Expr ann)
  | Mul      ann (Expr ann) (Expr ann)
  | Div      ann (Expr ann) (Expr ann)
  | Add      ann (Expr ann) (Expr ann)
  | Sub      ann (Expr ann) (Expr ann)
  | Neg      ann (Expr ann)
  | Cons     ann (Expr ann) (Expr ann)
  | At       ann (Expr ann) (Expr ann)
  | Quant    ann Quantifier (Binder ann) (Expr ann)
  | QuantIn  ann Quantifier (Binder ann) (Expr ann) (Expr ann)
  | Seq      ann [Expr ann]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Type of Vehicle Frontend declaration.
data Decl ann
  = DeclNetw
    Provenance
    (WithProvenance Identifier) -- Name of the declared network.
    (Expr  ann)                 -- Type of the declared network.

  | DeclData
    Provenance
    (WithProvenance Identifier) -- Name of the declared dataset.
    (Expr  ann)                 -- Type of the declared dataset.

  | DefType
    Provenance
    (WithProvenance Identifier) -- Name of the type declaration.
    [Binder ann]                -- Variables of the type declaration.
    (Expr   ann)                -- Body of the type declaration.

  | DefFun
    Provenance
    (WithProvenance Identifier) -- Name of the function declaration.
    (Expr   ann)                -- Type of the function declaration.
    [Binder ann]                -- Variables of the function declaration.
    (Expr   ann)                -- Body of the function declaration.

  deriving (Eq, Ord, Show)

-- | Type of Vehicle programs
newtype Prog ann
  = Main [Decl ann] -- Sequence of declarations

  deriving (Eq, Ord, Show)

-- | An annotation that stores both the type of the expression and some other
-- arbitrary annotations. Used to avoid unrestricted type-level recursion.
data RecAnn ann = RecAnn (Expr (RecAnn ann)) ann

makeBaseFunctor ''Expr