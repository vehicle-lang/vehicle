module Vehicle.Language.Sugar where

import Data.Bifunctor (first)

import Vehicle.Language.AST
import Vehicle.Language.Provenance

--------------------------------------------------------------------------------
-- General definitions

-- This module deals with all the unfolding and folding of syntactic
-- sugar in the external language. The unfolding is designed so that it should
-- be 100% reversible.

type BindersAndBody     binder var ann = ([Binder binder var ann], Expr binder var ann)
type LetBinder          binder var ann = (Binder binder var ann, Expr binder var ann)
type LetBindingsAndBody binder var ann = ([LetBinder binder var ann], Expr binder var ann)

unfoldBinders :: ann
              -> (ann -> Binder binder var ann -> Expr binder var ann -> Expr binder var ann)
              -> BindersAndBody binder var ann
              -> Expr binder var ann
unfoldBinders ann fn (binders, body) = foldr (fn ann) body binders

--------------------------------------------------------------------------------
-- Pi/Fun/Forall declarations

-- | Unfolads a list of binders into a consecutative forall expressions
unfoldForall :: ann -> BindersAndBody binder var ann -> Expr binder var ann
unfoldForall ann = unfoldBinders ann Pi

unfoldFun :: ann -> Expr DBBinding var ann -> Expr DBBinding var ann -> Expr DBBinding var ann
unfoldFun ann dom = Pi ann (ExplicitBinder ann Nothing dom)

-- | Collapses pi expressions into either a sequence of forall bindings or a
-- a function input/output type pair.
foldPi :: ann -> Binder binder var ann -> Expr binder var ann ->
          Either (BindersAndBody binder var ann) (Expr binder var ann, Expr binder var ann)
foldPi ann binder result = if isFunBinder binder
  then Right (typeOf binder, result)
  else Left  (foldForall (Pi ann binder result))

-- | Folds consecutative forall expressions into a list of binders
foldForall :: Expr binder var ann -> BindersAndBody binder var ann
foldForall expr = first reverse (decomposeForall ([], expr))
  where
    decomposeForall :: BindersAndBody binder var ann -> BindersAndBody binder var ann
    decomposeForall (bs, Pi _ b body) = decomposeForall (b : bs, body)
    decomposeForall result            = result

-- | Tests if a binder is from a forall or a function.
isFunBinder :: Binder binder var ann -> Bool
isFunBinder binder = visibilityOf binder == Explicit

--------------------------------------------------------------------------------
-- Lambdas

unfoldLam :: ann -> BindersAndBody binder var ann -> Expr binder var ann
unfoldLam ann = unfoldBinders ann Lam

-- | Collapses consecutative lambda expressions into a list of binders
foldLam :: Expr binder var ann -> BindersAndBody binder var ann
foldLam expr = first reverse (decomposeLam ([], expr))
  where
    decomposeLam :: BindersAndBody binder var ann -> BindersAndBody binder var ann
    decomposeLam (bs, Lam _ b body) = decomposeLam (b : bs, body)
    decomposeLam result             = result

--------------------------------------------------------------------------------
-- Quantifiers

unfoldQuantifier :: forall binder var ann. Semigroup ann
                 => ann
                 -> Builtin
                 -> BindersAndBody binder var ann
                 -> Expr binder var ann
unfoldQuantifier ann q = unfoldBinders ann (\ann1 binder body ->
  normAppList ann1 (Builtin ann1 q)
    [ExplicitArg ann1 (Lam ann1 binder body)])

unfoldQuantifierIn :: forall binder var ann. Semigroup ann
                   => ann
                   -> Builtin
                   -> Expr binder var ann
                   -> BindersAndBody binder var ann
                   -> Expr binder var ann
unfoldQuantifierIn ann q container = unfoldBinders ann (\ann1 binder body ->
  normAppList ann1 (Builtin ann1 q)
    [ ExplicitArg ann1 (Lam ann1 binder body)
    , ExplicitArg ann1 container
    ])

--------------------------------------------------------------------------------
-- Let declarations

unfoldLet :: forall binder var ann. ann -> LetBindingsAndBody binder var ann -> Expr binder var ann
unfoldLet ann (binders, body) = foldr insertLet body binders
  where
    insertLet :: LetBinder binder var ann -> Expr binder var ann -> Expr binder var ann
    insertLet (binder, bound) = Let ann bound binder

-- | Collapses consecutative let expressions into a list of let declarations
foldLet :: Expr binder var ann -> LetBindingsAndBody binder var ann
foldLet expr = first reverse (decomposeLet ([], expr))
  where
    decomposeLet :: LetBindingsAndBody binder var ann -> LetBindingsAndBody binder var ann
    decomposeLet (bs, Let _ bound binder body) = decomposeLet ((binder, bound) : bs, body)
    decomposeLet result                        = result

--------------------------------------------------------------------------------
-- Function and type declarations

unfoldDefFun :: HasProvenance ann
             => ann -> Identifier
             -> Expr binder var ann
             -> [Binder binder var ann]
             -> Expr binder var ann
             -> Decl binder var ann
unfoldDefFun ann ident t bs e =
  DefFunction ann Nothing ident t (unfoldLam ann (bs, e))

unfoldDefType :: HasProvenance ann
              => ann -> Identifier
              -> [Binder binder var ann]
              -> Expr binder var ann
              -> Decl binder var ann
unfoldDefType ann ident bs e =
  let t = foldr (Pi ann) (Type ann 0) bs in
  unfoldDefFun ann ident t bs e

foldDefFun :: Expr binder var ann -> Expr binder var ann ->
              Either
                (Expr binder var ann, ([Binder binder var ann], Expr binder var ann))
                ([Binder binder var ann], Expr binder var ann)
foldDefFun t e = if isTypeSynonym t
  then Right (foldLam e)
  else Left  (t, foldLam e)
  where
    isTypeSynonym :: Expr binder var ann -> Bool
    isTypeSynonym (Type _ _)   = True
    isTypeSynonym (Pi _ _ res) = isTypeSynonym res
    isTypeSynonym _            = False
