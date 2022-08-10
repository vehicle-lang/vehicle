module Vehicle.Language.Sugar where

import Data.Bifunctor (first)

import Vehicle.Language.AST

--------------------------------------------------------------------------------
-- General definitions

-- This module deals with all the unfolding and folding of syntactic
-- sugar in the external language. The unfolding is designed so that it should
-- be 100% reversible.

type BindersAndBody     binder var = ([Binder binder var], Expr binder var)
type LetBinder          binder var = (Binder binder var, Expr binder var)
type LetBindingsAndBody binder var = ([LetBinder binder var], Expr binder var)

unfoldBinders :: ann
              -> (ann -> Binder binder var -> Expr binder var -> Expr binder var)
              -> BindersAndBody binder var
              -> Expr binder var
unfoldBinders ann fn (binders, body) = foldr (fn ann) body binders

--------------------------------------------------------------------------------
-- Pi/Fun/Forall declarations

-- | Unfolads a list of binders into a consecutative forall expressions
unfoldForall :: Provenance -> BindersAndBody binder var -> Expr binder var
unfoldForall ann = unfoldBinders ann Pi

unfoldFun :: Provenance -> Expr DBBinding var -> Expr DBBinding var -> Expr DBBinding var
unfoldFun ann dom = Pi ann (ExplicitBinder ann Nothing dom)

-- | Collapses pi expressions into either a sequence of forall bindings or a
-- a function input/output type pair.
foldPi :: Provenance -> Binder binder var -> Expr binder var ->
          Either (BindersAndBody binder var) (Expr binder var, Expr binder var)
foldPi ann binder result = if isFunBinder binder
  then Right (typeOf binder, result)
  else Left  (foldForall (Pi ann binder result))

-- | Folds consecutative forall expressions into a list of binders
foldForall :: Expr binder var -> BindersAndBody binder var
foldForall expr = first reverse (decomposeForall ([], expr))
  where
    decomposeForall :: BindersAndBody binder var -> BindersAndBody binder var
    decomposeForall (bs, Pi _ b body) = decomposeForall (b : bs, body)
    decomposeForall result            = result

-- | Tests if a binder is from a forall or a function.
isFunBinder :: Binder binder var -> Bool
isFunBinder binder = visibilityOf binder == Explicit

--------------------------------------------------------------------------------
-- Lambdas

unfoldLam :: Provenance -> BindersAndBody binder var -> Expr binder var
unfoldLam ann = unfoldBinders ann Lam

-- | Collapses consecutative lambda expressions into a list of binders
foldLam :: Expr binder var -> BindersAndBody binder var
foldLam expr = first reverse (decomposeLam ([], expr))
  where
    decomposeLam :: BindersAndBody binder var -> BindersAndBody binder var
    decomposeLam (bs, Lam _ b body) = decomposeLam (b : bs, body)
    decomposeLam result             = result

--------------------------------------------------------------------------------
-- Quantifiers

unfoldQuantifier :: Provenance
                 -> Builtin
                 -> BindersAndBody binder var
                 -> Expr binder var
unfoldQuantifier ann q = unfoldBinders ann (\ann1 binder body ->
  normAppList ann1 (Builtin ann1 q)
    [ExplicitArg ann1 (Lam ann1 binder body)])

unfoldQuantifierIn :: Provenance
                   -> Quantifier
                   -> Expr binder var
                   -> BindersAndBody binder var
                   -> Expr binder var
unfoldQuantifierIn ann q container = unfoldBinders ann (\ann1 binder body ->
  normAppList ann1 (Builtin ann1 (TypeClassOp $ QuantifierInTC q))
    [ ExplicitArg ann1 (Lam ann1 binder body)
    , ExplicitArg ann1 container
    ])

--------------------------------------------------------------------------------
-- Let declarations

unfoldLet :: forall binder var. Provenance -> LetBindingsAndBody binder var -> Expr binder var
unfoldLet ann (binders, body) = foldr insertLet body binders
  where
    insertLet :: LetBinder binder var -> Expr binder var -> Expr binder var
    insertLet (binder, bound) = Let ann bound binder

-- | Collapses consecutative let expressions into a list of let declarations
foldLet :: Expr binder var -> LetBindingsAndBody binder var
foldLet expr = first reverse (decomposeLet ([], expr))
  where
    decomposeLet :: LetBindingsAndBody binder var -> LetBindingsAndBody binder var
    decomposeLet (bs, Let _ bound binder body) = decomposeLet ((binder, bound) : bs, body)
    decomposeLet result                        = result

--------------------------------------------------------------------------------
-- Function and type declarations

unfoldDefFun :: Provenance
             -> Identifier
             -> Expr binder var
             -> [Binder binder var]
             -> Expr binder var
             -> Decl binder var
unfoldDefFun p ident t bs e =
  DefFunction p ident t (unfoldLam p (bs, e))

unfoldDefType :: Provenance
              -> Identifier
              -> [Binder binder var]
              -> Expr binder var
              -> Decl binder var
unfoldDefType p ident bs e =
  let t = foldr (Pi p) (Universe p (TypeUniv 0)) bs in
  unfoldDefFun p ident t bs e

foldDefFun :: Expr binder var -> Expr binder var ->
              Either
                (Expr binder var, ([Binder binder var], Expr binder var))
                ([Binder binder var], Expr binder var)
foldDefFun t e = if isTypeSynonym t
  then Right (foldLam e)
  else Left  (t, foldLam e)
  where
    isTypeSynonym :: Expr binder var -> Bool
    isTypeSynonym (Universe _ TypeUniv{}) = True
    isTypeSynonym (Pi _ _ res)   = isTypeSynonym res
    isTypeSynonym _              = False
