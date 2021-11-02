module Vehicle.Language.Sugar where

import Data.Bifunctor (first)

import Vehicle.Prelude
import Vehicle.Language.AST

--------------------------------------------------------------------------------
-- General definitions

-- This module deals with all the unfolding and folding of syntactic
-- sugar in the frontend language. The unfolding is designed so that it should
-- be 100% reversible.

type BindersAndBody     name ann = ([Binder name ann], Expr name ann)
type LetBinding         name ann = (Binder name ann, Expr name ann)
type LetBindingsAndBody name ann = ([LetBinding name ann], Expr name ann)

unfoldBinders :: ann
              -> (ann -> Binder name ann -> Expr name ann -> Expr name ann)
              -> BindersAndBody name ann
              -> Expr name ann
unfoldBinders ann fn (binders, body) = foldr (fn ann) body binders

--------------------------------------------------------------------------------
-- Pi/Fun/Forall declarations

-- | Unfolads a list of binders into a consecutative forall expressions
unfoldForall :: ann -> BindersAndBody name ann -> Expr name ann
unfoldForall ann = unfoldBinders ann Pi

unfoldFun :: HasProvenance ann => ann -> Expr name ann -> Expr name ann -> Expr name ann
unfoldFun ann dom = Pi ann (ExplicitBinder (prov ann) Machine dom)

-- | Collapses pi expressions into either a sequence of forall bindings or a
-- a function input/output type pair.
foldPi :: ann -> Binder name ann -> Expr name ann ->
          Either (BindersAndBody name ann) (Expr name ann, Expr name ann)
foldPi ann binder result = if isFunBinder binder
  then Left  (foldForall (Pi ann binder result))
  else Right (binderType binder, result)

-- | Folds consecutative forall expressions into a list of binders
foldForall :: Expr name ann -> BindersAndBody name ann
foldForall expr = first reverse (decomposeForall ([], expr))
  where
    decomposeForall :: BindersAndBody name ann -> BindersAndBody name ann
    decomposeForall (bs, Pi _ b body) = decomposeForall (b : bs, body)
    decomposeForall result            = result

-- | Tests if a binder is from a forall or a function.
isFunBinder :: Binder name ann -> Bool
isFunBinder binder = vis binder == Explicit

--------------------------------------------------------------------------------
-- Lambdas

unfoldLam :: ann -> BindersAndBody name ann -> Expr name ann
unfoldLam ann = unfoldBinders ann Lam

-- | Collapses consecutative lambda expressions into a list of binders
foldLam :: Expr name ann -> BindersAndBody name ann
foldLam expr = first reverse (decomposeLam ([], expr))
  where
    decomposeLam :: BindersAndBody name ann -> BindersAndBody name ann
    decomposeLam (bs, Lam _ b body) = decomposeLam (b : bs, body)
    decomposeLam result             = result

--------------------------------------------------------------------------------
-- Quantifiers

unfoldQuantifier :: forall name ann. Semigroup ann
                 => ann
                 -> Quantifier
                 -> BindersAndBody name ann
                 -> Expr name ann
unfoldQuantifier ann q = unfoldBinders ann (\ann1 binder body ->
  normAppList ann1 (Builtin ann1 (Quant q))
    [ExplicitArg (Lam ann1 binder body)])

unfoldQuantifierIn :: forall name ann. Semigroup ann
                   => ann
                   -> Quantifier
                   -> Expr name ann
                   -> BindersAndBody name ann
                   -> Expr name ann
unfoldQuantifierIn ann q container = unfoldBinders ann (\ann1 binder body ->
  normAppList ann1 (Builtin ann1 (QuantIn q))
    [ ExplicitArg (Lam ann1 binder body)
    , ExplicitArg container
    ])

--------------------------------------------------------------------------------
-- Let declarations

unfoldLet :: forall name ann. ann -> LetBindingsAndBody name ann -> Expr name ann
unfoldLet ann (binders, body) = foldr insertLet body binders
  where
    insertLet :: LetBinding name ann -> Expr name ann -> Expr name ann
    insertLet (binder, bound) res = Let ann bound binder res

-- | Collapses consecutative let expressions into a list of let declarations
foldLet :: Expr name ann -> LetBindingsAndBody name ann
foldLet expr = first reverse (decomposeLet ([], expr))
  where
    decomposeLet :: LetBindingsAndBody name ann -> LetBindingsAndBody name ann
    decomposeLet (bs, Let _ bound binder body) = decomposeLet ((binder, bound) : bs, body)
    decomposeLet result                        = result

--------------------------------------------------------------------------------
-- Function and type declarations

unfoldDefFun :: HasProvenance ann
             => ann -> Identifier
             -> Expr name ann
             -> [Binder name ann]
             -> Expr name ann
             -> Decl name ann
unfoldDefFun ann ident t bs e = DefFun (prov ann) ident t (unfoldLam ann (bs, e))

unfoldDefType :: HasProvenance ann
              => ann -> Identifier
              -> [Binder name ann]
              -> Expr name ann
              -> Decl name ann
unfoldDefType ann ident bs e =
  let t = foldr (Pi ann) Type0 bs in
  unfoldDefFun ann ident t bs e

foldDefFun :: Expr name ann -> Expr name ann -> Either
                (Expr name ann, ([Binder name ann], Expr name ann))
                ([Binder name ann], Expr name ann)
foldDefFun t e = if isTypeSynonym t
  then Right (foldLam e)
  else Left  (t, foldLam e)
  where
    isTypeSynonym :: Expr name ann -> Bool
    isTypeSynonym Type0        = True
    isTypeSynonym (Pi _ _ res) = isTypeSynonym res
    isTypeSynonym _            = False