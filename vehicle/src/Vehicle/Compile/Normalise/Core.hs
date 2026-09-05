module Vehicle.Compile.Normalise.Core where

import Data.Void (absurd)
import Vehicle.Data.AST.Expr.Scoped
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Tensor
import Vehicle.Data.Variable.Bound.Context.Name.Class
import Vehicle.Data.Variable.Free.Context.Class
import Vehicle.Prelude
import Vehicle.Prelude.Logging.Class

class MetaLike meta where
  toMetaID :: meta -> MetaID
  fromMetaID :: MetaID -> meta

instance MetaLike NoMeta where
  toMetaID = absurd
  fromMetaID = unexpectedExprError "normalisation" "meta"

instance MetaLike MetaID where
  toMetaID = id
  fromMetaID = id

type BlockingArgs expr builtin = [expr builtin]

data BuiltinEvaluationResult expr thunk builtin
  = -- The builtin was evaluated and was reduced to a simpler form.
    Evaluated (thunk builtin)
  | -- The builtin could not be evaluated.
    Unevaluable (BlockingArgs expr builtin)

type EvalBuiltinFn meta builtin args m =
  ( MonadLogger m,
    MonadReadableNameContext m,
    NormalisableExpr (GenericForcedValue meta) (GenericThunk meta) builtin m,
    HasBuiltinConstructor (GenericForcedValue meta) (GenericThunk meta),
    HasLambdaConstructor (GenericForcedValue meta) (GenericThunk meta) (GenericClosure meta)
  ) =>
  args (GenericThunk meta builtin) ->
  m (BuiltinEvaluationResult (GenericForcedValue meta) (GenericThunk meta) builtin)

data EvalScheme meta builtin m
  = forall args. (IsArgs args) => Eval (EvalBuiltinFn meta builtin args m)
  | Derived Identifier
  | TypeClassOperation
  | None

class (Monad m, HasBuiltinConstructor expr thunk, Show (expr builtin)) => NormalisableExpr expr thunk builtin m | thunk -> expr where
  force :: thunk builtin -> m (expr builtin)
  forceApp :: thunk builtin -> [GenericArg (thunk builtin)] -> m (expr builtin)

instance (Monad m, Show builtin) => NormalisableExpr Expr Expr builtin m where
  force = return
  forceApp fun args = return $ normAppList fun args

type TensorOpEvalData expr thunk args builtin =
  ( Accessor (expr builtin) (args (thunk builtin)),
    expr builtin -- The element type
  )

type TensorComparisonOpEvalData expr thunk builtin =
  ( RatTensorComparisonAccessor expr thunk builtin,
    expr builtin -- The element type
  )

class HasLiftableTensorOperations expr thunk builtin where
  liftableTensorOp1s :: [TensorOpEvalData expr thunk TensorOp1Args builtin]
  liftableTensorOp2s :: [TensorOpEvalData expr thunk TensorOp2Args builtin]
  liftableTensorComparisons :: [TensorComparisonOpEvalData expr thunk builtin]

data TensorLiteralAccessor expr builtin
  = forall a. (Eq a) => Wrapper (Accessor (expr builtin) (Tensor a))

class HasTensorLiterals expr builtin where
  tensorLiterals :: [TensorLiteralAccessor expr builtin]

-- | A type-class for builtins that can be normalised compositionally.
class (PrintableBuiltin builtin) => NormalisableBuiltin builtin where
  evalScheme :: builtin -> EvalScheme meta builtin m
  isCast :: (MonadLogger m) => Provenance -> builtin -> Maybe ([GenericArg (Expr builtin)] -> m (Expr builtin))
  isTypeClassOp :: builtin -> Bool

type MonadNorm builtin m =
  ( MonadLogger m,
    NormalisableBuiltin builtin,
    MonadFreeContext builtin m,
    MonadReadableNameContext m
  )
