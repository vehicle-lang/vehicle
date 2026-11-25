module Vehicle.Compile.Simplify
  ( Simplify (..),
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Vehicle.Data.AST.Arg
import Vehicle.Data.AST.Binder (GenericBinder (..))
import Vehicle.Data.AST.Expr.Desugared
import Vehicle.Data.AST.Provenance (Provenance)
import Vehicle.Data.AST.Record (FieldName (..), mapRecordFields)
import Vehicle.Data.AST.Relevance (Relevance (..), setRelevance)
import Vehicle.Data.AST.Visibility (Visibility (..), isExplicit, isInstance, wasInsertedByCompiler)
import Vehicle.Data.Builtin.Core.StdlibTypeClassOp (lookupStdlibTypeClassOp)
import Vehicle.Data.Builtin.Standard.Core

-- | Operations that make code easier to read when displayed to the user.
--
-- Note that these operations can be seen as undoing parts of the type-checking,
-- and therefore the resulting code is not guaranteed to be well-typed.
class Simplify a where
  -- | Removes the parts of the code that were inserted by the compiler rather
  -- than written by the user (e.g. implicit arguments, casts and internal
  -- constraints), and converts internal representations back into the forms
  -- that the user would have written (e.g. lists into vector literals).
  clean :: a -> a

  -- | Abbreviates long vector literals.
  shortenVec :: a -> a

instance Simplify (Expr Builtin) where
  clean = transformBottomUp cleanNode
  shortenVec = transformBottomUp shortenVecNode

instance Simplify (Arg Builtin) where
  clean = fmap clean . setRelevance Relevant
  shortenVec = fmap shortenVec

instance Simplify (Binder Builtin) where
  clean = fmap clean . setRelevance Relevant
  shortenVec = fmap shortenVec

instance Simplify (Decl Builtin) where
  clean = fmap clean
  shortenVec = fmap shortenVec

instance Simplify (Module Builtin) where
  clean = fmap clean
  shortenVec = fmap shortenVec

--------------------------------------------------------------------------------
-- Traversal

-- | Applies the function to every node in the expression, starting at the
-- leaves. Therefore, when the function is applied to a node, it has already
-- been applied to all the node's sub-expressions.
transformBottomUp :: (Expr Builtin -> Expr Builtin) -> Expr Builtin -> Expr Builtin
transformBottomUp f = f . descend (transformBottomUp f)

-- | Applies the function to the immediate sub-expressions of the expression.
descend :: (Expr Builtin -> Expr Builtin) -> Expr Builtin -> Expr Builtin
descend f expr = case expr of
  Universe {} -> expr
  Hole {} -> expr
  Builtin {} -> expr
  Var {} -> expr
  Pi p binder result -> Pi p (fmap f binder) (f result)
  Let p bound binder body -> Let p (f bound) (fmap f binder) (f body)
  Lam p binder body -> Lam p (fmap f binder) (f body)
  App fun args -> App (f fun) (fmap (fmap f) args)
  Record p fields -> Record p (mapRecordFields f fields)
  RecordAcc p record field -> RecordAcc p (f record) field

--------------------------------------------------------------------------------
-- Cleaning

-- | Cleans a single node, whose sub-expressions have already been cleaned.
cleanNode :: Expr Builtin -> Expr Builtin
cleanNode expr = case expr of
  App fun args -> cleanApp fun (cleanArgs args)
  -- Projections of stdlib type-class operations out of type-class instances
  -- (e.g. `instance.gtTC`) are replaced with the operation itself (e.g. `gtTC`),
  -- which is then resugared into the corresponding operator (e.g. `>`).
  RecordAcc p _ (FieldName _ name)
    | Just _ <- lookupStdlibTypeClassOp name -> Var p name
  -- The internal constraints that literals lie within their type's domain.
  Pi _ binder result
    | isInstance binder && isNatInDomainConstraint (binderValue binder) -> result
  -- The empty list is displayed as an empty vector literal, i.e. `[]`.
  Builtin p (BuiltinConstructor Nil) -> vecLiteral p []
  _ -> expr
  where
    isNatInDomainConstraint e = case headOf e of
      Builtin _ NatInDomainConstraint -> True
      _ -> False

-- | Removes the arguments inserted by the compiler, and the modalities of the
-- remaining arguments.
cleanArgs :: NonEmpty (Arg Builtin) -> [Arg Builtin]
cleanArgs = fmap (setRelevance Relevant) . NonEmpty.filter (not . wasInsertedByCompiler)

-- | Cleans an application whose function and arguments have already been cleaned.
cleanApp :: Expr Builtin -> [Arg Builtin] -> Expr Builtin
cleanApp fun args = case (fun, reverse args) of
  -- Casts of literals are replaced by the literal itself.
  (Builtin _ b, lastArg : _)
    | isLiteralCast b -> argExpr lastArg
  -- Tensors with no dimensions are displayed as their element type.
  (Builtin _ (BuiltinType TensorType), [dims, elemType])
    | Just [] <- vecLiteralElems (argExpr dims) -> argExpr elemType
  (Builtin p (TypeClassOp TensorTypeTC), _) ->
    cleanApp (Builtin p (BuiltinType TensorType)) args
  -- Stacked tensors, vectors and lists are displayed as vector literals.
  (Builtin p b, _)
    | isVecLiteral b -> vecLiteral p args
  (Builtin p (BuiltinConstructor Cons), xs : x : _)
    | Just elems <- vecLiteralElems (argExpr xs) -> vecLiteral p (x : elems)
  -- Existential quantifiers that the loss backend has bounded by a domain
  -- are displayed as quantifiers whose body states the bounds.
  (Builtin _ (BuiltinFunction (QuantifyRatTensor Exists)), [RelevantExplicitArg (Lam p binder body), upper, lower])
    | isExplicit upper && isExplicit lower -> do
        let bounds = normAppList (Var p "boundedBy") [lower, upper]
        let boundedBody = normAppList (Builtin p (BuiltinFunction And)) [explicit bounds, explicit body]
        normAppList fun [explicit (Lam p binder boundedBody)]
  _ -> normAppList fun args
  where
    isVecLiteral = \case
      BuiltinFunction StackTensor -> True
      BuiltinConstructor VectorLiteral -> True
      TypeClassOp VecLiteralTC -> True
      _ -> False

    isLiteralCast = \case
      BuiltinCast FromNat {} -> True
      BuiltinCast FromRat {} -> True
      TypeClassOp FromNatTC {} -> True
      TypeClassOp FromRatTC {} -> True
      _ -> False

-- | Creates a vector literal from the explicit arguments.
vecLiteral :: Provenance -> [Arg Builtin] -> Expr Builtin
vecLiteral p = normAppList (Builtin p (TypeClassOp VecLiteralTC)) . filter isExplicit

-- | If the expression is a vector literal, returns its elements.
vecLiteralElems :: Expr Builtin -> Maybe [Arg Builtin]
vecLiteralElems = \case
  Builtin _ (TypeClassOp VecLiteralTC) -> Just []
  App (Builtin _ (TypeClassOp VecLiteralTC)) args -> Just (NonEmpty.filter isExplicit args)
  _ -> Nothing

--------------------------------------------------------------------------------
-- Shortening vectors

-- | The maximum number of elements of a vector that are displayed.
maxDisplayedVecElems :: Int
maxDisplayedVecElems = 5

-- | Shortens a single node, whose sub-expressions have already been shortened.
-- Vectors with too many elements are displayed as their first and last
-- elements, separated by the number of elements omitted.
shortenVecNode :: Expr Builtin -> Expr Builtin
shortenVecNode expr = case expr of
  App fun@(Builtin p (BuiltinFunction StackTensor)) args@(size :| _)
    | Builtin _ (BuiltinConstructor (NatLiteral n)) <- argExpr size,
      elems@(firstElem : _) <- drop (length args - n) (NonEmpty.toList args),
      length elems > maxDisplayedVecElems -> do
        let omitted = Text.pack $ show (length elems - 2)
        let omittedElems = Arg Explicit Relevant (Var p ("<" <> omitted <> " more>"))
        normAppList fun [firstElem, omittedElems, last elems]
  _ -> expr
