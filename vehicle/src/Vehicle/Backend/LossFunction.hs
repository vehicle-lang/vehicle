module Vehicle.Backend.LossFunction
  ( lossPreprocessingStep,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Data.Proxy (Proxy (..))
import Vehicle.Backend.LossFunction.Convert
import Vehicle.Backend.LossFunction.Logics
import Vehicle.Backend.Prelude
import Vehicle.Compile.Context.Bound
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.BuiltinInterface.ASTInterface
import Vehicle.Data.DSL (DSLExpr, fromDSL)
import Vehicle.Data.NormalisedExpr
import Vehicle.Syntax.Builtin

lossPreprocessingStep :: DifferentiableLogicID -> TensorPreprocessingStep
lossPreprocessingStep logicID = TensorPreprocessingStep $ convertLossExpr (implementationOf logicID)

convertLossExpr ::
  forall m.
  (MonadTensorProperty m) =>
  DifferentialLogicImplementation ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
convertLossExpr DifferentialLogicImplementation {..} u = do logDebug MaxDetail $ prettyVerbose u; go u
  where
    go :: WHNFValue Builtin -> m (WHNFValue Builtin)
    go expr = case expr of
      VMeta {} -> unexpectedExprError currentPass "VMeta"
      VUniverse {} -> unexpectedExprError currentPass "VUniverse"
      VPi {} -> unexpectedExprError currentPass "VPi"
      VFreeVar v spine -> VFreeVar v <$> traverse (traverse go) spine
      VBoundVar v spine -> VBoundVar v <$> traverse (traverse go) spine
      VBuiltin b spine -> do
        logDebug MaxDetail $ quotePretty b
        case goBuiltin b of
          NoTranslation -> do
            spine' <- traverse (traverse go) spine
            return $ VBuiltin b spine'
          Translation t -> do
            spine' <- traverse (traverse go) spine
            let unnormFn = fromDSL mempty t
            normFn <- normaliseInEnv mempty unnormFn
            normaliseApp normFn spine'
          EliminateNot -> do
            case spine of
              [arg] -> case lowerNot (argExpr arg) of
                Right result -> go result
                Left errExpr -> do
                  ctx <- getNamedBoundCtx (Proxy @Builtin)
                  throwError $ UnsupportedNegatedOperation logicID ctx errExpr
              _ -> illTypedError currentPass (prettyVerbose expr)
          ErrorCase mkError -> do
            declProv <- ask
            throwError $ mkError declProv mempty
      VLam binder body -> return $ VLam binder body

    goBuiltin :: Builtin -> BuiltinTranslation
    goBuiltin b = case b of
      BuiltinConstructor c -> goConstructor c
      BuiltinFunction f -> goFunction f
      _ -> NoTranslation

    goConstructor :: BuiltinConstructor -> BuiltinTranslation
    goConstructor c = case c of
      LBool True -> Translation compileTrue
      LBool False -> Translation compileFalse
      _ -> NoTranslation

    goFunction :: BuiltinFunction -> BuiltinTranslation
    goFunction f = case f of
      And -> Translation compileAnd
      Or -> Translation compileOr
      Not -> case compileNot of
        UnaryNot func -> Translation func
        TryToEliminate -> EliminateNot
      Implies -> Translation compileImplies
      Quantifier Forall -> Translation compileForall
      Quantifier Exists -> Translation compileExists
      If -> ErrorCase UnsupportedIfOperation
      Order OrderRat Ge -> Translation compileGe
      Order OrderRat Le -> Translation compileLe
      Order OrderRat Lt -> Translation compileLt
      Order OrderRat Gt -> Translation compileGt
      _ -> NoTranslation

lowerNot :: WHNFValue Builtin -> Either (WHNFValue Builtin) (WHNFValue Builtin)
lowerNot arg = case arg of
  -- Base cases
  IBoolLiteral p v ->
    return $ IBoolLiteral p (not v)
  VBuiltinFunction (Order dom op) args ->
    return $ VBuiltinFunction (Order dom (neg op)) args
  VBuiltinFunction (Equals dom op) args ->
    return $ VBuiltinFunction (Equals dom (neg op)) args
  INot x ->
    return x
  -- Easy inductive cases
  IInfiniteQuantifier q args (VLam binder (WHNFBody env body)) ->
    return $ IInfiniteQuantifier (neg q) args (VLam binder (WHNFBody env (INot body)))
  IAnd x y ->
    IOr <$> lowerNot x <*> lowerNot y
  IOr x y ->
    IAnd <$> lowerNot x <*> lowerNot y
  IIf tRes c e1 e2 ->
    IIf tRes c <$> lowerNot e1 <*> lowerNot e2
  -- Difficult inductive cases
  IFoldVector n t1 t2 (ExpandedBinaryOp binder1 env binder2 (BuiltinFunction b)) e xs -> do
    let mkBinOp op = ExpandedBinaryOp binder1 env binder2 (BuiltinFunction op)
    case b of
      And -> IFoldVector n t1 t2 (mkBinOp Or) <$> lowerNot e <*> lowerNotVector xs
      Or -> IFoldVector n t1 t2 (mkBinOp And) <$> lowerNot e <*> lowerNotVector xs
      _ -> Left arg
  -- Error cases
  _ -> Left arg

lowerNotVector :: WHNFValue Builtin -> Either (WHNFValue Builtin) (WHNFValue Builtin)
lowerNotVector arg = case arg of
  IVecLiteral t xs ->
    IVecLiteral t <$> traverse (traverse lowerNot) xs
  IForeachIndex t n (VLam binder (WHNFBody env body)) ->
    return $ IForeachIndex t n (VLam binder (WHNFBody env (INot body)))
  _ -> Left arg

data BuiltinTranslation
  = NoTranslation
  | Translation (DSLExpr Builtin)
  | EliminateNot
  | ErrorCase (DeclProvenance -> Provenance -> CompileError)

currentPass :: CompilerPass
currentPass = "loss function translation"

pattern ExpandedBinaryOp :: WHNFBinder builtin -> WHNFBoundEnv builtin -> Binder Ix builtin -> builtin -> WHNFValue builtin
pattern ExpandedBinaryOp binder1 env binder2 b <-
  VLam
    binder1
    ( WHNFBody
        env
        ( Lam
            _
            binder2
            ( BuiltinExpr
                _
                b
                [ RelevantExplicitArg _ (BoundVar _ 1),
                  RelevantExplicitArg _ (BoundVar _ 0)
                  ]
              )
          )
      )
  where
    ExpandedBinaryOp binder1 env binder2 b =
      VLam
        binder1
        ( WHNFBody
            env
            ( Lam
                mempty
                binder2
                ( BuiltinExpr
                    mempty
                    b
                    [ Arg mempty Explicit Relevant (BoundVar mempty 1),
                      Arg mempty Explicit Relevant (BoundVar mempty 0)
                    ]
                )
            )
        )
