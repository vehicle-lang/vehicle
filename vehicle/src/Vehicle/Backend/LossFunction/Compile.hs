module Vehicle.Backend.LossFunction.Compile
  ( LDecl,
    compile,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Vehicle.Backend.JSON (ToJBuiltin (..), compileProgToJSON)
import Vehicle.Backend.LossFunction.Logics
import Vehicle.Backend.LossFunction.Syntax
import Vehicle.Backend.Prelude (DifferentiableLogicID (..))
import Vehicle.Compile.Descope (DescopeNamed (descopeNamed))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude qualified as V
import Vehicle.Compile.Type.Subsystem.Standard (StandardBuiltinType (..))
import Vehicle.Compile.Type.Subsystem.Standard qualified as V
import Vehicle.Compile.Type.Subsystem.Standard.Patterns qualified as V
import Vehicle.Expr.DSL (fromDSL)
import Vehicle.Expr.DeBruijn (Ix)
import Vehicle.Expr.Normalisable qualified as V
import Vehicle.Expr.Normalised (GluedExpr (..))
import Vehicle.Libraries.StandardLibrary
import Vehicle.Prelude
import Vehicle.Syntax.AST (argExpr)

--------------------------------------------------------------------------------
-- Compilation

-- | The translation into the LExpr (this is the exported top compile function)
compile ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  V.StandardGluedProg ->
  m (Doc a)
compile logic typedProg = do
  let unnormalisedProg = fmap unnormalised typedProg
  let logicImplementation = implementationOf logic
  reformattedProg <- reformatLogicalOperators logicImplementation unnormalisedProg
  lossProg <- compileProg logicImplementation reformattedProg
  -- monomorphisedProg <- _
  let descopedProg = descopeNamed lossProg
  compileProgToJSON descopedProg

--------------------------------------------------------------------------------
-- Utilities

currentPass :: Doc a
currentPass = "compilation to loss functions"

--------------------------------------------------------------------------------
-- Main compilation pass

compileProg ::
  (MonadCompile m) =>
  DifferentialLogicImplementation ->
  V.Prog Ix V.StandardBuiltin ->
  m LProg
compileProg logic (V.Main ds) =
  logCompilerPass MinDetail currentPass $
    V.Main <$> traverse (compileDecl logic) ds

compileDecl ::
  (MonadCompile m) =>
  DifferentialLogicImplementation ->
  V.Decl Ix V.StandardBuiltin ->
  m LDecl
compileDecl logic decl = do
  let declProv = (V.identifierOf decl, V.provenanceOf decl)
  logCompilerPass MinDetail ("compilation of" <+> quotePretty (V.identifierOf decl) <+> "to loss function") $
    traverse (compileExpr logic declProv) decl

-- | Compile a property or single expression
compileExpr ::
  (MonadCompile m) =>
  DifferentialLogicImplementation ->
  V.DeclProvenance ->
  V.Expr Ix V.StandardBuiltin ->
  m LExpr
compileExpr DifferentialLogicImplementation {..} declProv =
  V.traverseBuiltinsM $ \p1 p2 op args -> do
    maybeLossOp <- case op of
      V.CConstructor x -> return $ case x of
        V.LBool True -> Just compileTrue
        V.LBool False -> Just compileFalse
        _ -> Nothing
      V.CFunction x -> case x of
        V.Not -> case compileNot of
          TryToEliminate ->
            compilerDeveloperError
              "Should have eliminated `not` already in compilation to loss functions"
          UnaryNot notFn -> return $ Just notFn
        V.And -> case compileAnd of
          NaryAnd andFn -> return $ Just andFn
          BinaryAnd andFn -> return $ Just andFn
        V.Or -> case compileOr of
          NaryOr orFn -> return $ Just orFn
          BinaryOr orFn -> return $ Just orFn
        V.Implies -> return $ Just compileImplies
        V.Quantifier q _ -> case reverse args of
          V.ExplicitArg _ (V.Lam _ binder _) : _ -> return $ Just $ case q of
            V.Forall -> compileForall (V.getBinderName binder)
            V.Exists -> compileExists (V.getBinderName binder)
          _ -> unexpectedExprError currentPass (pretty op)
        V.If -> throwError $ UnsupportedIfOperation declProv p2
        -- TODO really not safe to throw away the type information here, but
        -- in the short term it might work.
        V.Equals _ V.Eq -> return $ Just compileEq
        V.Equals _ V.Neq -> return $ Just compileNeq
        V.Order _ V.Le -> return $ Just compileLe
        V.Order _ V.Lt -> return $ Just compileLt
        V.Order _ V.Ge -> return $ Just compileGe
        V.Order _ V.Gt -> return $ Just compileGt
        _ -> return Nothing
      V.CType x -> case x of
        StandardBuiltinType y -> return $ case y of
          V.Bool -> Just compileBool
          _ -> Nothing
        StandardTypeClass {} -> unexpectedExprError currentPass (pretty x)
        StandardTypeClassOp {} -> unexpectedExprError currentPass (pretty x)

    let newOp = case maybeLossOp of
          Nothing -> V.Builtin p2 $ toJBuiltin op
          Just lossOp -> fromDSL p2 lossOp

    return $ V.normAppList p1 newOp args

--------------------------------------------------------------------------------
-- Reformating logical operators

reformatLogicalOperators ::
  forall m.
  (MonadCompile m) =>
  DifferentialLogicImplementation ->
  V.Prog Ix V.StandardBuiltin ->
  m (V.Prog Ix V.StandardBuiltin)
reformatLogicalOperators logic = traverse (V.traverseBuiltinsM builtinUpdateFunction)
  where
    builtinUpdateFunction :: V.BuiltinUpdate m Ix V.StandardBuiltin V.StandardBuiltin
    builtinUpdateFunction p1 p2 b args = do
      maybeUpdatedExpr <- case b of
        V.CFunction V.Not -> case compileNot logic of
          TryToEliminate -> Just <$> lowerNot p2 (argExpr $ head args)
          UnaryNot {} -> return Nothing
        V.CFunction V.And -> case compileAnd logic of
          NaryAnd {} -> return $ Just (V.AndExpr p1 (flattenAnds (V.ExplicitArg p1 (V.AndExpr p1 (NonEmpty.fromList args)))))
          BinaryAnd {} -> return Nothing
        V.CFunction V.Or -> case compileOr logic of
          NaryOr {} -> return $ Just (V.OrExpr p1 (flattenOrs (V.ExplicitArg p1 (V.OrExpr p1 (NonEmpty.fromList args)))))
          BinaryOr {} -> return Nothing
        _ -> return Nothing

      let unchangedExpr = V.normAppList p1 (V.Builtin p2 b) args
      return $ fromMaybe unchangedExpr maybeUpdatedExpr

    lowerNot :: V.Provenance -> V.Expr Ix V.StandardBuiltin -> m (V.Expr Ix V.StandardBuiltin)
    lowerNot notProv arg = case arg of
      -- Base cases
      V.BoolLiteral p b -> return $ V.BoolLiteral p (not b)
      V.OrderExpr p dom ord args -> return $ V.OrderExpr p dom (neg ord) args
      V.EqualityExpr p dom eq args -> return $ V.EqualityExpr p dom (neg eq) args
      V.NotExpr _ [e] -> return $ argExpr e
      -- Inductive cases
      V.ForallRatExpr p binder body -> V.ExistsRatExpr p binder <$> lowerNot notProv body
      V.ExistsRatExpr p binder body -> V.ForallRatExpr p binder <$> lowerNot notProv body
      V.ImpliesExpr p [e1, e2] -> do
        ne2 <- traverse (lowerNot notProv) e2
        return $ V.AndExpr p [e1, ne2]
      V.OrExpr p args -> V.AndExpr p <$> traverse (traverse (lowerNot notProv)) args
      V.AndExpr p args -> V.OrExpr p <$> traverse (traverse (lowerNot notProv)) args
      V.IfExpr p tRes [c, e1, e2] -> do
        ne1 <- traverse (lowerNot notProv) e1
        ne2 <- traverse (lowerNot notProv) e2
        return $ V.IfExpr p tRes [c, ne1, ne2]
      V.App p1 (V.FreeVar p2 ident) args
        | ident == V.identifierOf StdEqualsVector -> return $ V.App p1 (V.FreeVar p2 (V.identifierOf StdNotEqualsVector)) args
        | ident == V.identifierOf StdNotEqualsVector -> return $ V.App p1 (V.FreeVar p2 (V.identifierOf StdEqualsVector)) args
      -- Errors
      _ -> throwError $ UnsupportedNegatedOperation (logicID logic) notProv

    flattenAnds :: V.Arg Ix V.StandardBuiltin -> NonEmpty (V.Arg Ix V.StandardBuiltin)
    flattenAnds arg = case argExpr arg of
      V.AndExpr _ [e1, e2] -> flattenAnds e1 <> flattenAnds e2
      _ -> [arg]

    flattenOrs :: V.Arg Ix V.StandardBuiltin -> NonEmpty (V.Arg Ix V.StandardBuiltin)
    flattenOrs arg = case argExpr arg of
      V.OrExpr _ [e1, e2] -> flattenOrs e1 <> flattenOrs e2
      _ -> [arg]
