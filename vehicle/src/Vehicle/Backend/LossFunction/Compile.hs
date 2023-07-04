module Vehicle.Backend.LossFunction.Compile
  ( LDecl,
    compile,
  )
where

import Vehicle.Backend.LossFunction.Syntax
import Vehicle.Backend.Prelude (DifferentiableLogicID (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Subsystem.Standard qualified as V
import Vehicle.Prelude
import Vehicle.Resource (Resources (..))

{-
import Control.Monad.Except (MonadError (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Vehicle.Backend.LossFunction.Logics
import Vehicle.Backend.Prelude (DifferentiableLogicID (..))
import Vehicle.Compile.Descope (DescopeNamed (descopeNamed))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude qualified as V
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Queries.LinearityAndPolarityErrors (resolveInstanceArguments)
import Vehicle.Compile.Type.Subsystem.Standard qualified as V
import Vehicle.Compile.Type.Subsystem.Standard.Patterns qualified as V
import Vehicle.Expr.Normalisable qualified as V
import Vehicle.Expr.Normalised (GluedExpr (..))
import Vehicle.Libraries.StandardLibrary
import Vehicle.Prelude
import Vehicle.Resource (Resources (..))
import Vehicle.Syntax.AST (argExpr)
import Vehicle.Backend.LossFunction.Syntax
import Vehicle.Backend.JSON (ToJBuiltin (..), compileProgToJSON)
import Vehicle.Expr.DeBruijn (Ix)
import Vehicle.Compile.Type.Subsystem.Standard (StandardBuiltinType(..))
import Vehicle.Expr.DSL (fromDSL)
-}

--------------------------------------------------------------------------------
-- Compilation

-- | The translation into the LExpr (this is the exported top compile function)
compile ::
  (MonadCompile m) =>
  Resources ->
  DifferentiableLogicID ->
  V.StandardGluedProg ->
  m (Doc a)
compile _resources _logic _typedProg = do
  return "" {-
              let unnormalisedProg = fmap unnormalised typedProg
              progWithoutInstanceArgs <- resolveInstanceArguments unnormalisedProg

              let logicImplementation = implementationOf logic
              reformattedProg <- reformatLogicalOperators logicImplementation progWithoutInstanceArgs
              lossProg <- compileProg logicImplementation reformattedProg
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
              m [LDecl]
            compileProg logic (V.Main ds) =
              logCompilerPass MinDetail currentPass $
                traverse (compileDecl logic) ds

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
            compileExpr DifferentialLogicImplementation{..} declProv =
              V.traverseBuiltinsM $ \p1 p2 op args -> do
                let lossOp' = case op of
                      V.CConstructor x -> case x of
                        V.LBool True  -> fromDSL p2 compileTrue
                        V.LBool False -> fromDSL p2 compileFalse
                        _ -> V.Builtin p2 $ toJBuiltin x
                      V.CFunction x -> case x of
                        V.Not -> case compileNot of
                          TryToEliminate -> _
                          UnaryNot notFn -> fromDSL p2 notFn
                        V.And -> case compileAnd of
                          NaryAnd andFn -> fromDSL p2 andFn
                          BinaryAnd andFn -> fromDSL p2 andFn
                        V.Or -> case compileOr of
                          NaryOr orFn -> fromDSL p2 orFn
                          BinaryOr orFn -> fromDSL p2 orFn
                        V.Implies -> fromDSL p2 compileImplies
                        V.Quantifier V.Forall dom -> compileForall dom
                        V.Quantifier V.Exists dom -> compileExists dom
                        V.If -> _
                        V.Equals V.EqRat V.Eq -> fromDSL p2 compileEq
                        V.Equals _ V.Eq -> _
                        V.Equals V.EqRat V.Neq -> fromDSL p2 compileNeq
                        V.Equals _ V.Neq -> _
                        V.Order V.OrderRat V.Le -> fromDSL p2 compileLe
                        V.Order _ V.Le -> _
                        V.Order V.OrderRat  V.Lt -> fromDSL p2 compileLt
                        V.Order _ V.Lt -> _
                        V.Order V.OrderRat  V.Ge -> fromDSL p2 compileGe
                        V.Order _ V.Ge -> _
                        V.Order V.OrderRat  V.Gt -> fromDSL p2 compileGt
                        V.Order _ V.Gt -> _
                        _ -> V.Builtin p2 $ toJBuiltin x
                      V.CType x -> case x of
                        StandardBuiltinType y -> case y of
                          V.Bool -> fromDSL p2 compileBool
                          _ -> V.Builtin p2 $ toJBuiltin x
                        StandardTypeClass{} -> typeClassesUnresolvedError
                        StandardTypeClassOp{} -> typeClassesUnresolvedError
                return $ V.normAppList p1 lossOp' args

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
                builtinUpdateFunction :: V.BuiltinUpdate m () Ix V.StandardBuiltin V.StandardBuiltin
                builtinUpdateFunction p1 p2 b args = do

                  maybeUpdatedExpr <- case b of
                    V.CFunction V.Not -> case compileNot logic of
                      TryToEliminate -> Just <$> lowerNot p2 (argExpr $ head args)
                      UnaryNot{} -> return Nothing
                    V.CFunction V.And -> case compileAnd logic of
                      NaryAnd{} -> return $ Just (V.AndExpr p1 (flattenAnds (V.ExplicitArg p1 (V.AndExpr p1 (NonEmpty.fromList args)))))
                      BinaryAnd{} -> return Nothing
                    V.CFunction V.Or -> case compileOr logic of
                      NaryOr{} -> return $ Just (V.OrExpr p1 (flattenOrs (V.ExplicitArg p1 (V.OrExpr p1 (NonEmpty.fromList args)))))
                      BinaryOr{} -> return Nothing
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
                  e -> throwError $ UnsupportedNegatedOperation (logicID logic) notProv e

                flattenAnds :: V.Arg Ix V.StandardBuiltin -> NonEmpty (V.Arg Ix V.StandardBuiltin)
                flattenAnds arg = case argExpr arg of
                  V.AndExpr _ [e1, e2] -> flattenAnds e1 <> flattenAnds e2
                  _ -> [arg]

                flattenOrs :: V.Arg Ix V.StandardBuiltin -> NonEmpty (V.Arg Ix V.StandardBuiltin)
                flattenOrs arg = case argExpr arg of
                  V.OrExpr _ [e1, e2] -> flattenOrs e1 <> flattenOrs e2
                  _ -> [arg]

            -----------------------------------------------------------------------
            -- Debugging options

            showEntry :: (MonadCompile m) => V.Expr V.Name V.StandardBuiltin -> m (V.Expr V.Name V.StandardBuiltin)
            showEntry e = do
              logDebug MinDetail ("loss-entry " <> prettyFriendly e)
              incrCallDepth
              return e

            showExit :: (MonadCompile m) => m LExpr -> m LExpr
            showExit mNew = do
              new <- mNew
              decrCallDepth
              logDebug MinDetail ("loss-exit " <+> pretty (show new))
              return new
            -}
