module Vehicle.Backend.Tensors.Clean
  ( cleanUpHigherOrderStuff,
  )
where

import Control.Monad.State (MonadState, evalStateT, gets, modify)
import Data.Data (Proxy (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Context.Var
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Expr.BuiltinInterface

cleanUpHigherOrderStuff ::
  forall m builtin.
  (MonadCompile m, HasStandardData builtin, PrintableBuiltin builtin) =>
  Prog Ix builtin ->
  m (Prog Ix builtin)
cleanUpHigherOrderStuff (Main ds) =
  Main <$> runFreshVarContextT (Proxy @builtin) (evalStateT (cleanDecls ds) mempty)

type MonadClean m builtin =
  ( MonadCompile m,
    MonadState (Map Identifier (Bool, Expr Ix builtin)) m,
    MonadVarContext builtin m,
    PrintableBuiltin builtin
  )

cleanDecls :: forall m builtin. (MonadClean m builtin) => [Decl Ix builtin] -> m [Decl Ix builtin]
cleanDecls = \case
  [] -> return []
  decl : decls -> do
    cleanedDecl <- traverse cleanExpr decl
    addDeclToContext cleanedDecl $ do
      let ident = identifierOf decl
      let declType = typeOf decl
      maybeNewDecl <- case cleanedDecl of
        DefAbstract {} -> return $ Just cleanedDecl
        DefFunction _ _ _ _ e -> do
          if isTypeSynonym declType
            then do
              modify (Map.insert ident (True, e))
              return Nothing
            else
              if hasHigherOrderFunctions declType
                then do
                  modify (Map.insert ident (False, e))
                  return Nothing
                else return $ Just cleanedDecl

      cleanedDecls <- cleanDecls decls
      return $ maybeToList maybeNewDecl <> cleanedDecls

hasHigherOrderFunctions :: Type Ix builtin -> Bool
hasHigherOrderFunctions = \case
  Pi _ binder body -> case typeOf binder of
    Pi {} -> True
    _ -> hasHigherOrderFunctions body
  _ -> False

cleanExpr :: (MonadClean m builtin) => Expr Ix builtin -> m (Expr Ix builtin)
cleanExpr expr = case expr of
  -- Free variables test to see if they should be eliminated
  FreeVar p v -> cleanFreeVar p p v []
  App p1 fun args -> do
    args' <- traverse (traverse cleanExpr) args
    case fun of
      FreeVar p2 v -> cleanFreeVar p1 p2 v (NonEmpty.toList args')
      Lam {} -> substArgs p1 <$> cleanExpr fun <*> pure (NonEmpty.toList args')
      _ -> App p1 <$> cleanExpr fun <*> pure args'

  -- Pi binders should have the binder form changed so as to only display the
  -- type as we no longer have dependently typed-indices.
  Pi p binder body -> do
    let typeOnlyBinder = mapBinderNamingForm (const OnlyType) binder
    Pi p <$> traverse cleanExpr typeOnlyBinder <*> cleanExpr body
  BoundVar {} -> return expr
  Universe {} -> return expr
  Meta {} -> return expr
  Hole {} -> return expr
  Builtin {} -> return expr
  Let p e1 binder e2 -> Let p <$> cleanExpr e1 <*> traverse cleanExpr binder <*> cleanExpr e2
  Lam p binder e -> Lam p <$> traverse cleanExpr binder <*> cleanExpr e

cleanFreeVar ::
  (MonadClean m builtin) =>
  Provenance ->
  Provenance ->
  Identifier ->
  [Arg Ix builtin] ->
  m (Expr Ix builtin)
cleanFreeVar p1 p2 ident args = do
  maybeSubstitution <- gets (Map.lookup ident)
  case maybeSubstitution of
    Nothing -> return $ normAppList p1 (FreeVar p2 ident) args
    Just (shouldNormalise, value) -> do
      let newValue = substArgs p1 value args
      if shouldNormalise
        then unnormalise =<< normalise newValue
        else return newValue
