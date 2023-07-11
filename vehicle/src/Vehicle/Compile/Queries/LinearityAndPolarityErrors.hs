module Vehicle.Compile.Queries.LinearityAndPolarityErrors
  ( diagnoseNonLinearity,
    diagnoseAlternatingQuantifiers,
    typeCheckWithSubsystem,
    resolveInstanceArguments,
    removeLiteralCoercions,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Compile.Error
import Vehicle.Compile.Monomorphisation (monomorphise)
import Vehicle.Compile.Normalise.NBE (findInstanceArg)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal, prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type (typeCheckProg)
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Linearity
import Vehicle.Compile.Type.Subsystem.Polarity
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised (GluedExpr (..), GluedProg)
import Vehicle.Libraries.StandardLibrary (StdLibFunction (..), findStdLibFunction)
import Vehicle.Verify.Core (QueryFormatID)

diagnoseNonLinearity ::
  forall m.
  (MonadCompile m) =>
  QueryFormatID ->
  StandardGluedProg ->
  Identifier ->
  m CompileError
diagnoseNonLinearity queryFormat prog propertyIdentifier = do
  setCallDepth 0
  logDebug MinDetail $
    "ERROR: found non-linear property. Switching to linearity type-checking mode for"
      <+> quotePretty propertyIdentifier
      <> line

  Main typedDecls <- typeCheckWithSubsystem prog

  -- Extract and diagnose the type.
  let property = head $ filter (\decl -> identifierOf decl == propertyIdentifier) typedDecls
  let propertyType = unnormalised $ typeOf property
  case propertyType of
    LinearityExpr _ (NonLinear p pp1 pp2) -> do
      let propertyProv = (propertyIdentifier, provenanceOf property)
      throwError $ UnsupportedNonLinearConstraint queryFormat propertyProv p pp1 pp2
    _ -> compilerDeveloperError $ "Unexpected linearity type for property" <+> quotePretty propertyIdentifier

diagnoseAlternatingQuantifiers ::
  forall m.
  (MonadCompile m) =>
  QueryFormatID ->
  StandardGluedProg ->
  Identifier ->
  m CompileError
diagnoseAlternatingQuantifiers queryFormat prog propertyIdentifier = do
  setCallDepth 0
  logDebug MinDetail $
    "ERROR: found property with alterating quantifiers. Switching to polarity type-checking mode for"
      <+> quotePretty propertyIdentifier
      <> line

  Main typedDecls <- typeCheckWithSubsystem prog

  -- Extract and diagnose the type.
  let property = head $ filter (\decl -> identifierOf decl == propertyIdentifier) typedDecls
  let propertyType = unnormalised $ typeOf property
  case propertyType of
    PolarityExpr _ (MixedSequential p pp1 pp2) -> do
      let propertyProv = (propertyIdentifier, provenanceOf property)
      throwError $ UnsupportedAlternatingQuantifiers queryFormat propertyProv p pp1 pp2
    _ -> compilerDeveloperError $ "Unexpected polarity type for property" <+> quotePretty propertyIdentifier

typeCheckWithSubsystem ::
  forall builtin m.
  (TypableBuiltin builtin, MonadCompile m) =>
  StandardGluedProg ->
  m (GluedProg builtin)
typeCheckWithSubsystem prog = do
  let unnormalisedProg = fmap unnormalised prog
  typeClassFreeProg <- resolveInstanceArguments unnormalisedProg
  monomorphisedProg <- monomorphise isPropertyDecl "-" typeClassFreeProg
  implicitFreeProg <- removeImplicitAndInstanceArgs monomorphisedProg
  runTypeChecker @m @builtin mempty $
    typeCheckProg mempty implicitFreeProg

resolveInstanceArguments :: forall m. (MonadCompile m) => StandardProg -> m StandardProg
resolveInstanceArguments prog =
  logCompilerPass MaxDetail "resolution of instance arguments" $ do
    result <- traverse (traverseBuiltinsM builtinUpdateFunction) prog
    logCompilerPassOutput $ prettyFriendly result
    return result
  where
    builtinUpdateFunction :: BuiltinUpdate m Ix StandardBuiltin StandardBuiltin
    builtinUpdateFunction p1 p2 b args = case b of
      CType (StandardTypeClassOp {}) -> do
        (inst, remainingArgs) <- findInstanceArg b args
        return $ normAppList p1 inst remainingArgs
      _ -> return $ normAppList p1 (Builtin p2 b) args

removeLiteralCoercions :: forall m. (MonadCompile m) => StandardProg -> m StandardProg
removeLiteralCoercions =
  traverseDecls
    ( \d ->
        traverse
          ( \e -> do
              e' <- traverseBuiltinsM (updateBuiltin d) e
              traverseFreeVarsM (updateFreeVar d) e'
          )
          d
    )
  where
    updateBuiltin decl p1 p2 b args = case b of
      (CFunction (FromNat dom)) -> case (dom, args) of
        (FromNatToIndex, [_, ExplicitArg _ (NatLiteral p n), _]) -> return $ IndexLiteral p n
        (FromNatToNat, [e, _]) -> return $ argExpr e
        (FromNatToInt, [ExplicitArg _ (NatLiteral p n), _]) -> return $ IntLiteral p n
        (FromNatToRat, [ExplicitArg _ (NatLiteral p n), _]) -> return $ RatLiteral p (fromIntegral n)
        -- Hack until https://github.com/vehicle-lang/vehicle/issues/621 is fixed.
        _ -> return $ normAppList p1 (Builtin p2 b) args -- partialApplication decl (pretty b) args
      (CFunction (FromRat dom)) -> case (dom, args) of
        (FromRatToRat, [e]) -> return $ argExpr e
        _ -> partialApplication decl (pretty b) args
      _ -> return $ normAppList p1 (Builtin p2 b) args

    updateFreeVar decl p1 p2 v args = case findStdLibFunction v of
      Just StdVectorToVector -> case reverse args of
        vec : _ -> return $ argExpr vec
        _ -> partialApplication decl (pretty v) args
      Just StdVectorToList -> case reverse args of
        ExplicitArg _ (VecLiteral p l xs) : _ -> return $ mkList p l (fmap argExpr xs)
        _ -> partialApplication decl (pretty v) args
      _ -> return $ normAppList p1 (FreeVar p2 v) args

    partialApplication decl v args =
      compilerDeveloperError $
        "Found partially applied"
          <+> squotes v
          <+> "@"
          <+> prettyVerbose args
          <+> "in"
          <> line
          <> indent
            2
            ( prettyFriendly decl
                <> line
                <> line
                <> prettyVerbose decl
                <> line
                <> line
                <> pretty (show $ bodyOf decl)
            )

removeImplicitAndInstanceArgs :: forall m. (MonadCompile m) => TypeCheckedProg -> m TypeCheckedProg
removeImplicitAndInstanceArgs prog =
  logCompilerPass MaxDetail "removal of implicit arguments" $ do
    result <- traverse go prog
    logCompilerPassOutput $ prettyExternal result
    return result
  where
    go :: TypeCheckedExpr -> m TypeCheckedExpr
    go expr = case expr of
      App p fun args -> do
        fun' <- go fun
        let nonImplicitArgs = NonEmpty.filter isExplicit args
        nonImplicitArgs' <- traverse (traverse go) nonImplicitArgs
        return $ normAppList p fun' nonImplicitArgs'
      BoundVar {} -> return expr
      FreeVar {} -> return expr
      Universe {} -> return expr
      Meta {} -> return expr
      Hole {} -> return expr
      Builtin {} -> return expr
      Ann p e t -> Ann p <$> go e <*> go t
      Pi p binder res -> Pi p <$> traverse go binder <*> go res
      Lam p binder body
        | isExplicit binder || not (isTypeUniverse (typeOf binder)) ->
            Lam p <$> traverse go binder <*> go body
        | otherwise -> do
            -- TODO This is a massive hack to get around the unused implicit
            -- {l} argument in `mapVector` in the standard library that isn't
            -- handled by monomorphisation.
            body' <- go body
            let removedBody = Hole p "_" `substDBInto` body'
            return removedBody
      Let p bound binder body -> Let p <$> go bound <*> traverse go binder <*> go body
