module Vehicle.Compile.CheckBackendFeatures
  ( BackendKind (..),
    checkBackendUnsupportedFeatures,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Writer (MonadWriter (..), execWriter)
import Data.Foldable (traverse_)
import Data.List (nub)
import Data.Text (Text)
import Data.Void (Void)
import Vehicle.Backend.Prelude (InteractiveTheoremProverID)
import Vehicle.Compile.Error (CompileError (..), MonadCompile)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Standard.Core (Builtin (..))
import Vehicle.Libraries.StandardLibrary (stlLibIdent)

-- | Which backend's feature-rejection rules to apply.
data BackendKind
  = Verifier
  | ITPBackend InteractiveTheoremProverID

instance Pretty BackendKind where
  pretty = \case
    Verifier -> "verifier"
    ITPBackend itp -> pretty itp <+> "ITP"

-- | Reject unsupported builtins for a backend, reporting all hits together.
checkBackendUnsupportedFeatures ::
  (MonadCompile m) =>
  BackendKind ->
  Prog Builtin ->
  m ()
checkBackendUnsupportedFeatures backend (Main decls) = do
  -- Only walk user decls; stdlib instance types can mention Time even when unused.
  let userDecls = filter isUserCode decls
  let hits = nub (execWriter (traverse_ (traverse_ goExpr) userDecls))
  case hits of
    [] -> return ()
    (_, firstProv) : _ ->
      throwError $ UnsupportedBackendFeatures firstProv (formatHits backend hits)

-- | (feature description, location). Description is used to deduplicate hits.
type Hit = (Text, Provenance)

formatHits :: BackendKind -> [Hit] -> Doc Void
formatHits backend hits =
  vsep @[]
    [ "The following features are not supported in the" <+> pretty backend <+> "backend:",
      indent 2 (vsep @[] (fmap formatHit hits)),
      line
        <> "Specs that need temporal operators or `Time` should be compiled with"
          <+> "`vehicle compile loss --logic STLLoss …` instead."
    ]
  where
    formatHit :: Hit -> Doc Void
    formatHit (desc, prov) =
      hsep @[] ["-", pretty desc, "(" <> pretty prov <> ")"]

------------------------------------------------------------------------
-- Walk

goExpr :: (MonadWriter [Hit] m) => Expr Builtin -> m ()
goExpr expr = case expr of
  Universe {} -> return ()
  BoundVar {} -> return ()
  -- Temporal operators reach user code as STL library wrappers, not as the builtin.
  FreeVar p ident -> case unsupportedFreeVarName ident of
    Nothing -> return ()
    Just desc -> tell [(desc, p)]
  Hole {} -> return ()
  Meta {} -> return ()
  Builtin p b -> reportIfUnsupported p b
  App fun args -> goExpr fun >> traverse_ (goExpr . argExpr) args
  Pi _ binder res -> goExpr (binderValue binder) >> goExpr res
  Let _ bound binder body -> goExpr bound >> goExpr (binderValue binder) >> goExpr body
  Lam _ binder body -> goExpr (binderValue binder) >> goExpr body
  Record _ t fs -> goExpr t >> traverse_ (goExpr . snd) fs
  RecordProj _ t r _ -> goExpr t >> goExpr r

------------------------------------------------------------------------
-- Per-builtin rejection table

-- | If this builtin is unsupported, append a hit for it.
reportIfUnsupported ::
  (MonadWriter [Hit] m) =>
  Provenance ->
  Builtin ->
  m ()
reportIfUnsupported p b =
  case unsupportedFeatureName b of
    Nothing -> return ()
    Just desc -> tell [(desc, p)]

-- | Unsupported in non-loss backends (verifier + ITP): temporal, rollout, Time.
unsupportedFeatureName :: Builtin -> Maybe Text
unsupportedFeatureName = \case
  BuiltinFunction (Temporal Globally) -> Just "temporal operator 'globally'"
  BuiltinFunction (Temporal Finally) -> Just "temporal operator 'finally'"
  BuiltinFunction (Temporal Until) -> Just "temporal operator 'until'"
  BuiltinFunction Rollout -> Just "'rollout'"
  BuiltinType TimeType -> Just "'Time' type"
  -- TimeLiteral skipped: only ever appears nested inside an operator that
  -- itself is already in the rejection list (temporal bounds, rollout
  -- count, or a `Time` annotation), so reporting it separately produces
  -- redundant entries with poor provenance.
  BuiltinFunction (Add AddTime) -> Just "'+' on 'Time'"
  BuiltinFunction (Sub SubTime) -> Just "'-' on 'Time'"
  BuiltinFunction (Mul MulTime) -> Just "'*' on 'Time'"
  BuiltinFunction (Div DivTime) -> Just "'/' on 'Time'"
  BuiltinCast (FromNat FromNatToTime) -> Just "cast 'Nat -> Time'"
  BuiltinCast (FromTime FromTimeToNat) -> Just "cast 'Time -> Nat'"
  _ -> Nothing

-- | The `STL` library wrappers for the temporal operators / `rollout`.
unsupportedFreeVarName :: Identifier -> Maybe Text
unsupportedFreeVarName ident
  | ident == stlLibIdent "globally" = Just "temporal operator 'globally'"
  | ident == stlLibIdent "finally" = Just "temporal operator 'finally'"
  | ident == stlLibIdent "until" = Just "temporal operator 'until'"
  | ident == stlLibIdent "rollout" = Just "'rollout'"
  | otherwise = Nothing
