module Vehicle.Compile.Normalise
  ( NormalisationOptions (..),
    fullNormalisationOptions,
    normaliseProg,
    normaliseExpr,
    nfTypeClassOp,
  )
where

import Control.Monad (when)
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Control.Monad.State (MonadState (..), evalStateT, modify)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty (reverse, toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalisable (NormalisableBuiltin (..))
import Vehicle.Expr.Normalised (GluedExpr (..), traverseUnnormalised)
import Vehicle.Libraries.StandardLibrary

-- | Run a function in 'MonadNorm'.
normaliseProg :: MonadCompile m => TypeCheckedProg -> NormalisationOptions -> m TypeCheckedProg
normaliseProg x options@Options {..} = logCompilerPass MinDetail currentPass $ do
  result <- evalStateT (runReaderT (nf x) options) declContext
  return result

normaliseExpr :: MonadCompile m => TypeCheckedExpr -> NormalisationOptions -> m TypeCheckedExpr
normaliseExpr x options@Options {..} = logCompilerPass MinDetail currentPass $ do
  result <- evalStateT (runReaderT (nf x) options) declContext
  return result

--------------------------------------------------------------------------------
-- Normalisation options

data NormalisationOptions = Options
  { declContext :: DeclCtx TypeCheckedExpr,
    boundContext :: BoundDBCtx,
    normaliseDeclApplications :: Identifier -> Bool,
    normaliseLambdaApplications :: Bool,
    normaliseBuiltin :: StandardBuiltin -> Bool
  }

fullNormalisationOptions :: NormalisationOptions
fullNormalisationOptions =
  Options
    { declContext = mempty,
      boundContext = mempty,
      normaliseDeclApplications = const True,
      normaliseLambdaApplications = True,
      normaliseBuiltin = const True
    }

-- | Constraint for the monad stack used by the normaliser.
type MonadNorm m =
  ( MonadCompile m,
    MonadReader NormalisationOptions m,
    MonadState (Map Identifier TypeCheckedExpr) m
  )

--------------------------------------------------------------------------------
-- Normalisation algorithm

-- | Class for the various normalisation functions.
class Norm vf where
  nf :: MonadNorm m => vf -> m vf

instance Norm (GenericDecl expr) => Norm (GenericProg expr) where
  nf = traverseDecls nf

instance Norm TypeCheckedDecl where
  nf decl = logCompilerPass MidDetail ("normalisation of" <+> declIdent) $ do
    result <- case decl of
      DefResource p r ident typ ->
        DefResource p r ident <$> nf typ
      DefFunction p ident isProperty typ expr -> do
        typ' <- nf typ
        expr' <- nf expr
        modify (Map.insert ident expr')
        return $ DefFunction p ident isProperty typ' expr'
      DefPostulate p ident typ ->
        DefPostulate p ident <$> nf typ
    logCompilerPassOutput (prettyFriendly result)
    return result
    where
      declIdent = quotePretty (identifierOf decl)

-- | This a horrible, horrible hack. Only introducing this very temporarily,
-- before normalisation is pushed all the way in.
instance Norm StandardGluedDecl where
  nf decl = logCompilerPass MidDetail ("normalisation of" <+> squotes declIdent) $
    case decl of
      DefResource p r ident typ ->
        DefResource p r ident <$> nf typ
      DefFunction p ident isProperty typ expr -> do
        typ' <- nf typ
        expr' <- nf expr
        modify (Map.insert ident (unnormalised expr'))
        return $ DefFunction p ident isProperty typ' expr'
      DefPostulate p ident typ ->
        DefPostulate p ident <$> nf typ
    where
      declIdent = pretty (identifierOf decl)

instance Norm StandardGluedExpr where
  nf = traverseUnnormalised nf

instance Norm TypeCheckedExpr where
  nf e = showExit e $ do
    e' <- showEntry e
    Options {..} <- ask
    case e' of
      Universe {} -> return e
      Hole {} -> return e
      Builtin {} -> return e
      Meta {} -> return e
      Lam p binder expr ->
        Lam p <$> nf binder <*> nf expr
      Pi p binder body ->
        Pi p <$> nf binder <*> nf body
      Ann _ expr _typ -> nf expr
      BoundVar {} -> return e
      FreeVar _ ident
        | normaliseDeclApplications ident -> do
            ctx <- get
            case Map.lookup ident ctx of
              Nothing -> return e
              Just x -> nf x
        | otherwise -> return e
      Let _ letValue _letBinder letBody -> do
        letValue' <- nf letValue
        letBody' <- nf letBody
        let letBodyWithSubstitution = substDBInto letValue' letBody'
        nf letBodyWithSubstitution
      App p fun args -> do
        nFun <- nf fun
        nArgs <- traverse nf args
        nfApp p nFun nArgs

instance Norm TypeCheckedBinder where
  nf = traverse nf

instance Norm TypeCheckedArg where
  nf = traverseNonInstanceArgExpr nf

--------------------------------------------------------------------------------
-- Application

nfApp :: MonadNorm m => Provenance -> TypeCheckedExpr -> NonEmpty TypeCheckedArg -> m TypeCheckedExpr
nfApp p fun args = do
  let (fun', args') = renormArgs fun args
  let e = App p fun' args'
  Options {..} <- ask
  case fun' of
    Lam {} | normaliseLambdaApplications -> nfAppLam p fun' (NonEmpty.toList args')
    Builtin _ b | normaliseBuiltin b -> nfBuiltin p b args'
    _ -> return e

nfAppLam :: MonadNorm m => Provenance -> TypeCheckedExpr -> [TypeCheckedArg] -> m TypeCheckedExpr
nfAppLam _ fun [] = nf fun
nfAppLam p (Lam _ _ body) (arg : args) = nfAppLam p (substDBInto (argExpr arg) body) args
nfAppLam p fun (arg : args) = nfApp p fun (arg :| args)

--------------------------------------------------------------------------------
-- Builtins

nfBuiltin :: forall m. MonadNorm m => Provenance -> StandardBuiltin -> NonEmpty TypeCheckedArg -> m TypeCheckedExpr
nfBuiltin p (CConstructor c) args =
  return $ App p (Builtin p (CConstructor c)) args
nfBuiltin p (CType (StandardTypeClassOp op)) args = do
  let originalExpr = BuiltinTypeClassOp p op args
  case nfTypeClassOp p op (NonEmpty.toList args) of
    Nothing -> return originalExpr
    Just res -> do
      (fn, newArgs) <- res
      nf (normApp p fn newArgs)
nfBuiltin p b args = do
  let e = App p (Builtin p b) args
  fromMaybe (return e) $ case e of
    -- Types
    TensorType _ tElem dims -> Just $ return $ nfTensor p tElem dims
    -- Binary relations
    EqualityExpr _ dom op (NonEmpty.reverse -> arg2 :| arg1 : _) -> Just $ return $ nfEq p dom op arg1 arg2
    OrderExpr _ dom op [arg1, arg2] -> Just $ return $ nfOrder p dom op arg1 arg2
    -- Boolean operations
    NotExpr _ [arg] -> Just $ return $ nfNot p arg
    AndExpr _ [arg1, arg2] -> Just $ return $ nfAnd p arg1 arg2
    OrExpr _ [arg1, arg2] -> Just $ return $ nfOr p arg1 arg2
    ImpliesExpr _ [arg1, arg2] -> Just $ return $ nfImplies p arg1 arg2
    IfExpr _ t [cond, e1, e2] -> Just $ return $ nfIf p t cond e1 e2
    -- Numeric operations
    NegExpr _ dom [arg] -> Just $ return $ nfNeg p dom arg
    AddExpr _ dom [arg1, arg2] -> Just $ return $ nfAdd p dom arg1 arg2
    SubExpr _ dom [arg1, arg2] -> Just $ return $ nfSub p dom arg1 arg2
    MulExpr _ dom [arg1, arg2] -> Just $ return $ nfMul p dom arg1 arg2
    DivExpr _ dom [arg1, arg2] -> Just $ return $ nfDiv p dom arg1 arg2
    -- Numeric conversion
    FromNatExpr _ n dom args' -> Just $ nfFromNat p n dom args'
    FromRatExpr _ dom [arg] -> Just $ nfFromRat dom arg
    -- Containers
    -- MapExpr _ tElem tRes [fn, cont] -> nfMap  p tElem tRes (argExpr fn) (argExpr cont)
    AtExpr _ tElem tDim [tensor, index] -> Just $ return $ nfAt p tElem tDim tensor index
    FoldVectorExpr _ tElem size tRes [op, unit, cont] ->
      Just $ nfFoldVector p tElem size tRes op unit cont
    -- Fall-through case
    _ -> Nothing

nfTypeClassOp ::
  MonadCompile m =>
  Provenance ->
  TypeClassOp ->
  [Arg binder var StandardBuiltin] ->
  Maybe (m (Expr binder var StandardBuiltin, NonEmpty (Arg binder var StandardBuiltin)))
nfTypeClassOp _p op args = do
  let (inst, remainingArgs) = findInstanceArg args
  case (inst, remainingArgs) of
    (Meta {}, _) -> Nothing
    (_, v : vs) -> do
      let (fn, args') = toHead inst
      Just $ return (fn, prependList args' (v :| vs))
    (_, []) ->
      Just $
        compilerDeveloperError $
          "Type class operation with no further arguments:" <+> pretty op

nfFoldVector ::
  MonadNorm m =>
  Provenance ->
  TypeCheckedExpr ->
  TypeCheckedExpr ->
  TypeCheckedExpr ->
  TypeCheckedArg ->
  TypeCheckedArg ->
  TypeCheckedArg ->
  m TypeCheckedExpr
nfFoldVector p tElem size tRes foldOp unit vector = case argExpr vector of
  VecLiteral _ _ xs -> do
    let combine x body = normApp p (argExpr foldOp) [x, ExplicitArg p body]
    nf $ foldr combine (argExpr unit) xs
  _ ->
    return $
      BuiltinFunctionExpr
        p
        (Fold FoldVector)
        ( ImplicitArg p tElem
            :| ImplicitArg p size
            : ImplicitArg p tRes
            : [foldOp, unit, vector]
        )

nfFromNat ::
  MonadNorm m =>
  Provenance ->
  Int ->
  FromNatDomain ->
  NonEmpty TypeCheckedArg ->
  m TypeCheckedExpr
nfFromNat p x dom _args = case dom of
  FromNatToIndex -> do
    -- This is a massive hack, we should really be normalising implicit args.
    return $ IndexLiteral p x
  FromNatToNat -> return $ NatLiteral p x
  FromNatToInt -> return $ IntLiteral p x
  FromNatToRat -> return $ RatLiteral p (fromIntegral x)

--------------------------------------------------------------------------------
-- Normalising equality

nfEq ::
  Provenance ->
  EqualityDomain ->
  EqualityOp ->
  TypeCheckedArg ->
  TypeCheckedArg ->
  TypeCheckedExpr
nfEq p dom eq e1 e2 = case (dom, argExpr e1, argExpr e2) of
  (EqIndex, IndexLiteral _ x, IndexLiteral _ y) -> BoolLiteral p (equalityOp eq x y)
  (EqNat, NatLiteral _ x, NatLiteral _ y) -> BoolLiteral p (equalityOp eq x y)
  (EqInt, IntLiteral _ x, IntLiteral _ y) -> BoolLiteral p (equalityOp eq x y)
  (EqRat, RatLiteral _ x, RatLiteral _ y) -> BoolLiteral p (equalityOp eq x y)
  _ -> EqualityExpr p dom eq [e1, e2]

--------------------------------------------------------------------------------
-- Normalising tensor types

nfTensor ::
  Provenance ->
  TypeCheckedType ->
  TypeCheckedExpr ->
  TypeCheckedType
nfTensor p tElem dims = case dims of
  NilExpr {} -> tElem
  AppConsExpr _ _ d ds -> VectorType p (nfTensor p tElem ds) d
  _ -> App p (FreeVar p TensorIdent) (ExplicitArg p <$> [tElem, dims])

--------------------------------------------------------------------------------
-- Normalising orders

nfOrder ::
  Provenance ->
  OrderDomain ->
  OrderOp ->
  TypeCheckedArg ->
  TypeCheckedArg ->
  TypeCheckedExpr
nfOrder p dom ord arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (OrderNat, NatLiteral _ x, NatLiteral _ y) -> BoolLiteral p (orderOp ord x y)
  (OrderIndex, IndexLiteral _ x, IndexLiteral _ y) -> BoolLiteral p (orderOp ord x y)
  (OrderInt, IntLiteral _ x, IntLiteral _ y) -> BoolLiteral p (orderOp ord x y)
  (OrderRat, RatLiteral _ x, RatLiteral _ y) -> BoolLiteral p (orderOp ord x y)
  _ -> OrderExpr p dom ord [arg1, arg2]

--------------------------------------------------------------------------------
-- Normalising boolean operations

nfNot :: Provenance -> TypeCheckedArg -> TypeCheckedExpr
nfNot p arg = case argExpr arg of
  BoolLiteral _ b -> BoolLiteral p (not b)
  _ -> NotExpr p [arg]

nfAnd :: Provenance -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfAnd p arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (TrueExpr _, _) -> argExpr arg2
  (FalseExpr _, _) -> FalseExpr p
  (_, TrueExpr _) -> argExpr arg1
  (_, FalseExpr _) -> FalseExpr p
  _ -> AndExpr p [arg1, arg2]

nfOr :: Provenance -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfOr p arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (TrueExpr _, _) -> TrueExpr p
  (FalseExpr _, _) -> argExpr arg2
  (_, TrueExpr _) -> TrueExpr p
  (_, FalseExpr _) -> argExpr arg1
  _ -> OrExpr p [arg1, arg2]

nfImplies :: Provenance -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfImplies p arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (TrueExpr _, _) -> argExpr arg2
  (FalseExpr _, _) -> TrueExpr p
  (_, TrueExpr _) -> TrueExpr p
  (_, FalseExpr _) -> NotExpr p [arg2]
  _ -> ImpliesExpr p [arg1, arg2]

nfIf :: Provenance -> TypeCheckedExpr -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfIf p t condition e1 e2 = case argExpr condition of
  TrueExpr _ -> argExpr e1
  FalseExpr _ -> argExpr e2
  _ -> IfExpr p t [condition, e1, e2]

-----------------------------------------------------------------------------
-- Normalising conversion

nfFromRat :: MonadCompile m => FromRatDomain -> TypeCheckedArg -> m TypeCheckedExpr
nfFromRat dom (ExplicitArg _ rat@RatLiteral {}) = case dom of
  FromRatToRat -> return rat
nfFromRat _ _ = unexpectedExprError "conversion FromRat" "non-Rat"

-----------------------------------------------------------------------------
-- Normalising numeric operations

nfNeg :: Provenance -> NegDomain -> TypeCheckedArg -> TypeCheckedExpr
nfNeg p dom e = case (dom, argExpr e) of
  (NegInt, IntLiteral _ x) -> IntLiteral p (-x)
  (NegRat, RatLiteral _ x) -> RatLiteral p (-x)
  _ -> NegExpr p dom [e]

nfAdd :: Provenance -> AddDomain -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfAdd p dom arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (AddNat, NatLiteral _ x, NatLiteral _ y) -> NatLiteral p (x + y)
  (AddInt, IntLiteral _ x, IntLiteral _ y) -> IntLiteral p (x + y)
  (AddRat, RatLiteral _ x, RatLiteral _ y) -> RatLiteral p (x + y)
  _ -> AddExpr p dom [arg1, arg2]

nfSub :: Provenance -> SubDomain -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfSub p dom arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (SubInt, IntLiteral _ x, IntLiteral _ y) -> IntLiteral p (x - y)
  (SubRat, RatLiteral _ x, RatLiteral _ y) -> RatLiteral p (x - y)
  _ -> SubExpr p dom [arg1, arg2]

nfMul :: Provenance -> MulDomain -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfMul p dom arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (MulNat, NatLiteral _ x, NatLiteral _ y) -> NatLiteral p (x * y)
  (MulInt, IntLiteral _ x, IntLiteral _ y) -> IntLiteral p (x * y)
  (MulRat, RatLiteral _ x, RatLiteral _ y) -> RatLiteral p (x * y)
  _ -> MulExpr p dom [arg1, arg2]

nfDiv :: Provenance -> DivDomain -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfDiv p dom arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (DivRat, RatLiteral _ x, RatLiteral _ y) -> RatLiteral p (x / y)
  _ -> DivExpr p dom [arg1, arg2]

-----------------------------------------------------------------------------
-- Normalising container operations

nfAt :: Provenance -> TypeCheckedExpr -> TypeCheckedExpr -> TypeCheckedArg -> TypeCheckedArg -> TypeCheckedExpr
nfAt p tElem tDim vector index = case (argExpr vector, argExpr index) of
  (VecLiteral _ _ es, IndexLiteral _ i) -> argExpr $ es !! fromIntegral i
  _ ->
    BuiltinFunctionExpr
      p
      At
      ( ImplicitArg p tElem
          :| ImplicitArg p tDim
          : [vector, index]
      )

--------------------------------------------------------------------------------
-- Debug functions

currentPass :: Doc ()
currentPass = "normalisation"

showEntry :: MonadNorm m => TypeCheckedExpr -> m TypeCheckedExpr
showEntry e = do
  logDebug MaxDetail ("norm-entry " <> prettyVerbose e)
  incrCallDepth
  return e

showExit :: MonadNorm m => TypeCheckedExpr -> m TypeCheckedExpr -> m TypeCheckedExpr
showExit old mNew = do
  new <- mNew
  decrCallDepth
  when (old /= new) $ do
    logDebug MaxDetail ("normalising" <+> prettyVerbose old)
  logDebug MaxDetail ("norm-exit " <+> prettyVerbose new)
  return new
