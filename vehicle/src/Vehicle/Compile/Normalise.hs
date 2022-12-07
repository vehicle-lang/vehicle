module Vehicle.Compile.Normalise
  ( NormalisationOptions(..)
  , fullNormalisationOptions
  , normalise
  , nfTensor
  , nfTypeClassOp
  ) where

import Control.Monad (when)
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Control.Monad.State (MonadState (..), evalStateT, modify)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty (head, toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised (GluedDecl, GluedExpr (..), traverseUnnormalised)
import Vehicle.Libraries.StandardLibrary.Names

-- |Run a function in 'MonadNorm'.
normalise :: (MonadCompile m, Norm a, PrettyWith ('Named ('As 'External)) ([DBBinding], a))
          => a
          -> NormalisationOptions
          -> m a
normalise x options@Options{..} = logCompilerPass MinDetail currentPass $ do
  result <- evalStateT (runReaderT (nf x) options) declContext
  -- logCompilerPassOutput (prettyFriendlyDB boundContext result)
  return result

--------------------------------------------------------------------------------
-- Normalisation options

data NormalisationOptions = Options
  { declContext                 :: DeclCtx CheckedExpr
  , boundContext                :: [DBBinding]
  , normaliseDeclApplications   :: Bool
  , normaliseLambdaApplications :: Bool
  , normaliseStdLibApplications :: Bool
  , normaliseBuiltin            :: Builtin -> Bool
  }

fullNormalisationOptions :: NormalisationOptions
fullNormalisationOptions = Options
  { declContext                 = mempty
  , boundContext                = mempty
  , normaliseDeclApplications   = True
  , normaliseLambdaApplications = True
  , normaliseStdLibApplications = True
  , normaliseBuiltin            = const True
  }

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm m =
  ( MonadCompile m
  , MonadReader NormalisationOptions m
  , MonadState (Map Identifier CheckedExpr) m
  )

--------------------------------------------------------------------------------
-- Normalisation algorithm

-- | Class for the various normalisation functions.
class Norm vf where
  nf :: MonadNorm m => vf -> m vf

instance Norm (GenericDecl expr) => Norm (GenericProg expr) where
  nf = traverseDecls nf

instance Norm CheckedDecl where
  nf decl = logCompilerPass MidDetail ("normalisation of" <+> declIdent) $ do
    result <- case decl of
      DefResource p r ident typ ->
        DefResource p r ident <$> nf typ

      DefFunction p ident isProperty typ expr -> do
        typ'  <- nf typ
        expr' <- nf expr
        modify (Map.insert ident expr')
        return $ DefFunction p ident isProperty typ' expr'

      DefPostulate p ident typ ->
        DefPostulate p ident <$> nf typ
    logCompilerPassOutput (prettyFriendlyDBClosed result)
    return result
    where declIdent = quotePretty (identifierOf decl)

-- | This a horrible, horrible hack. Only introducing this very temporarily,
-- before normalisation is pushed all the way in.
instance Norm GluedDecl where
  nf decl = logCompilerPass MidDetail ("normalisation of" <+> squotes declIdent) $
    case decl of
      DefResource p r ident typ ->
        DefResource p r ident <$> nf typ

      DefFunction p ident isProperty typ expr -> do
        typ'  <- nf typ
        expr' <- nf expr
        modify (Map.insert ident (unnormalised expr'))
        return $ DefFunction p ident isProperty typ' expr'

      DefPostulate p ident typ ->
        DefPostulate p ident <$> nf typ

    where declIdent = pretty (identifierOf decl)

instance Norm GluedExpr where
  nf = traverseUnnormalised nf

instance Norm CheckedExpr where
  nf e = showExit e $ do
    e' <- showEntry e
    Options{..} <- ask
    case e' of
      Universe{}  -> return e
      Hole{}      -> return e
      Literal{}   -> return e
      Builtin{}   -> return e
      Meta{}      -> return e

      LVec p xs -> LVec p <$> traverse nf xs

      Lam p binder expr ->
        Lam p <$> nf binder <*> nf expr

      Pi p binder body ->
        Pi p <$> nf binder <*> nf body

      Ann _ expr _typ -> nf expr

      Var _ v -> case v of
        Bound{} -> return e
        Free ident
          | normaliseDeclApplications -> do
            ctx <- get
            case Map.lookup ident ctx of
              Nothing -> return e
              Just x  -> nf x
          | otherwise -> return e

      Let _ letValue _letBinder letBody -> do
        letValue' <- nf letValue
        letBody'  <- nf letBody
        let letBodyWithSubstitution = substDBInto letValue' letBody'
        nf letBodyWithSubstitution

      App p fun args -> do
        nFun  <- nf fun
        nArgs <- traverse nf args
        nfApp p nFun nArgs

instance Norm CheckedBinder where
  nf = traverse nf

instance Norm CheckedArg where
  nf = traverseNonInstanceArgExpr nf

--------------------------------------------------------------------------------
-- Application

nfApp :: MonadNorm m => Provenance -> CheckedExpr -> NonEmpty CheckedArg -> m CheckedExpr
nfApp p fun args = do
  let (fun', args') = renormArgs fun args
  let e = App p fun' args'
  Options{..} <- ask
  case fun' of
    Lam{}       | normaliseLambdaApplications -> nfAppLam p fun' (NonEmpty.toList args')
    Builtin _ b | normaliseBuiltin b          -> nfBuiltin p b args'
    FreeVar _ i | normaliseStdLibApplications -> case findStdLibFunction (nameOf i) of
      Nothing -> return e
      Just f  -> nfStdLibFn p f args'
    _ -> return e

nfAppLam :: MonadNorm m => Provenance -> CheckedExpr -> [CheckedArg] -> m CheckedExpr
nfAppLam _ fun            []           = nf fun
nfAppLam p (Lam _ _ body) (arg : args) = nfAppLam p (substDBInto (argExpr arg) body) args
nfAppLam p fun            (arg : args) = nfApp p fun (arg :| args)

nfStdLibFn :: MonadNorm m
           => Provenance
           -> StdLibFunction
           -> NonEmpty CheckedArg
           -> m CheckedExpr
nfStdLibFn p f allArgs = do
  let e = App p (FreeVar p (identifierOf f)) allArgs
  fromMaybe (return e) $ case embedStdLib f allArgs of
    Nothing -> Nothing
    Just res -> case res of
      EqualsBool    args -> fmap return (nfEqualsBool Eq  p args)
      NotEqualsBool args -> fmap return (nfEqualsBool Neq p args)

      ExistsBool{} -> Nothing
      ForallBool{} -> Nothing

      EqualsVector    tElem size recFn args -> nfEqualsVector Eq  p tElem size recFn args
      NotEqualsVector tElem size recFn args -> nfEqualsVector Neq p tElem size recFn args
      AddVector       tElem size recFn args -> nfAddVector p tElem size recFn args
      SubVector       tElem size recFn args -> nfSubVector p tElem size recFn args

      ForallVector    tElem s recFn binder body -> case s of
        NatLiteral _ size -> Just $ nfQuantifierVector p tElem size binder body recFn
        _                 -> Nothing

      ExistsVector    tElem s recFn binder body -> case s of
        NatLiteral _ size -> Just $ nfQuantifierVector p tElem size binder body recFn
        _                 -> Nothing

      ExistsIndex size lam -> Just $ nfQuantifierIndex p Exists size lam
      ForallIndex size lam -> Just $ nfQuantifierIndex p Forall size lam

--------------------------------------------------------------------------------
-- Builtins

nfBuiltin :: forall m. MonadNorm m => Provenance -> Builtin -> NonEmpty CheckedArg -> m CheckedExpr
nfBuiltin p (Constructor c) args =
  return $ App p (Builtin p (Constructor c)) args

nfBuiltin p (TypeClassOp op) args = do
  let originalExpr = App p (Builtin p (TypeClassOp op)) args
  case nfTypeClassOp p op (NonEmpty.toList args) of
    Nothing  -> return originalExpr
    Just res -> do
      (fn, newArgs) <- res
      nfNewArgs <- traverse nf newArgs
      nfApp p fn nfNewArgs

nfBuiltin p b                args = do
  let e = App p (Builtin p b) args
  fromMaybe (return e) $ case e of
    -- Types
    TensorType _ tElem dims -> Just $ return $ nfTensor p tElem dims

    -- Binary relations
    EqualityExpr _ dom op [arg1, arg2] -> Just $ return $ nfEq    p dom op arg1 arg2
    OrderExpr    _ dom op [arg1, arg2] -> Just $ return $ nfOrder p dom op arg1 arg2

    -- Boolean operations
    NotExpr     _ [arg]          -> Just $ return $ nfNot     p arg
    AndExpr     _ [arg1, arg2]   -> Just $ return $ nfAnd     p arg1 arg2
    OrExpr      _ [arg1, arg2]   -> Just $ return $ nfOr      p arg1 arg2
    ImpliesExpr _ [arg1, arg2]   -> Just $ return $ nfImplies p arg1 arg2
    IfExpr    _ t [cond, e1, e2] -> Just $ return $ nfIf      p t cond e1 e2

    -- Numeric operations
    NegExpr _ dom [arg]        -> Just $ return $ nfNeg p dom arg
    AddExpr _ dom [arg1, arg2] -> Just $ return $ nfAdd p dom arg1 arg2
    SubExpr _ dom [arg1, arg2] -> Just $ return $ nfSub p dom arg1 arg2
    MulExpr _ dom [arg1, arg2] -> Just $ return $ nfMul p dom arg1 arg2
    DivExpr _ dom [arg1, arg2] -> Just $ return $ nfDiv p dom arg1 arg2

    -- Numeric conversion
    FromNatExpr _ n dom args' -> Just $ nfFromNat p n dom args'
    FromRatExpr _ dom   [arg] -> Just $ nfFromRat dom arg
    FromVecExpr _ _ dom [arg] -> Just $ nfFromVec dom arg

    -- Containers
    -- MapExpr _ tElem tRes [fn, cont] -> nfMap  p tElem tRes (argExpr fn) (argExpr cont)
    AtExpr _ tElem tDim [tensor, index]          -> Just $ return $ nfAt p tElem tDim tensor index

    ForeachExpr _ tElem s body -> case s of
      NatLiteral _ size -> Just $ nfForeach p tElem size body
      _                 -> Nothing

    MapVectorExpr _ tFrom tTo size [fn, vector] ->
      Just $ nfMapVector p tFrom tTo size fn vector

    FoldVectorExpr _ tElem size tRes [op, unit, cont] ->
      Just $ nfFoldVector p tElem size tRes op unit cont

    -- Fall-through case
    _ -> Nothing

nfTypeClassOp :: MonadCompile m
              => Provenance
              -> TypeClassOp
              -> [Arg binder var]
              -> Maybe (m (Expr binder var, NonEmpty (Arg binder var)))
nfTypeClassOp _p op args = do
  let (inst, remainingArgs) = findInstanceArg args
  case (inst, remainingArgs) of
    (Meta{}, _)  -> Nothing
    (_, v : vs) -> do
      let (fn, args') = toHead inst
      Just $ return (fn, prependList args' (v :| vs))
    (_     , []) -> Just $ compilerDeveloperError $
      "Type class operation with no further arguments:" <+> pretty op

--------------------------------------------------------------------------------
-- Normalising quantification over types

nfForeach :: MonadNorm m
          => Provenance
          -> CheckedType
          -> Int
          -> CheckedExpr
          -> m CheckedExpr
nfForeach p resultType size lambda = do
  let fn i = nfAppLam p lambda [ExplicitArg p (IndexLiteral p size i)]
  VecLiteral p resultType <$> traverse fn [0 .. (size-1 :: Int)]


nfFoldVector :: MonadNorm m
             => Provenance
             -> CheckedExpr
             -> CheckedExpr
             -> CheckedExpr
             -> CheckedArg
             -> CheckedArg
             -> CheckedArg
             -> m CheckedExpr
nfFoldVector p tElem size tRes foldOp unit vector = case argExpr vector of
  AnnVecLiteral _ _ xs -> do
    let combine x body = normApp p (argExpr foldOp) (ExplicitArg p <$> [x, body])
    nf $ foldr combine (argExpr unit) xs
  _ -> return $ FoldVectorExpr p tElem size tRes [foldOp, unit, vector]

-----------------------------------------------------------------------------
-- Normalising quantifiers

-- If we're quantifying over a tensor, instead quantify over each individual
-- element, and then substitute in a LVec construct with those elements in.
nfQuantifierVector :: MonadNorm m
                   => Provenance
                   -> CheckedExpr
                   -> Int
                   -> CheckedBinder
                   -> CheckedExpr
                   -> CheckedExpr
                   -> m CheckedExpr
nfQuantifierVector p tElem size binder body recFn = do
  -- Use the list monad to create a nested list of all possible indices into the tensor
  let allIndices = [0..size-1]

  -- Generate the corresponding names from the indices
  let allNames   = map (mkNameWithIndices (getBinderName binder)) (reverse allIndices)

  -- Generate a list of variables, one for each index
  let allExprs   = map (\i -> Var p (Bound i)) (reverse allIndices)
  -- Construct the corresponding nested tensor expression
  let tensor     = VecLiteral p tElem allExprs
  -- We're introducing `tensorSize` new binder so lift the indices in the body accordingly
  let body1      = liftDBIndices size body
  -- Substitute throught the tensor expression for the old top-level binder
  body2 <- nf $ substDBIntoAtLevel size tensor body1

  let mkBinder name = Binder p (binderForm binder) Explicit Relevant name tElem
  let mkQuantifier e name = App p recFn [ExplicitArg p (Lam p (mkBinder name) e)]

  -- Generate a expression prepended with `tensorSize` quantifiers
  return $ foldl mkQuantifier body2 (map Just allNames)

nfQuantifierIndex :: MonadNorm m
                  => Provenance
                  -> Quantifier
                  -> Int
                  -> CheckedExpr
                  -> m CheckedExpr
nfQuantifierIndex p q size lam = do
  let indexType = ConcreteIndexType p size
  let cont  = VecLiteral p indexType (fmap (IndexLiteral p size) [0 .. size-1])
  nfQuantifierInVector p q indexType (NatLiteral p size) lam cont

-- | Elaborate quantification over the members of a container type.
-- Expands e.g. `forAll x in vector . y` to `fold and true (map (\x -> y) vector)`
nfQuantifierInVector :: MonadNorm m
                     => Provenance
                     -> Quantifier
                     -> CheckedExpr
                     -> CheckedExpr
                     -> CheckedExpr
                     -> CheckedExpr
                     -> m CheckedExpr
nfQuantifierInVector p q tElem size lam container = do
  let tTo   = BoolType p
  let mappedContainer = MapVectorExpr p tElem tTo size (ExplicitArg p <$> [lam, container])

  let ident = Identifier StdLib $ if q == Forall then "bigAnd" else "bigOr"
  nf $ bigOp p ident size mappedContainer

nfEqualsBool :: EqualityOp -> Provenance -> [CheckedArg] -> Maybe CheckedExpr
nfEqualsBool op p args@[arg1, arg2] = case op of
  Neq -> nfNot p . ExplicitArg p <$> nfEqualsBool Neq p args
  Eq  -> do
    let bothTrue  = AndExpr p [arg1, arg2]
    let bothFalse = AndExpr p (ExplicitArg p . nfNot p <$> [arg1, arg2])
    Just $ OrExpr p (ExplicitArg p <$> [bothTrue, bothFalse])
nfEqualsBool _ _ _ = Nothing

nfEqualsVector :: MonadNorm m
               => EqualityOp
               -> Provenance
               -> CheckedExpr
               -> CheckedExpr
               -> CheckedExpr
               -> [CheckedArg]
               -> Maybe (m CheckedExpr)
nfEqualsVector op p tElem size recFn args = case args of
  [ExplicitArg _ xs, ExplicitArg _ ys] -> Just $ do
    equalitiesSeq <- nf $ zipWithVector p tElem tElem (BoolType p) size recFn xs ys
    let ident = Identifier StdLib $ if op == Eq then "bigAnd" else "bigOr"
    nf $ bigOp p ident size equalitiesSeq
  _ -> Nothing

nfAddVector :: MonadNorm m
            => Provenance
            -> CheckedExpr
            -> CheckedExpr
            -> CheckedExpr
            -> [CheckedArg]
            -> Maybe (m CheckedExpr)
nfAddVector p tElem size recFn args = case args of
  [ExplicitArg _ xs, ExplicitArg _ ys] -> do
    Just $ nf $ zipWithVector p tElem tElem tElem size recFn xs ys
  _ -> Nothing

nfSubVector :: MonadNorm m
            => Provenance
            -> CheckedExpr
            -> CheckedExpr
            -> CheckedExpr
            -> [CheckedArg]
            -> Maybe (m CheckedExpr)
nfSubVector p tElem size recFn args = case args of
  [ExplicitArg _ xs, ExplicitArg _ ys] -> do
    Just $ nf $ zipWithVector p tElem tElem tElem size recFn xs ys
  _ -> Nothing

nfMapVector :: MonadNorm m
            => Provenance
            -> CheckedExpr
            -> CheckedExpr
            -> CheckedExpr
            -> CheckedArg
            -> CheckedArg
            -> m CheckedExpr
nfMapVector p tFrom tTo size fun vector =
  case argExpr vector of
    AnnVecLiteral _ _ xs -> do
      let appFun x = App p (argExpr fun) [ExplicitArg p x]
      return $ VecLiteral p tTo (fmap appFun xs)
    _                 ->  return $ MapVectorExpr p tFrom tTo size [fun, vector]

nfFromNat :: MonadNorm m
          => Provenance
          -> Int
          -> FromNatDomain
          -> NonEmpty CheckedArg
          -> m CheckedExpr
nfFromNat p x dom args = case (dom, argExpr (NonEmpty.head args)) of
  (FromNatToIndex, size) -> do
    -- This is a massive hack, we should really be normalising implicit args.
    nSize <- nf size
    case nSize of
      NatLiteral _ n -> return $ IndexLiteral p n x
      _              -> unexpectedExprError currentPass "non-NatLiteral index"
  (FromNatToNat, _)       -> return $ NatLiteral p x
  (FromNatToInt, _)       -> return $ IntLiteral p x
  (FromNatToRat, _)       -> return $ RatLiteral p (fromIntegral x)

--------------------------------------------------------------------------------
-- Debug functions

currentPass :: Doc ()
currentPass = "normalisation"

showEntry :: MonadNorm m => CheckedExpr -> m CheckedExpr
showEntry e = do
  logDebug MaxDetail ("norm-entry " <> prettySimple e)
  incrCallDepth
  return e

showExit :: MonadNorm m => CheckedExpr -> m CheckedExpr -> m CheckedExpr
showExit old mNew = do
  new <- mNew
  decrCallDepth
  when (old /= new) $
    logDebug MaxDetail ("normalising" <+> prettySimple old)
  logDebug MaxDetail ("norm-exit " <+> prettySimple new)
  return new
