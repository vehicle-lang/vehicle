module Vehicle.Compile.Normalise
  ( NormalisationOptions(..)
  , fullNormalisationOptions
  , noNormalisationOptions
  , normalise
  , nfTensor
  ) where

import Control.Monad (when)
import Control.Monad.State (MonadState(..), evalStateT, modify)
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty (toList)

import Vehicle.Language.Print
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Core
import Vehicle.Language.StandardLibrary.Names

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
  { implicationsToDisjunctions  :: Bool
  , expandOutPolynomials        :: Bool
  , declContext                 :: DeclCtx CheckedExpr
  , boundContext                :: [DBBinding]
  , normaliseAnnotations        :: Bool
  , normaliseDeclApplications   :: Bool
  , normaliseLambdaApplications :: Bool
  , normaliseLetBindings        :: Bool
  , normaliseStdLibApplications :: Bool
  , normaliseBuiltin            :: Builtin -> Bool
  , normaliseWeakly             :: Bool
  }

fullNormalisationOptions :: NormalisationOptions
fullNormalisationOptions = Options
  { implicationsToDisjunctions  = False
  , expandOutPolynomials        = False
  , declContext                 = mempty
  , boundContext                = mempty
  , normaliseDeclApplications   = True
  , normaliseLambdaApplications = True
  , normaliseLetBindings        = True
  , normaliseAnnotations        = True
  , normaliseStdLibApplications = True
  , normaliseBuiltin            = const True
  , normaliseWeakly             = False
  }

noNormalisationOptions :: NormalisationOptions
noNormalisationOptions = Options
  { implicationsToDisjunctions  = False
  , expandOutPolynomials        = False
  , declContext                 = mempty
  , boundContext                = mempty
  , normaliseDeclApplications   = False
  , normaliseLambdaApplications = False
  , normaliseLetBindings        = False
  , normaliseAnnotations        = False
  , normaliseStdLibApplications = False
  , normaliseBuiltin            = const False
  , normaliseWeakly             = False
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

instance Norm CheckedProg where
  nf (Main decls) = Main <$> traverse nf decls

instance Norm CheckedDecl where
  nf decl = logCompilerPass MaxDetail ("normalisation of" <+> squotes declIdent) $
    case decl of
      DefResource ann r ident typ ->
        DefResource ann r ident <$> nf typ

      DefFunction ann u ident typ expr -> do
        typ'  <- nf typ
        expr' <- nf expr
        modify (Map.insert ident expr')
        return $ DefFunction ann u ident typ' expr'

      DefPostulate ann ident typ ->
        DefPostulate ann ident <$> nf typ

    where declIdent = pretty (identifierOf decl)

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

      Ann p expr typ
        | normaliseAnnotations -> nf expr
        | otherwise            -> Ann p <$> nf expr <*> nf typ

      Var _ (Bound _) ->
        return e

      Var _ (Free ident)
        | normaliseDeclApplications -> do
          ctx <- get
          case Map.lookup ident ctx of
            Nothing -> return e
            Just v  -> nf v
        | otherwise                 -> return e

      Let p letValue letBinder letBody -> do
        letValue' <- nf letValue
        letBody'  <- nf letBody
        if normaliseLetBindings then do
          let letBodyWithSubstitution = substInto letValue' letBody'
          nf letBodyWithSubstitution
        else do
          letBinder' <- nf letBinder
          return $ Let p letValue' letBinder' letBody'

      App p fun args -> do
        nFun  <- nf fun
        nArgs <- if normaliseWeakly
          then return args
          -- Remove the below in future if we actually have computational
          -- behaviour in implicit/instance arguments
          else traverse (traverseExplicitArgExpr nf) args

        nfApp p nFun nArgs

instance Norm CheckedBinder where
  nf = traverseBinderType nf

instance Norm CheckedArg where
  nf (ExplicitArg ann e) = ExplicitArg ann <$> nf e
  nf arg@Arg{}           = return arg

--------------------------------------------------------------------------------
-- Application

nfApp :: MonadNorm m => Provenance -> CheckedExpr -> NonEmpty CheckedArg -> m CheckedExpr
nfApp p fun args = do
  let e = App p fun args
  Options{..} <- ask
  case fun of
    Lam{}       | normaliseLambdaApplications -> nfAppLam p fun (NonEmpty.toList args)
    Builtin _ b | normaliseBuiltin b          -> nfBuiltin p b args
    FreeVar _ i | normaliseStdLibApplications
                  && isStdLibFunction i       -> nfStdLibFn p i args
    _                                         -> return e

nfAppLam :: MonadNorm m => Provenance -> CheckedExpr -> [CheckedArg] -> m CheckedExpr
nfAppLam _ fun            []           = nf fun
nfAppLam p (Lam _ _ body) (arg : args) = nfAppLam p (substInto (argExpr arg) body) args
nfAppLam p fun            (arg : args) = nfApp p fun (arg :| args)

nfTypeClassOp :: MonadNorm m => Provenance -> TypeClassOp -> NonEmpty CheckedArg -> m CheckedExpr
nfTypeClassOp p op args = do
  let originalExpr = App p (Builtin p (TypeClassOp op)) args
  let (inst, remainingArgs) = findInstanceArg (NonEmpty.toList args)

  case (inst, remainingArgs) of
    (Meta{}, _)  -> return originalExpr
    (_, v : vs) -> do
      let (fn, args') = toHead inst
      nfApp p fn (prependList args' (v :| vs))
    (_     , []) -> compilerDeveloperError $
      "Type class operation with no further arguments:" <+> prettyVerbose originalExpr

nfStdLibFn :: MonadNorm m
           => Provenance
           -> Identifier
           -> NonEmpty CheckedArg
           -> m CheckedExpr
nfStdLibFn p ident args = do
  let e = App p (FreeVar p ident) args
  fromMaybe (return e) $ case args of
    -- Quantification
    [ImplicitArg _ tElem, ImplicitArg _ (NatLiteral _ size), InstanceArg _ recFn, ExplicitArg _ (Lam _ binder body)]
      | ident == StdForallVector || ident == StdExistsVector ->
        Just $ nfQuantifierVector p tElem size binder body recFn

    [ImplicitArg _ (NatLiteral _ size), ExplicitArg _ lam]
      | ident == StdForallIndex || ident == StdExistsIndex ->
        Just $ nfQuantifierIndex p ident size lam

    -- Equality
    [ImplicitArg _ tElem, ImplicitArg _ size, InstanceArg _ recFn, arg1, arg2]
      | ident == StdEqualsVector || ident == StdNotEqualsVector ->
        Just $ nfEqualsVector p ident tElem size recFn arg1 arg2

    -- Addition
    [ImplicitArg _ tElem1, ImplicitArg _ tElem2, ImplicitArg _ tElem3, ImplicitArg _ size, InstanceArg _ recFn, arg1, arg2]
      | ident == StdAddVector || ident == StdSubVector ->
        Just $ return $ zipWithVector p tElem1 tElem2 tElem3 size recFn arg1 arg2

    _ -> Nothing

--------------------------------------------------------------------------------
-- Builtins

nfBuiltin :: MonadNorm m => Provenance -> Builtin -> NonEmpty CheckedArg -> m CheckedExpr
nfBuiltin p (TypeClassOp op) args = nfTypeClassOp p op args
nfBuiltin p b                args = do
  let e = App p (Builtin p b) args
  Options{..} <- ask
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
    ImpliesExpr _ [arg1, arg2]   -> Just $ return $ nfImplies p arg1 arg2 implicationsToDisjunctions
    IfExpr    _ t [cond, e1, e2] -> Just $ return $ nfIf      p t cond e1 e2

    -- Numeric operations
    (NegExpr _ dom [arg])        -> Just $ return $ nfNeg p dom arg
    (AddExpr _ dom [arg1, arg2]) -> Just $ return $ nfAdd p dom arg1 arg2
    (SubExpr _ dom [arg1, arg2]) -> Just $ return $ nfSub expandOutPolynomials p dom arg1 arg2
    (MulExpr _ dom [arg1, arg2]) -> Just $ return $ nfMul expandOutPolynomials p dom arg1 arg2
    (DivExpr _ dom [arg1, arg2]) -> Just $ return $ nfDiv p dom arg1 arg2

    -- Numeric conversion
    (FromNatExpr _ n dom args') -> Just $ nfFromNat p n dom args'
    (FromRatExpr _ dom   [arg]) -> Just $ nfFromRat dom arg
    (FromVecExpr _ _ dom [arg]) -> Just $ nfFromVec dom arg

    -- Containers
    -- MapExpr _ tElem tRes [fn, cont] -> nfMap  p tElem tRes (argExpr fn) (argExpr cont)
    AtExpr _ tElem tDim [tensor, index]          -> Just $ return $ nfAt p tElem tDim tensor index
    ForeachExpr _ tElem (NatLiteral _ size) body -> Just $ nfForeach p tElem size body

    MapVectorExpr _ tFrom tTo size [fn, vector] ->
      Just $ nfMapVector p tFrom tTo size fn vector

    FoldVectorExpr _ tElem size tRes [op, unit, cont] ->
      Just $ nfFoldVector p tElem size tRes op unit cont

    -- Fall-through case
    _ -> Nothing

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
  let allNames   = map (mkNameWithIndices (getBinderSymbol binder)) (reverse allIndices)

  -- Generate a list of variables, one for each index
  let allExprs   = map (\i -> Var p (Bound i)) (reverse allIndices)
  -- Construct the corresponding nested tensor expression
  let tensor     = VecLiteral p tElem allExprs
  -- We're introducing `tensorSize` new binder so lift the indices in the body accordingly
  let body1      = liftFreeDBIndices size body
  -- Substitute throught the tensor expression for the old top-level binder
  body2 <- nf $ substIntoAtLevel size tensor body1

  let mkQuantifier e name = App p recFn [ExplicitArg p (Lam p (ExplicitBinder p name tElem) e)]

  -- Generate a expression prepended with `tensorSize` quantifiers
  return $ foldl mkQuantifier body2 (map Just allNames)

nfQuantifierIndex :: MonadNorm m
                  => Provenance
                  -> Identifier
                  -> Int
                  -> CheckedExpr
                  -> m CheckedExpr
nfQuantifierIndex p ident size lam = do
  let indexType = ConcreteIndexType p size
  let cont  = VecLiteral p indexType (fmap (IndexLiteral p size) [0 .. size-1])
  let q = if ident == StdForallIndex then Forall else Exists
  nfQuantifierInVector p q indexType (NatLiteral p size) lam cont

-- | Elaborate quantification over the members of a container type.
-- Expands e.g. `forall x in vector . y` to `fold and true (map (\x -> y) vector)`
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

  let ops = case q of
        Forall -> (True, And)
        Exists -> (False, Or)

  nf $ bigOp p ops size mappedContainer

nfEqualsVector :: MonadNorm m
               => Provenance
               -> Identifier
               -> CheckedExpr
               -> CheckedExpr
               -> CheckedExpr
               -> CheckedArg
               -> CheckedArg
               -> m CheckedExpr
nfEqualsVector p ident tElem size recFn xs ys = do
  let equalitiesSeq  = zipWithVector p tElem tElem (BoolType p) size recFn xs ys
  let ops = if ident == StdEqualsVector then (True, And) else (False, Or)
  nf $ bigOp p ops size equalitiesSeq

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