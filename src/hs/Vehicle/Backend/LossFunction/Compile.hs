module Vehicle.Backend.LossFunction.Compile
  ( LExpr
  , compile
  ) where

import Data.Aeson
import GHC.Generics (Generic)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)

import Vehicle.Prelude
import Vehicle.Compile.Error
import Vehicle.Resource.NeuralNetwork
import Vehicle.Language.AST qualified as V
import Vehicle.Compile.Prelude qualified as V
import Vehicle.Language.Print
import Vehicle.Language.AST.Name (HasName(nameOf))

--------------------------------------------------------------------------------
-- Declaration definition

data LDecl
  = DefFunction
    Symbol                   -- Bound function name.
    LExpr                    -- Bound function body.
  deriving (Eq, Show, Generic)

instance FromJSON LDecl
instance ToJSON LDecl
--------------------------------------------------------------------------------
-- Definitions


data Quantifier
  = All
  | Any
  deriving (Eq, Ord, Generic, Show)

instance FromJSON Quantifier
instance ToJSON Quantifier

newtype Domain = Domain ()
  deriving (Eq, Ord, Generic, Show)

instance FromJSON Domain
instance ToJSON Domain 

--definitoon of the LExpr - all expressions allowed in loss constraint

data LExpr
  = Negation LExpr
  | Constant Double
  | Min LExpr LExpr
  | Max LExpr LExpr
  | Subtraction LExpr LExpr
  | IndicatorFunction LExpr LExpr
  | Variable V.DBIndex
  | FreeVariable Symbol
  | NetworkApplication Symbol (NonEmpty LExpr)
  | Quantifier Quantifier Symbol Domain LExpr
  | At LExpr LExpr
  | TensorLiteral [LExpr]
  | Lambda Symbol LExpr
  deriving (Eq, Ord, Generic, Show)

instance FromJSON LExpr
instance ToJSON LExpr

--------------------------------------------------------------------------------
-- Compilation
-- the translation into the LExpr

compile :: MonadCompile m => NetworkCtx -> V.CheckedProg -> m [LDecl]
compile _ (V.Main ds) = catMaybes <$> traverse compileDecl ds

compileDecl :: MonadCompile m => V.CheckedDecl -> m (Maybe LDecl)
compileDecl V.DefResource{} = normalisationError currentPass "Resource declarations"
compileDecl (V.DefFunction _p i t expr) =
    if not $ V.isProperty t
      -- If it's not a property then we can discard it as all applications
      -- of it should have been normalised out by now.
      then return Nothing
      else do 
        expr' <- compileExpr expr
        return (Just (DefFunction (nameOf i) expr'))

currentPass :: Doc a
currentPass = "compilation to loss functions"

compileArg :: MonadCompile m => V.CheckedArg -> m LExpr
compileArg (V.Arg _ _ e) = compileExpr e

compileLiteral :: V.Literal -> Double
compileLiteral (V.LBool _) = developerError "LBool"
compileLiteral (V.LNat e ) = fromIntegral e
compileLiteral (V.LInt e ) = fromIntegral e
compileLiteral (V.LRat e ) = fromRational e

compileExpr :: MonadCompile m => V.CheckedExpr -> m LExpr
compileExpr e = showExit $ do
  e' <- showEntry e
  case e' of
    V.NotExpr  _ _ [e1]     -> Negation <$> compileArg e1
    V.AndExpr  _ _ [e1, e2] -> Min <$> compileArg e1 <*> compileArg e2
    V.OrExpr   _ _ [e1, e2] -> Max <$> compileArg e1 <*> compileArg e2
    V.ImplExpr _ _ [e1, e2] -> Max <$> (Negation <$> compileArg e1) <*> compileArg e2

    V.EqualityExpr V.Eq  _ _ _ [e1, e2] -> IndicatorFunction <$> compileArg e1 <*> compileArg e2
    V.EqualityExpr V.Neq _ _ _ [e1, e2] -> Negation <$> (IndicatorFunction <$> compileArg e1 <*> compileArg e2)
    V.OrderExpr    order _ _ _ [e1, e2] ->
      case order of
        V.Le -> Subtraction <$> compileArg e2 <*> compileArg e1
        V.Lt -> Negation <$> (Subtraction <$> compileArg e1 <*> compileArg e2)
        V.Ge -> Subtraction <$> compileArg e1 <*> compileArg e2
        V.Gt -> Negation <$> (Subtraction <$> compileArg e2 <*> compileArg e1)

    V.LiteralExpr _ _ l                   -> return $ Constant (compileLiteral l)
    V.App _ (V.Var _ (V.Free ident)) p    -> NetworkApplication (V.nameOf ident) <$> traverse compileArg p
    V.Var _ (V.Bound t)                   -> return (Variable t)
    V.Var _ (V.Free t)                    -> return (FreeVariable (V.nameOf t))
    V.QuantifierExpr q _ binder body      -> Quantifier (compileQuant q) (V.getBinderSymbol binder) (Domain ()) <$> compileExpr body
    V.AtExpr _ _ _ _ [xs, i]              -> At <$> compileArg xs <*> compileArg i
    V.LSeq _ _ xs                         -> TensorLiteral <$> traverse compileExpr xs
    V.Ann _ body _                        -> compileExpr body
    V.Lam _ binder body                   -> Lambda (V.getBinderSymbol binder) <$> compileExpr body
    V.Let _ val _ body                    -> compileExpr (V.substInto val body)
          

    V.Hole{}                              -> resolutionError "lossFunction" "Hole"
    V.Meta{}                              -> resolutionError "lossFunction" "Meta"  
    V.PrimDict{}                          -> typeError "lossFunction" "PrimDict"
    V.Pi{}                                -> typeError "lossFunction" "Pi"
    V.Type{}                              -> typeError "lossFunction" "Type"
    _                                     -> compilerDeveloperError $ unexpectedExprError currentPass (prettySimple e)
    

compileQuant :: V.Quantifier -> Quantifier
compileQuant V.All = All
compileQuant V.Any = Any

showEntry :: MonadCompile m => V.CheckedExpr -> m V.CheckedExpr
showEntry e = do
  logDebug MaxDetail ("loss-entry " <> prettySimple e)
  incrCallDepth
  return e

showExit :: MonadCompile m => m LExpr -> m LExpr
showExit mNew = do
  new <- mNew
  decrCallDepth
  logDebug MaxDetail ("loss-exit " <+> pretty (show new))
  return new
