module Vehicle.Backend.VNNLib.Compile
  ( compile
  ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.Maybe (catMaybes)
import Data.List.NonEmpty qualified as NonEmpty (toList)

import Vehicle.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.AST
import Vehicle.Compile.SupplyNames (supplyDBNames)
import Vehicle.Compile.Descope (runDescope)
import Vehicle.Compile.Prelude
import Vehicle.Compile.QuantifierAnalysis (checkQuantifiersAreHomogeneous)
import Vehicle.Compile.Normalise

import Vehicle.Backend.VNNLib.Core
import Vehicle.Compile.Normalise.NetworkApplications (convertNetworkAppsToMagicVars)
import Vehicle.Compile.Normalise.DNF (splitConjunctions)
import Vehicle.Backend.Prelude
import Vehicle.Resource.NeuralNetwork

compile :: MonadCompile m => NetworkCtx -> CheckedProg -> m [VNNLibProperty]
compile networkCtx prog = do
  logDebug "Beginning compilation to SMT"
  incrCallDepth
  result <- compileProg networkCtx prog
  decrCallDepth
  logDebug "Finished compilation to SMT\n"
  return result

supportedTypes :: [Builtin]
supportedTypes =
  [ NumericType Real
  , NumericType Rat
  ]

--------------------------------------------------------------------------------
-- Compilation

type MonadVNNLibProp m =
  ( MonadCompile m
  , MonadReader Identifier m
  )

compileProg :: MonadCompile m => NetworkCtx -> CheckedProg -> m [VNNLibProperty]
compileProg networkCtx (Main ds) = do
  results <- catMaybes <$> traverse (compileDecl networkCtx) ds
  if null results
    then throwError NoPropertiesFound
    else return results

compileDecl :: MonadCompile m => NetworkCtx -> CheckedDecl -> m (Maybe VNNLibProperty)
compileDecl networkCtx = \case
  DefFunction _ ident t e -> if not $ isProperty t
      then return Nothing
      else do
        let doc = compileProperty networkCtx ident e
        Just <$> doc

  DefResource _ r _ _ ->
    normalisationError currentPass (pretty r <+> "declarations")

compileProperty :: MonadCompile m => NetworkCtx -> Identifier -> CheckedExpr -> m VNNLibProperty
compileProperty networkCtx ident expr = flip runReaderT ident $ do
  logDebug $ "Beginning compilation of SMTLib property" <+> squotes (pretty ident)
  incrCallDepth
  let ann = annotationOf expr

  -- Check that we only have one type of quantifier in the property
  quantifier <- checkQuantifiersAreHomogeneous VNNLibBackend ident expr

  -- If the property is universally quantified then we need to negate the expression
  let (isPropertyNegated, possiblyNegatedExpr) =
        if quantifier == Any
          then (False, expr)
          else (True,  NotExpr ann Prop [ExplicitArg ann expr])

  -- Convert all applications of networks into magic variables
  (networklessExpr, metaNetwork) <-
    convertNetworkAppsToMagicVars VNNLib networkCtx quantifier possiblyNegatedExpr

  -- Normalise the expression to remove any implications, push the negations through
  -- and expand out any multiplication.
  normExpr <- normalise (Options
    { implicationsToDisjunctions = False
    , subtractionToAddition      = False
    , expandOutPolynomials       = False
    }) networklessExpr

  -- Switch from DeBruijn indices to names
  let descopedExpr = runDescope [] (supplyDBNames normExpr)

  -- Strip off the leading quantifiers
  (vars, quantifierFreeExpr) <- stripQuantifiers descopedExpr

  -- Separate out conjunctions into separate assertions
  let assertionExprs = splitConjunctions quantifierFreeExpr

  assertionDocs <- traverse compileAssertion assertionExprs
  let bodyDoc = vsep (map assertion assertionDocs)

  let doc = if null vars then bodyDoc else compileVars vars <> line <> line <> bodyDoc
  logDebug $ "Output:" <> align (line <> doc)

  decrCallDepth
  logDebug $ "Finished compilation of SMTLib property" <+> squotes (pretty ident)

  return $ VNNLibProperty
    { name        = nameOf ident
    , doc         = doc
    , vars        = vars
    , negated     = isPropertyNegated
    , metaNetwork = metaNetwork
    }

compileVars :: [VNNVar] -> Doc a
compileVars vars = vsep (map compileVar vars)
  where
    compileVar :: VNNVar -> Doc a
    compileVar (VNNVar name t) = parens ("declare-const" <+> pretty name <+> pretty t)

compileAssertion :: MonadVNNLibProp m => OutputExpr -> m (Doc b)
compileAssertion = \case
  Type _ _       -> typeError          currentPass "Type"
  Pi   _ann _ _  -> typeError          currentPass "Pi"
  Hole _p _      -> resolutionError    currentPass "Hole"
  Meta _p _      -> resolutionError    currentPass "Meta"
  Ann _ann _ _   -> normalisationError currentPass "Ann"
  Lam _ann _ _   -> normalisationError currentPass "Lam"
  LSeq _ann _ _  -> normalisationError currentPass "LSeq"
  PrimDict _ _tc -> visibilityError    currentPass "PrimDict"

  Builtin _ann op -> compileBuiltin op
  Literal _ann l  -> return $ compileLiteral l
  Var     _ann v  -> return $ pretty v

  QuantifierExpr q ann binder _ -> do
    ident <- ask
    throwError $ UnsupportedQuantifierPosition VNNLibBackend (provenanceOf ann) ident q (nameOf binder)

  App _ann fun args -> do
    funDoc  <- compileAssertion fun
    argDocs <- catMaybes <$> traverse compileArg (NonEmpty.toList args)
    return $ if null argDocs
      then funDoc
      else parens $ hsep (funDoc : argDocs)

  Let _ann bound binder body -> do
    let binderDoc = pretty (nameOf binder :: OutputBinding)
    boundDoc <- compileAssertion bound
    bodyDoc  <- compileAssertion body
    return $ parens $ "let" <+> parens (parens (binderDoc <+> boundDoc)) <+> bodyDoc

compileBuiltin :: MonadVNNLibProp m => Builtin -> m (Doc b)
compileBuiltin = \case
  BooleanType   t -> typeError currentPass $ pretty t
  NumericType   t -> typeError currentPass $ pretty t
  ContainerType t -> typeError currentPass $ pretty t
  Fin             -> typeError currentPass "Fin"
  TypeClass tc    -> typeError currentPass $ pretty tc

  QuantIn _ -> normalisationError currentPass "QuantIn"
  Cons      -> normalisationError currentPass $ pretty Cons
  At        -> normalisationError currentPass $ pretty At
  Map       -> normalisationError currentPass $ pretty Map
  Fold      -> normalisationError currentPass $ pretty Fold
  Quant q   -> normalisationError currentPass $ pretty (Quant q)

  If              -> return "ite"
  BooleanOp2 Impl -> return "=>"
  BooleanOp2 And  -> return "and"
  BooleanOp2 Or   -> return "or"
  Not             -> return "not"
  Equality Eq     -> return "=="
  Equality Neq    -> return "distinct"
  Order o         -> return $ pretty o
  NumericOp2 Mul  -> return "*"
  NumericOp2 Div  -> return "/"
  NumericOp2 Add  -> return "+"
  NumericOp2 Sub  -> return "-"
  Neg             -> return "-"

compileArg :: MonadVNNLibProp m => OutputArg -> m (Maybe (Doc a))
compileArg arg = if visibilityOf arg == Explicit
  then Just <$> compileAssertion (argExpr arg)
  else return Nothing

compileLiteral :: Literal -> Doc a
compileLiteral (LBool True)  = "true"
compileLiteral (LBool False) = "false"
compileLiteral (LNat n)      = pretty n
compileLiteral (LInt i)      = pretty i
compileLiteral (LRat x)      = pretty x

stripQuantifiers :: MonadVNNLibProp m
                 => OutputExpr
                 -> m ([VNNVar], OutputExpr)
stripQuantifiers (QuantifierExpr _ ann binder body) = do
  let varSymbol = nameOf binder
  varType <- getVarType ann varSymbol (typeOf binder)
  let var = VNNVar varSymbol varType
  (vars, body') <- stripQuantifiers body
  return (var : vars, body')
stripQuantifiers e = return ([], e)

getVarType :: MonadVNNLibProp m => OutputAnn -> Symbol -> OutputExpr -> m VNNVarType
getVarType _   _ (RealType _) = return VReal
getVarType _   _ (RatType  _) = return VReal -- See Issue #46
getVarType ann s t            = do
  ident <- ask
  throwError $ UnsupportedVariableType VNNLibBackend (provenanceOf ann) ident s t supportedTypes


assertion :: Doc a -> Doc a
assertion p = parens ("assert" <+> p)

currentPass :: Doc a
currentPass = "compilation to VNNLib"