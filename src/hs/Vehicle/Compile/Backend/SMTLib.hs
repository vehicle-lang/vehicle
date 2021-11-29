module Vehicle.Compile.Backend.SMTLib
  ( compileToSMTLib
  , compileProp
  , SMTDoc(..)
  , SMTVar(..)
  , SMTLibError(..)
  -- VNNLib
  , InputOrOutput(..)
  , UnsupportedNetworkType(..)
  , supportedTypes
  ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.Maybe (catMaybes)
import Data.List.NonEmpty qualified as NonEmpty (toList)

import Vehicle.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.AST
import Vehicle.Language.Print (prettySimple)
import Vehicle.Compile.Normalise (normaliseInternal)
import Vehicle.Compile.SupplyNames (runSupplyNames)
import Vehicle.Compile.Descope (runDescope)
import Vehicle.Compile.Backend.Verifier

compileToSMTLib :: (AsSMTLibError e, MonadLogger m, MonadError e m)
                => CheckedProg -> m [SMTDoc]
compileToSMTLib prog = do
  logDebug "Beginning compilation to SMT"
  result <- compileProg prog
  logDebug "Finished compilation to SMT\n"
  return result

--------------------------------------------------------------------------------
-- Control

supportedTypes :: [Builtin]
supportedTypes =
  [ NumericType Real
  ]

--------------------------------------------------------------------------------
-- Data

data SMTDoc = SMTDoc
  { text    :: Doc ()   -- The problem to feed to the verifier
  , vars    :: [SMTVar] -- The list of variables in the property
  , negated :: Bool     -- Did the property contain universal quantifier and is therefore negated?
  }

data SMTVar = SMTVar
  { name    :: Symbol  -- Name of the variable
  , typ     :: SMTType -- Type of the variable
  }

data SMTType
  = SReal

instance Pretty SMTType where
  pretty SReal = "Real"

--------------------------------------------------------------------------------
-- Monad

type MonadSMTLib e m =
  ( AsSMTLibError e
  , MonadLogger m
  , MonadError e m
  )

type MonadSMTLibProp e m =
  ( MonadSMTLib e m
  , MonadReader Identifier m
  )

--------------------------------------------------------------------------------
-- Compilation

compileProg :: MonadSMTLib e m => CheckedProg -> m [SMTDoc]
compileProg (Main ds) = do
  results <- catMaybes <$> traverse compileDecl ds
  if null results
    then throwError mkNoPropertiesFound
    else return results

compileDecl :: MonadSMTLib e m => CheckedDecl -> m (Maybe SMTDoc)
compileDecl = \case
  DeclData{} ->
    normalisationError "dataset declarations"

  DeclNetw ann ident _ ->
    throwError $ mkUnsupportedDecl ann ident Network

  DefFun _ ident t e -> if not $ isProperty t
      then return Nothing
      else do
        let doc = compileProp ident e
        Just <$> doc

compileProp :: MonadSMTLib e m => Identifier -> CheckedExpr -> m SMTDoc
compileProp ident expr = runReaderT propertyDoc ident
  where
  propertyDoc :: MonadSMTLibProp e m => m SMTDoc
  propertyDoc = do
    logDebug $ "Beginning compilation of SMTLib property" <+> squotes (pretty ident)
    incrCallDepth

    (vars, body, negated) <- stripQuantifiers True expr
    let ctx = map name vars

    logDebug $ "Stripped existential quantifiers:" <+> pretty ctx <> line

    let body2 = runDescope (reverse ctx) (runSupplyNames body)

    logDebug $ "Descoping property" <+> prettySimple body2 <> line

    let assertions = splitTopLevelConjunctions body2
    assertionDocs <- traverse compileExpr assertions
    let bodyDoc = vsep (map assertion assertionDocs)

    let doc = if null vars then bodyDoc else compileVars vars <> line <> line <> bodyDoc
    let result = SMTDoc doc vars negated
    logDebug $ "Output:" <> align (line <> doc)

    decrCallDepth
    logDebug $ "Finished compilation of SMTLib property" <+> squotes (pretty ident)

    return result

compileVars :: [SMTVar] -> Doc a
compileVars vars = vsep (map compileVar vars)
  where
    compileVar :: SMTVar -> Doc a
    compileVar (SMTVar name t) = parens ("declare-const" <+> pretty name <+> pretty t)

stripQuantifiers :: MonadSMTLibProp e m => Bool -> CheckedExpr -> m ([SMTVar], CheckedExpr, Bool)
stripQuantifiers atTopLevel (QuantifierExpr q ann binder body) = do
  (e', negated) <- negatePropertyIfNecessary atTopLevel ann q body
  let varSymbol = getQuantifierSymbol binder
  varType <- getType ann varSymbol (typeOf binder)
  let var = SMTVar varSymbol varType
  (vars, body', _) <- stripQuantifiers False e'
  return (var : vars, body', negated)
stripQuantifiers _atTopLevel e = return ([], e, False)

negatePropertyIfNecessary :: MonadSMTLibProp e m
                          => Bool
                          -> CheckedAnn
                          -> Quantifier
                          -> CheckedExpr
                          -> m (CheckedExpr, Bool)
negatePropertyIfNecessary _atTopLevel _ann Any body  = return (body, False)
negatePropertyIfNecessary False       ann All _body = do
  ident <- ask
  throwError $ mkUnsupportedQuantifierSequence ann ident
negatePropertyIfNecessary True        ann  All body  = do
  let body' = normaliseInternal $ NotExpr ann (Builtin ann (BooleanType Prop)) [ExplicitArg ann body]
  logDebug $ align $ "Negating universal quantifier:" <+> prettySimple body' <> line
  return (body', False)

splitTopLevelConjunctions :: OutputExpr -> [OutputExpr]
splitTopLevelConjunctions (App _ann (Builtin _ (BooleanOp2 And)) [_, _, e1, e2]) =
  splitTopLevelConjunctions (argExpr e1) <> splitTopLevelConjunctions (argExpr e2)
splitTopLevelConjunctions e = [e]

getType :: MonadSMTLibProp e m => CheckedAnn -> Symbol -> CheckedExpr -> m SMTType
getType _   _ (Builtin _ (NumericType Real)) = return SReal
getType ann s t                              = do
  ident <- ask
  throwError $ mkUnsupportedVariableType ann ident s t supportedTypes

compileExpr :: MonadSMTLibProp e m => OutputExpr -> m (Doc b)
compileExpr = \case
  Type _         -> typeError "Type"
  Pi   _ann _ _  -> typeError "Pi"
  Hole _p _      -> resolutionError "Hole"
  Meta _p _      -> resolutionError "Meta"
  Ann _ann _ _   -> normalisationError "Ann"
  Lam _ann _ _   -> normalisationError "Lam"
  Seq _ann _     -> normalisationError "Seq"
  PrimDict _tc   -> visibilityError "PrimDict"

  Builtin _ann op -> compileBuiltin op
  Literal _ann l  -> return $ compileLiteral l
  Var     _ann v  -> compileVariable v

  QuantifierExpr q ann binder _ -> do
    ident <- ask
    throwError $ mkNonTopLevelQuantifier ann ident q (nameOf binder)

  App _ann fun args -> do
    funDoc  <- compileExpr fun
    argDocs <- catMaybes <$> traverse compileArg (NonEmpty.toList args)
    return $ if null argDocs
      then funDoc
      else parens $ hsep (funDoc : argDocs)

  Let _ann bound binder body -> do
    let binderDoc = pretty (nameOf binder :: OutputBinding)
    boundDoc <- compileExpr bound
    bodyDoc  <- compileExpr body
    return $ parens $ "let" <+> parens (parens (binderDoc <+> boundDoc)) <+> bodyDoc

compileBuiltin :: MonadSMTLibProp e m => Builtin -> m (Doc b)
compileBuiltin = \case
  BooleanType   t -> typeError $ pretty t
  NumericType   t -> typeError $ pretty t
  ContainerType t -> typeError $ pretty t
  TypeClass tc    -> typeError $ pretty tc

  QuantIn _ -> normalisationError "QuantIn"
  Cons      -> normalisationError $ pretty Cons
  At        -> normalisationError $ pretty At
  Map       -> normalisationError $ pretty Map
  Fold      -> normalisationError $ pretty Fold
  Quant q   -> normalisationError $ pretty (Quant q)

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

compileArg :: MonadSMTLibProp e m => OutputArg -> m (Maybe (Doc a))
compileArg arg = if visibilityOf arg == Explicit
  then Just <$> compileExpr (argExpr arg)
  else return Nothing

compileVariable :: MonadSMTLibProp e m => OutputVar -> m (Doc a)
compileVariable symbol = return $ pretty symbol

compileLiteral :: Literal -> Doc a
compileLiteral (LBool True)  = "true"
compileLiteral (LBool False) = "false"
compileLiteral (LNat n)      = pretty n
compileLiteral (LInt i)      = pretty i
compileLiteral (LRat x)      = pretty x

assertion :: Doc a -> Doc a
assertion p = parens ("assert" <+> p)