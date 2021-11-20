module Vehicle.Backend.Verifier.SMTLib
  ( compileToSMTLib
  , compileProp
  , SMTDoc(..)
  , SMTVar(..)
  , SMTLibError(..)
  -- VNNLib
  , InputOrOutput(..)
  , UnsupportedNetworkType(..)
  ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.Maybe (catMaybes)
import Data.List.NonEmpty qualified as NonEmpty (toList)

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Print (prettyFriendlyDBClosed, prettySimple)
import Vehicle.Language.Normalise (normaliseInternal)
import Vehicle.Language.SupplyNames (runSupplyNames)
import Vehicle.Language.Descope (runDescope)
import Vehicle.Backend.Verifier.Core

compileToSMTLib :: (MonadLogger m, MonadError SMTLibError m)
                => CheckedProg -> m [SMTDoc]
compileToSMTLib prog = do
  logDebug "Beginning compilation to SMT"
  result <- compileProg prog
  logDebug "Finished compilation to SMT\n"
  return result

--------------------------------------------------------------------------------
-- Control

data InputOrOutput
  = Input
  | Output
  deriving (Eq)

instance Pretty InputOrOutput where
  pretty = \case
    Input  -> "input"
    Output -> "output"

supportedTypes :: [Builtin]
supportedTypes =
  [ NumericType Real
  ]

-- | Reasons why we might not support the network type.
-- Options with `Bool` type equate
data UnsupportedNetworkType
  = NotAFunction
  | NotATensor             InputOrOutput
  | MultidimensionalTensor InputOrOutput
  | VariableSizeTensor     InputOrOutput
  | WrongTensorType        InputOrOutput

instance Pretty UnsupportedNetworkType where
  pretty = \case
    NotAFunction              -> "the network type is not a function"
    NotATensor io             -> "the" <+> pretty io <+> "of the network is not a tensor"
    MultidimensionalTensor io -> "the" <+> pretty io <+> "of the network is not a 1D tensor"
    VariableSizeTensor io     -> "the" <+> pretty io <+> "of the network is a tensor with a non-fixed dimension"
    WrongTensorType io        -> "the type of the" <+> pretty io <+> "tensor of the network is not supported"


data SMTLibError
  = UnsupportedDecl               Provenance Identifier DeclType
  | UnsupportedVariableType       CheckedAnn Identifier Symbol CheckedExpr
  | UnsupportedQuantifierSequence CheckedAnn Identifier
  | NonTopLevelQuantifier         CheckedAnn Identifier Quantifier Symbol
  | NoPropertiesFound
  -- VNNLib
  | UnsupportedNetworkType        CheckedAnn Identifier CheckedExpr UnsupportedNetworkType
  | NoNetworkUsedInProperty       CheckedAnn Identifier

instance MeaningfulError SMTLibError where
  details = \case
    UnsupportedDecl ann ident decType -> let dType = squotes (pretty decType) in UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "When compiling property" <+> squotes (pretty ident) <+> "found" <+>
                     "a" <+> dType <+> "declaration which cannot be compiled to SMTLib."
      , fix        = "Remove all" <+> dType <+> "declarations or switch to a" <+>
                     "different compilation target."
      }

    UnsupportedVariableType ann ident name t -> UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "When compiling property" <+> squotes (pretty ident) <+> "found" <+>
                     "a quantified variable" <+> squotes (pretty name) <+> "of type" <+>
                     squotes (prettyFriendlyDBClosed t) <+> "which is not currently supported" <+>
                     "when compiling to SMTLib."
      , fix        = "Try switching the variable to one of the following supported types:" <+>
                     pretty supportedTypes
      }

    UnsupportedQuantifierSequence ann ident -> UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "When compiling property" <+> squotes (pretty ident) <+> "found a mixed" <+>
                     "sequence of quantifiers which is not currently supported when compiling" <+>
                     "to SMTLib. All properties must either be a sequence of" <+>
                     squotes (pretty (Quant All)) <+> "s or" <+> squotes (pretty (Quant Any)) <+> "s"
      , fix        = "If possible try removing some quantifiers."
      }

    NonTopLevelQuantifier ann ident q n -> UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "When compiling property" <+> squotes (pretty ident) <+> "found a non-top" <+>
                     "level quantifier" <+> squotes (pretty (Quant q) <+> pretty n) <+>
                     "which is not currently supported when compiling to SMTLib."
      , fix        = "Lift all quantifiers to the top-level"
      }

    NoPropertiesFound -> UError $ UserError
      { provenance = mempty
      , problem    = "No properties found in file."
      , fix        = "An expression is labelled as a property by giving it type" <+> squotes (pretty Prop) <+> "."
      }

    -- VNNLib
    UnsupportedNetworkType ann ident t detailedError -> UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "Found a" <+> squotes (pretty Network) <+> "declaration" <+> squotes (pretty ident) <+>
                     "whose type" <+> squotes (prettyFriendlyDBClosed t) <+> "is not currently unsupported." <+>
                     "Currently only networks of type" <+> squotes "Tensor A [m] -> Tensor B [n]" <+>
                     "where" <+> squotes "m" <+> "and" <+> squotes "n" <+> "are integer literals are allowed." <+>
                     "In particular" <+> pretty detailedError <+> "."
      , fix        = "Change the network type."
      }

    NoNetworkUsedInProperty ann ident -> UError $ UserError
      { provenance = provenanceOf ann
      , problem    = "After normalisation, the property" <+>
                     squotes (pretty ident) <+>
                     "does not contain any neural networks and" <+>
                     "therefore VNNLib is the wrong compilation target"
      , fix        = "Choose a different compilation target than VNNLib"
      }



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

type MonadSMTLib m =
  ( MonadLogger m
  , MonadError SMTLibError m
  )

type MonadSMTLibProp m =
  ( MonadSMTLib m
  , MonadReader Identifier m
  )

--------------------------------------------------------------------------------
-- Compilation

compileProg :: MonadSMTLib m => CheckedProg -> m [SMTDoc]
compileProg (Main ds) = do
  results <- catMaybes <$> traverse compileDecl ds
  if null results then
    throwError NoPropertiesFound
  else
    return results

compileDecl :: MonadSMTLib m => CheckedDecl -> m (Maybe SMTDoc)
compileDecl = \case
  DeclData{} ->
    normalisationError "dataset declarations"

  DeclNetw ann ident _ ->
    throwError $ UnsupportedDecl ann ident Network

  DefFun _ ident t e -> if not $ isProperty t
      then return Nothing
      else do
        let doc = compileProp ident e
        Just <$> doc

compileProp :: MonadSMTLib m => Identifier -> CheckedExpr -> m SMTDoc
compileProp ident expr = runReaderT propertyDoc ident
  where
  propertyDoc :: MonadSMTLibProp m => m SMTDoc
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

stripQuantifiers :: MonadSMTLibProp m => Bool -> CheckedExpr -> m ([SMTVar], CheckedExpr, Bool)
stripQuantifiers atTopLevel (QuantifierExpr q ann binder body) = do
  (e', negated) <- negatePropertyIfNecessary atTopLevel ann q body
  let varSymbol = getQuantifierSymbol binder
  varType <- getType ann varSymbol (typeOf binder)
  let var = SMTVar varSymbol varType
  (vars, body', _) <- stripQuantifiers False e'
  return (var : vars, body', negated)
stripQuantifiers _atTopLevel e = return ([], e, False)

negatePropertyIfNecessary :: MonadSMTLibProp m
                          => Bool
                          -> CheckedAnn
                          -> Quantifier
                          -> CheckedExpr
                          -> m (CheckedExpr, Bool)
negatePropertyIfNecessary _atTopLevel _ann Any body  = return (body, False)
negatePropertyIfNecessary False       ann All _body = do
  ident <- ask
  throwError $ UnsupportedQuantifierSequence ann ident
negatePropertyIfNecessary True        ann  All body  = do
  let body' = normaliseInternal $ NotExpr ann (Builtin ann (BooleanType Prop)) [ExplicitArg ann body]
  logDebug $ align $ "Negating universal quantifier:" <+> prettySimple body' <> line
  return (body', False)

splitTopLevelConjunctions :: OutputExpr -> [OutputExpr]
splitTopLevelConjunctions (App _ann (Builtin _ (BooleanOp2 And)) [_, _, e1, e2]) =
  splitTopLevelConjunctions (argExpr e1) <> splitTopLevelConjunctions (argExpr e2)
splitTopLevelConjunctions e = [e]

getType :: MonadSMTLibProp m => CheckedAnn -> Symbol -> CheckedExpr -> m SMTType
getType _   _ (Builtin _ (NumericType Real)) = return SReal
getType ann s t                              = do
  ident <- ask
  throwError $ UnsupportedVariableType ann ident s t

compileExpr :: MonadSMTLibProp m => OutputExpr -> m (Doc b)
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
    throwError $ NonTopLevelQuantifier ann ident q (nameOf binder)

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

compileBuiltin :: MonadSMTLibProp m => Builtin -> m (Doc b)
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

compileArg :: MonadSMTLibProp m => OutputArg -> m (Maybe (Doc a))
compileArg arg = if visibilityOf arg == Explicit
  then Just <$> compileExpr (argExpr arg)
  else return Nothing

compileVariable :: MonadSMTLibProp m => OutputVar -> m (Doc a)
compileVariable symbol = return $ pretty symbol

compileLiteral :: Literal -> Doc a
compileLiteral (LBool True)  = "true"
compileLiteral (LBool False) = "false"
compileLiteral (LNat n)      = pretty n
compileLiteral (LInt i)      = pretty i
compileLiteral (LRat x)      = pretty x

assertion :: Doc a -> Doc a
assertion p = parens ("assert" <+> p)