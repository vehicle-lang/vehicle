{-# LANGUAGE OverloadedLists #-}

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
import Vehicle.Language.Print (prettyFriendly, prettySimple)
import Vehicle.Language.Normalise (normaliseInternal)
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
  [ Real
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
  = UnsupportedDecl Provenance Identifier DeclType
  | UnsupportedVariableType Provenance Identifier Symbol CheckedExpr
  | UnsupportedQuantifierSequence Provenance Identifier
  | NonTopLevelQuantifier Provenance Identifier
  | NoPropertiesFound
  -- VNNLib
  | UnsupportedNetworkType Provenance Identifier CheckedExpr UnsupportedNetworkType
  | NoNetworkUsedInProperty Provenance Identifier

instance MeaningfulError SMTLibError where
  details = \case
    UnsupportedDecl p ident decType -> let dType = squotes (pretty decType) in UError $ UserError
      { provenance = p
      , problem    = "When compiling property" <+> squotes (pretty ident) <+> "found" <+>
                     "a" <+> dType <+> "declaration which cannot be compiled to SMTLib."
      , fix        = "Remove all" <+> dType <+> "declarations or switch to a" <+>
                     "different compilation target."
      }

    UnsupportedVariableType p ident name t -> UError $ UserError
      { provenance = p
      , problem    = "When compiling property" <+> squotes (pretty ident) <+> "found" <+>
                     "a quantified variable" <+> squotes (pretty name) <+> "of type" <+>
                     squotes (prettyFriendly t) <+> "which is not currently supported" <+>
                     "when compiling to SMTLib."
      , fix        = "Try switching the variable to one of the following supported types:" <+>
                     pretty supportedTypes
      }

    UnsupportedQuantifierSequence p ident -> UError $ UserError
      { provenance = p
      , problem    = "When compiling property" <+> squotes (pretty ident) <+> "found a mixed" <+>
                     "sequence of quantifiers which is not currently supported when compiling" <+>
                     "to SMTLib. All properties must either be a sequence of" <+>
                     squotes (pretty All) <+> "s or" <+> squotes (pretty Any) <+> "s"
      , fix        = "If possible try removing some quantifiers."
      }

    NonTopLevelQuantifier p ident -> UError $ UserError
      { provenance = p
      , problem    = "When compiling property" <+> squotes (pretty ident) <+> "found a non-top" <+>
                     "level quantifier which is not currently supported when compiling to SMTLib."
      , fix        = "Lift all quantifiers to the top-level"
      }

    NoPropertiesFound -> UError $ UserError
      { provenance = mempty
      , problem    = "No properties found in file."
      , fix        = "An expression is labelled as a property by giving it type" <+> squotes (pretty Prop) <+> "."
      }

    -- VNNLib
    UnsupportedNetworkType p ident t detailedError -> UError $ UserError
      { provenance = p
      , problem    = "Found a" <+> squotes (pretty Network) <+> "declaration" <+> squotes (pretty ident) <+>
                     "whose type" <+> squotes (prettyFriendly t) <+> "is not currently unsupported." <+>
                     "Currently only networks of type" <+> squotes "Tensor A [m] -> Tensor B [n]" <+>
                     "where" <+> squotes "m" <+> "and" <+> squotes "n" <+> "are integer literals are allowed." <+>
                     "In particular" <+> pretty detailedError <+> "."
      , fix        = "Change the network type."
      }

    NoNetworkUsedInProperty p ident -> UError $ UserError
      { provenance = p
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

  DeclNetw p ident _ ->
    throwError $ UnsupportedDecl p ident Network

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
    let ctx = map (User . name) vars
    let varDoc = compileVars vars

    logDebug $ "Stripped existential quantifiers:" <+> pretty ctx <> line

    let body2 = runDescope (reverse ctx) body

    logDebug $ "Descoping property" <+> prettySimple body2 <> line

    let assertions = splitTopLevelConjunctions body2
    assertionDocs <- traverse compileExpr assertions
    let bodyDoc = vsep (map assertion assertionDocs)

    let doc = varDoc <> line <> line <> bodyDoc
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
stripQuantifiers atTopLevel e = case quantView e of
  Just (QuantView ann q name t body) -> do
    (e', negated) <- negatePropertyIfNecessary atTopLevel ann q body
    let varSymbol = getSymbol name
    varType <- getType ann varSymbol t
    let var = SMTVar varSymbol varType
    (vars, body', _) <- stripQuantifiers False e'
    return (var : vars, body', negated)
  Nothing -> return ([], e, False)

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
  let body' = normaliseInternal $ mkNot ann (Builtin ann Prop) body
  logDebug $ align $ "Negating universal quantifier:" <+> prettySimple body' <> line
  return (body', False)

splitTopLevelConjunctions :: OutputExpr -> [OutputExpr]
splitTopLevelConjunctions (App _ann (Builtin _ And) [_, _, e1, e2]) =
  splitTopLevelConjunctions (argExpr e1) <> splitTopLevelConjunctions (argExpr e2)
splitTopLevelConjunctions e = [e]

getSymbol :: Name -> Symbol
getSymbol (User symbol) = symbol
getSymbol Machine       = developerError "Should not have quantifiers with machine names?"

getType :: MonadSMTLibProp m => CheckedAnn -> Symbol -> CheckedExpr -> m SMTType
getType _   _ (Builtin _ Real) = return SReal
getType ann s t                = do
  ident <- ask
  throwError $ UnsupportedVariableType ann ident s t

compileExpr :: MonadSMTLibProp m => OutputExpr -> m (Doc b)
compileExpr = \case
  Type _         -> typeError "Type"
  Pi   _ann _ _  -> typeError "Pi"
  Hole _p _      -> resolutionError "Hole"
  Meta _p _      -> resolutionError "Meta"
  Ann _ann _ _   -> normalisationError "Ann"
  Let _ann _ _ _ -> normalisationError "Let"
  Lam _ann _ _   -> normalisationError "Lam"
  Seq _ann _     -> normalisationError "Seq"
  PrimDict _tc   -> visibilityError "PrimDict"

  Builtin ann op -> compileBuiltin ann op
  Literal _ann l -> return $ compileLiteral l
  Var     _ann v -> compileVariable v

  App _ann fun args -> do
    funDoc  <- compileExpr fun
    argDocs <- catMaybes <$> traverse compileArg (NonEmpty.toList args)
    return $ if null argDocs
      then funDoc
      else parens $ hsep (funDoc : argDocs)

compileBuiltin :: MonadSMTLibProp m => OutputAnn -> Builtin -> m (Doc b)
compileBuiltin ann = \case
  Bool           -> typeError "Bool"
  Prop           -> typeError "Prop"
  Nat            -> typeError "Nat"
  Int            -> typeError "Int"
  Real           -> typeError "Real"
  List           -> typeError "List"
  Tensor         -> typeError "Tensor"
  HasEq          -> typeError "HasEq"
  HasOrd         -> typeError "HasOrd"
  IsTruth        -> typeError "IsTruth"
  IsNatural      -> typeError "IsNatural"
  IsIntegral     -> typeError "IsIntegral"
  IsRational     -> typeError "IsRational"
  IsReal         -> typeError "IsReal"
  IsContainer    -> typeError "IsContainer"
  IsQuantifiable -> typeError "IsQuantifiable"

  QuantIn _      -> normalisationError "QuantIn"

  Cons           -> normalisationError ":"
  At             -> normalisationError "!"
  Map            -> normalisationError "map"
  Fold           -> normalisationError "fold"

  Quant _q       -> do
    ident <- ask
    throwError $ NonTopLevelQuantifier ann ident

  If             -> return "ite"
  Impl           -> return "=>"
  And            -> return "and"
  Or             -> return "or"
  Not            -> return "not"
  Eq             -> return "=="
  Neq            -> return "distinct"
  Order o        -> return (pretty o)
  Mul            -> return "*"
  Div            -> return "/"
  Add            -> return "+"
  Sub            -> return "-"
  Neg            -> return "-"

compileArg :: MonadSMTLibProp m => OutputArg -> m (Maybe (Doc a))
compileArg arg = if vis arg == Explicit
  then Just <$> compileExpr (argExpr arg)
  else return Nothing

compileVariable :: MonadSMTLibProp m => OutputVar -> m (Doc a)
compileVariable (User symbol) = return $ pretty symbol
compileVariable Machine       = developerError "Should be no machine names in expressions."

compileLiteral :: Literal -> Doc a
compileLiteral (LBool True)  = "true"
compileLiteral (LBool False) = "false"
compileLiteral (LNat n)      = pretty n
compileLiteral (LInt i)      = pretty i
compileLiteral (LRat x)      = pretty x

assertion :: Doc a -> Doc a
assertion p = parens ("assert" <+> p)