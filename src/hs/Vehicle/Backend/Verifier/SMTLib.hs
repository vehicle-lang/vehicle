
module Vehicle.Backend.Verifier.SMTLib
  ( compileToSMTLib
  , PropertyDoc(..)
  , SMTVar(..)
  ) where

import Control.Monad.Except (MonadError(..))
import Data.Maybe (catMaybes)
import Data.List.NonEmpty qualified as NonEmpty (toList)

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Core.Normalise (normaliseInternal)
import Vehicle.Core.Compile.Descope (runDescope)
import Vehicle.Backend.Verifier.Core

compileToSMTLib :: (MonadLogger m, MonadError SMTLibError m)
                => CheckedProg -> m [PropertyDoc]
compileToSMTLib prog = do
  logDebug "Beginning compilation to SMT"
  result <- compileProg prog
  logDebug "Finished compilation to SMT"
  return result

--------------------------------------------------------------------------------
-- Control

data SMTLibError
  = UnsupportedDecl (WithProvenance Identifier) DeclType
  | UnsupportedVariableType Provenance Symbol CheckedExpr
  | UnnormalisedListOperation Provenance Builtin
  | UnsupportedQuantifierSequence
  | NonTopLevelQuantifier Provenance

data PropertyDoc = PropertyDoc
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

--------------------------------------------------------------------------------
-- Monad

type MonadSMTLib m =
  ( MonadLogger m
  , MonadError SMTLibError m
  )

--------------------------------------------------------------------------------
-- Compilation

compileProg :: MonadSMTLib m => CheckedProg -> m [PropertyDoc]
compileProg (Main ds) = catMaybes <$> traverse compileDecl ds

compileDecl :: MonadSMTLib m => CheckedDecl -> m (Maybe PropertyDoc)
compileDecl = \case
  DeclNetw _ ident _ ->
    throwError $ UnsupportedDecl ident Network

  DeclData _ ident _ ->
    throwError $ UnsupportedDecl ident Dataset

  DefFun _ ident t e -> if isProperty t
      then Just <$> compileProp (deProv ident) e
      else return Nothing

compileProp :: MonadSMTLib m => Identifier -> CheckedExpr -> m PropertyDoc
compileProp ident expr = do
  (vars, body, negated) <- stripQuantifiers True expr
  let body2 = runDescope body
  bodyDoc <- compileExpr body2
  return $ PropertyDoc bodyDoc vars negated

stripQuantifiers :: MonadSMTLib m => Bool -> CheckedExpr -> m ([SMTVar], CheckedExpr, Bool)
stripQuantifiers atTopLevel e = case quantView e of
  Just (QuantView ann q name t body) -> do
    (e', negated) <- negatePropertyIfNecessary atTopLevel ann q body
    let varSymbol = getSymbol name
    varType <- getType ann varSymbol t
    let var = SMTVar varSymbol varType
    (vars, body', _) <- stripQuantifiers False e'
    return (var : vars, body', negated)
  Nothing -> return ([], e, False)

negatePropertyIfNecessary :: MonadSMTLib m
                          => Bool
                          -> CheckedAnn
                          -> Quantifier
                          -> CheckedExpr
                          -> m (CheckedExpr, Bool)
negatePropertyIfNecessary _atTopLevel _ann Any body  = return (body, False)
negatePropertyIfNecessary False       _ann All _body =
  throwError UnsupportedQuantifierSequence
negatePropertyIfNecessary True        ann  All body  =
  return (normaliseInternal $ mkNot ann (Builtin ann Prop) body, False)

getSymbol :: Name -> Symbol
getSymbol (User symbol) = symbol
getSymbol Machine       = developerError "Should not have quantifiers with machine names?"

getType :: MonadSMTLib m => CheckedAnn -> Symbol -> CheckedExpr -> m SMTType
getType _   _ (Builtin _ Real) = return SReal
getType ann s t                =
  throwError $ UnsupportedVariableType ann s t

compileExpr :: MonadSMTLib m => OutputExpr -> m (Doc b)
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
    return $ parens $ hsep (funDoc : argDocs)

compileBuiltin :: MonadSMTLib m => OutputAnn -> Builtin -> m (Doc b)
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

  Cons           -> throwError $ UnnormalisedListOperation ann Cons
  At             -> throwError $ UnnormalisedListOperation ann At
  Map            -> throwError $ UnnormalisedListOperation ann Map
  Fold           -> throwError $ UnnormalisedListOperation ann Fold

  Quant _q       -> throwError $ NonTopLevelQuantifier ann

  If             -> return "ite"
  Impl           -> return "=>"
  And            -> return "and"
  Or             -> return "or"
  Not            -> return "not"
  Eq             -> return "=="
  Neq            -> return "distinct"
  Le             -> return "<="
  Lt             -> return "<"
  Ge             -> return ">"
  Gt             -> return ">="
  Mul            -> return "*"
  Div            -> return "/"
  Add            -> return "+"
  Sub            -> return "-"
  Neg            -> return "-"

compileArg :: MonadSMTLib m => OutputArg -> m (Maybe (Doc a))
compileArg (Arg _ Explicit e) = Just <$> compileExpr e
compileArg _                  = return Nothing

compileVariable :: MonadSMTLib m => OutputVar -> m (Doc a)
compileVariable (User symbol) = return $ pretty symbol
compileVariable Machine       = developerError "Should be no machine names in expressions."

compileLiteral :: Literal -> Doc a
compileLiteral (LBool True)  = "true"
compileLiteral (LBool False) = "false"
compileLiteral (LNat n)      = pretty n
compileLiteral (LInt i)      = pretty i
compileLiteral (LRat x)      = pretty x