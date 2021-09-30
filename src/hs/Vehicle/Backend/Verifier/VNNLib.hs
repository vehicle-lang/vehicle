module Vehicle.Backend.Verifier.VNNLib where

import Control.Monad.State (MonadState(..), modify)
import Control.Monad.Reader (MonadReader(..))
import Prettyprinter (Pretty(..), Doc)

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Backend.Core
import Vehicle.Core.Print ()

data VNNLibOptions

type Code = Doc Precedence

type DeclCtx = [(Identifier, BlackBoxType)]

type MonadVNNLibCompile m =
  ( MonadCompile VNNLibOptions m
  , MonadState DeclCtx m
  )

class CompileToVNNLib a where
  compile
    :: MonadVNNLibCompile m
    => a
    -> m Code

compileProg :: MonadVNNLibCompile m => OutputProg -> m Code
compileProg (Main ds) = vsep2 <$> traverse compileDecl ds

compileDecl :: MonadVNNLibCompile m => OutputDecl -> m Code
compileDecl DeclData{} = normError "Dataset declarations"
compileDecl (DeclNetw _ ident _) = do
  modify ((deProv ident, Network) :)
  return ""
compileDecl (DefFun _ ident t e) = case t of
  (Builtin _ Prop) -> do
    let comment = ";" <+> pretty (deProv ident)
    body <- compileExpr True e
    return $ comment <> line <> body
  _                -> return ""

compileExpr :: MonadVNNLibCompile m => Bool -> OutputExpr -> m Code
compileExpr topLevel = \case
  Type{}     -> typeError
  Pi{}       -> typeError
  Ann{}      -> normError "Annotations"
  Let{}      -> normError "Let declarations"
  Lam{}      -> normError "Lambda functions"
  Seq{}      -> normError "Sequence literals"
  PrimDict{} -> _
  Hole{}     -> _
  Meta{}     -> _

  App     _ann fun arg -> _
  Builtin _ann op      -> _
  Var     _ann v       -> _
  Literal _ann l       -> return $ pretty l

normError :: Doc b -> a
normError form = developerError $ form <+> "should have been normalised out"

typeError :: a
typeError = developerError "Should not be compiling types"