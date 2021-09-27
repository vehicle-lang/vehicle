
module Vehicle.Backend.Verifier.VNNLib where

import Prettyprinter (Pretty(..), Doc)

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Backend.Core

data VNNLibOptions

type Code = Doc Precedence

type MonadVNNLibCompile m = MonadCompile VNNLibOptions m

class CompileToVNNLib a where
  compile
    :: MonadVNNLibCompile m
    => a
    -> m Code


compileProg :: MonadVNNLibCompile m => OutputProg -> _
compileProg = _
