
module Vehicle.Core.Print.Friendly
  ( PrettyFriendly(..)
  ) where

import Vehicle.Prelude
import Vehicle.Core.AST as VC ( Name, CheckedExpr, OutputExpr )
import Vehicle.Core.Compile.Descope ( runDescopeWithCtx )
import Vehicle.Core.Print (prettySimple)
import Vehicle.Frontend.AST qualified as VF ( OutputExpr )
import Vehicle.Frontend.Delaborate ( runDelabWithoutLogging )
import Vehicle.Frontend.Print (prettyFrontend)

class PrettyFriendly a where
  prettyFriendly :: [(Name, CheckedExpr)] -> a -> Doc b

instance PrettyFriendly CheckedExpr where
  prettyFriendly ctx e = prettySimple e
    {-
    let
    e' :: OutputExpr = runDescopeWithCtx (fst <$> ctx) e
    e'' :: VF.OutputExpr = runDelabWithoutLogging e'
    in prettyFrontend e''
    -}