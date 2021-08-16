
module Vehicle.Core.Print.Frontend
  ( prettyFrontend
  ) where

import Prettyprinter (Doc, pretty, line)

import Vehicle.Prelude
import Vehicle.Core.AST as VC ( CheckedExpr, OutputExpr )
import Vehicle.Core.Compile.Descope ( runDescope )
import Vehicle.Core.Print.Core ()
import Vehicle.Frontend.AST as VF ( OutputExpr )
import Vehicle.Frontend.Delaborate ( DelabError, runDelab )
import Vehicle.Frontend.Print ()

prettyFrontend :: VC.CheckedExpr -> Doc ann
prettyFrontend e = pretty e {-case runDelab (runDescope e :: VC.OutputExpr) :: Either DelabError VF.OutputExpr of
  Right expr -> pretty expr
  Left  err  -> developerError $
    "The following error:" <> line <> line <>
    pretty (show (details err)) <> line <> line <>
    "was encountered while trying to delaborate the following core expression:" <> line <> line <>
    pretty e -}