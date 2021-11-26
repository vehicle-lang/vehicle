module Vehicle.Language.LetLifting where

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.AlphaEquivalence (alphaEq)

{-
letLift :: CheckedExpr -> CheckedExpr
letLift = \case  
  Type     l             -> _
  Ann      ann e t       -> _
  App      ann ex ne     -> _
  Pi       ann bi ex     -> _
  Builtin  ann bu        -> _
  Var      ann var       -> _
  Hole     ann txt       -> _
  Meta     ann me        -> _
  Let      ann ex bi ex' -> _
  Lam      ann bi ex     -> _
  Literal  ann lit       -> _
  Seq      ann exs       -> _
  PrimDict ex           -> _
-}