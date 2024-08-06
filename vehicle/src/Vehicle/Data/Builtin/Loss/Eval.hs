{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Loss.Eval where

import Vehicle.Compile.Normalise.Builtin
  ( NormalisableBuiltin (..),
    evalAddNat,
    evalAddRat,
    evalAt,
    evalDivRat,
    evalFoldList,
    evalFoldVector,
    evalIndices,
    evalMapList,
    evalMapVector,
    evalMaxRat,
    evalMinRat,
    evalMulNat,
    evalMulRat,
    evalNegRat,
    evalPowRat,
    evalSubRat,
    evalZipWith,
  )
import Vehicle.Data.Builtin.Loss.Core
import Vehicle.Data.Builtin.Standard.Core ()
import Vehicle.Data.Expr.Normalised (Value (..))
import Vehicle.Syntax.Builtin qualified as V

instance NormalisableBuiltin LossBuiltin where
  evalBuiltinApp evalApp unchanged builtin args = do
    case builtin of
      IndexType -> return unchanged
      NatType -> return unchanged
      RatType -> return unchanged
      VectorType -> return unchanged
      ListType -> return unchanged
      -- Constructors
      Index {} -> return unchanged
      Bool {} -> return unchanged
      Nat {} -> return unchanged
      Rat {} -> return unchanged
      NilList -> return unchanged
      ConsList -> return unchanged
      Vector -> return unchanged
      -- Numeric operations
      Neg V.NegRat -> return $ evalNegRat unchanged args
      Add V.AddRat -> return $ evalAddRat unchanged args
      Add V.AddNat -> return $ evalAddNat unchanged args
      Sub V.SubRat -> return $ evalSubRat unchanged args
      Mul V.MulNat -> return $ evalMulNat unchanged args
      Mul V.MulRat -> return $ evalMulRat unchanged args
      Div V.DivRat -> return $ evalDivRat unchanged args
      PowRat -> return $ evalPowRat unchanged args
      MinRat -> return $ evalMinRat unchanged args
      MaxRat -> return $ evalMaxRat unchanged args
      MapList -> evalMapList (VBuiltin MapList) evalApp unchanged args
      FoldList -> evalFoldList (VBuiltin FoldList) evalApp unchanged args
      LookupVector -> return $ evalAt unchanged args
      FoldVector -> evalFoldVector evalApp unchanged args
      MapVector -> evalMapVector evalApp unchanged args
      ZipWithVector -> evalZipWith evalApp unchanged args
      Indices -> return $ evalIndices (VBuiltin Indices) unchanged args
      -- Don't evaluate in order to allow for tensor optimisations later.
      ForeachIndex {} -> return unchanged
      Maximise {} -> return unchanged
      Minimise {} -> return unchanged
