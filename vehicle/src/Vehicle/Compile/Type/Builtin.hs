module Vehicle.Compile.Type.Builtin where

import Data.Proxy (Proxy)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrintableBuiltin)
import Vehicle.Compile.Type.Builtin.Linearity (isLinearityBuiltinConstructor, typeLinearityBuiltin)
import Vehicle.Compile.Type.Builtin.Polarity (isPolarityBuiltinConstructor, typePolarityBuiltin)
import Vehicle.Compile.Type.Builtin.Standard (isStandardConstructor, typeStandardBuiltin)
import Vehicle.Data.Builtin.Linearity (LinearityBuiltin)
import Vehicle.Data.Builtin.Polarity (PolarityBuiltin)
import Vehicle.Data.Builtin.Standard (Builtin)

class (PrintableBuiltin builtin) => TypableBuiltin builtin where
  -- | Construct a type for the builtin
  typeBuiltin :: Provenance -> builtin -> Type builtin

  -- | Can meta variables depend on other values in the scope?
  -- Efficiency hack for polarity/linearity subsystems.
  useDependentMetas :: Proxy builtin -> Bool

  -- | Is the builtin a constructor?
  isConstructor :: builtin -> Bool

instance TypableBuiltin LinearityBuiltin where
  typeBuiltin = typeLinearityBuiltin
  useDependentMetas _ = False
  isConstructor = isLinearityBuiltinConstructor

instance TypableBuiltin PolarityBuiltin where
  typeBuiltin = typePolarityBuiltin
  useDependentMetas _ = False
  isConstructor = isPolarityBuiltinConstructor

instance TypableBuiltin Builtin where
  typeBuiltin = typeStandardBuiltin
  useDependentMetas _ = True
  isConstructor = isStandardConstructor
