/-
Verification that the tensor library is accessible and compiles
-/

import Vehicle

namespace TensorTests

open Vehicle

-- Verify tensor types are accessible
#check (Tensor Rat [2] : Type)
#check (Tensor Nat [3] : Type)

-- Verify all major functions are available
#check @foreach_tensor
#check @at_tensor
#check @const_tensor
#check @hadamard
#check @tensorLe
#check @tensorSum
#check @tensorProd

-- Simple example: construct a tensor of naturals
example : Tensor Nat [3] :=
  foreach_tensor fun i => i.val

end TensorTests
