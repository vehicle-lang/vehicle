import Lake
open Lake DSL

package vehicle


@[default_target]
lean_lib Vehicle where
  srcDir := "src"
  roots := #[`Vehicle, `Vehicle.Tensor, `Vehicle.Utils, `TensorTest]

require Mathlib from git
  "https://github.com/leanprover-community/mathlib4.git" @ "master"
