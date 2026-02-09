# Vehicle Lean 4 Companion Library

This library contains definitions and utilities used by the Vehicle compiler when generating Lean 4 code. Users should include this library when working with Vehicle-generated Lean files.

## Building

To build this library, you need:
- Lean 4 (v4.0.0 or later)
- Lake (Lean's package manager, included with Lean)
- Mathlib (installed via Lake)

Build with:
```bash
lake build
```

## Library Contents

### `Vehicle.Tensor`
Definitions for tensor types and operations, including:
- `Tensor` - multi-dimensional arrays with contravariant and covariant dimensions
- `constTensor` - constant tensor construction
- `tensorMap` - pointwise operations on tensors
- `tensorOp` - binary operations on tensors

### `Vehicle.Utils`
Utility definitions including:
- `forallInList` / `existsInList` - quantification over lists
- `forallIndex` / `existsIndex` - quantification over finite indices
- `reduceAnd` / `reduceOr` - boolean tensor reduction
- Tensor comparison operations with reduction

## References

For more information about Vehicle, see: https://github.com/vehicle-lang/vehicle
