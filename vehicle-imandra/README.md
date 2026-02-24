# Vehicle Imandra Backend — Runtime Libraries

IML libraries used by the [Vehicle](https://github.com/vehicle-lang/vehicle) Imandra backend. Some of these are adapted from Vehicle's Isabelle/HOL tensor libraries. Together they provide the primitives that generated IML code depends on.

For design background and end-to-end verification examples, see [imandra-vehicle](https://github.com/imandra-ai/imandra-vehicle).

## Libraries

| File | Description |
|------|-------------|
| `tensor.iml` | Core tensor type and operations (lookup, map, element-wise arithmetic) |
| `subtensor.iml` | Subtensor extraction and combination |
| `add.iml` | Tensor addition with algebraic properties |
| `scalar_mult.iml` | Scalar multiplication with distributivity laws |
| `vehicle.iml` | High-level Vehicle primitives (comparisons, reductions, quantifiers, flex tensors) |

## Usage

Generate an IML module from a Vehicle specification:

```bash
cabal run vehicle -- compile itp -s examples/windController/windController.vcl -t Imandra -o vehicle-imandra/spec.iml
```

### Example: WindController

The Vehicle specification `windController.vcl` defines safety properties for a neural network car controller. The generated IML module contains:

- An **opaque** controller declaration (the NN is abstract)
- **Normalisation**, **safe input**, and **safe output** predicates over tensors
- A **safety axiom**: `safe_input x ==> safe_output x`

```iml
let safe_input (x : input_vector) : bool =
  (forall_index 2
    (fun (i : int) ->
      ((leq_tensor_reduced_real
          (tensor_cdot (-1.0) (flextensor_from_vec [] [ (Real.(13.0 /. 4.0)) ]))
          (flex_subtensor x i))
        && (leq_tensor_reduced_real (flex_subtensor x i) (flextensor_from_vec [] [ (Real.(13.0 /. 4.0)) ])))))

axiom safe x =
  ((safe_input x) ==> (safe_output x))
```

This generated module can then be imported into a hand-written IML proof that establishes end-to-end system-level safety (see [`car_safety_auto.iml`](https://github.com/imandra-ai/imandra-vehicle/blob/main/src/car_safety_auto.iml) in imandra-vehicle).

## Verification

```bash
cd vehicle-imandra
imandrax-cli check spec.iml
```
