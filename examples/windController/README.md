# Car controller example

A simple car controller that is formally proven to always keep the car on the road in the face of noisy sensor data and an unpredictable cross-wind. The
specification is verified in Marabou and can then be exported to Agda and Rocq and
combined with a larger proof to prove that the car never leaves the road. A full
description of the setup can be found in Section 2.1 of the [Vehicle paper](https://arxiv.org/pdf/2202.05207v1.pdf).

This folder contains the following files:

- `controller.onnx` - the neural network used to implement the controller.

- `windController.vcl` - the specification describing the desired behaviour.

- `rocqProof/SafetyProof.v` - the Rocq proof the car never leaves the road.

- `isabelleProof/SafetyProof.v` - the Isabelle proof the car never leaves the road.

## Verifying using Marabou

The controller can be verified against the specification by running the following command:

```bash
vehicle verify \
  --specification examples/windController/windController.vcl \
  --network controller:examples/windController/controller.onnx \
  --solver Marabou \
  --cache examples/windController/verificationResult
```

where the last line tells Vehicle where to write out the result of the verification
which can then be used by Agda in the next step.

The intermediate Marabou queries can be found in `examples/windController/verificationResult`.

## Compiling to specification to an ITP backend

### Rocq

The (verified) specification may then be compiled to Rocq by running the command:

```bash
vehicle compile itp \
  --target Rocq \
  --specification examples/windController/windController.vcl \
  --cache examples/windController/verificationResult \
  --output examples/windController/rocqProof/WindControllerSpec.v
```

When `--cache` is supplied, each `@property` is emitted as

```coq
Lemma <name> : <type>.
Proof. vehicle_validate "<absolute path to cache>". Qed.
```

instead of an `Axiom`. The `vehicle_validate` tactic — provided by the
`vehicle-rocq` companion library — invokes `vehicle validate --cache=...`
at `Qed.` time and closes the goal only if validation succeeds. The full
proof of safety using the generated spec is in `rocqProof/SafetyProof.v`.

`vehicle compile` canonicalises the supplied cache path to an absolute
path before embedding it in the generated `.v` file, so `rocq compile`
can be invoked from any directory. For the cache itself to remain
relocatable, pass absolute paths for the specification and network when
running `vehicle verify` (otherwise those resource paths are stored
relative to the verify cwd and `vehicle validate` will look for them
there).

To build the Rocq proof, generate the Coq makefile from `_CoqProject`
and run it:

```bash
cd examples/windController/rocqProof
rocq makefile -f _CoqProject -o Makefile
make
```

See [`vehicle-rocq/README.md`](../../vehicle-rocq/README.md) for details
on the plugin.
