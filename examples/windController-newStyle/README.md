# Car controller example

A simple car controller that is formally proven to always keep the car on the road in the face of noisy sensor data and an unpredictable cross-wind. The
specification is verified in Marabou and can then be exported to Agda and Rocq and
combined with a larger proof to prove that the car never leaves the road. A full
description of the setup can be found in Section 2.1 of the [Vehicle paper](https://arxiv.org/pdf/2202.05207v1.pdf).

This folder contains the following files:

- `controller.onnx` - the neural network used to implement the controller.

- `windController.vcl` - the specification describing the desired behaviour.

- `agdaProof/SafetyProof.agda` - the Agda proof the car never leaves the road.

- `rocqProof/SafetyProof.v` - the Rocq proof the car never leaves the road.

The `newStyle` in the title of this folder refers to the use of the `records` in the specification which makes for a
much clearer specification. The normalisation has also been folded into the network itself.

## Verifying using Marabou

The controller can be verified against the specification by running the following command:

```bash
vehicle verify \
  --specification examples/windController-newStyle/windController.vcl \
  --network controller:examples/windController-newStyle/controller.onnx \
  --solver Marabou \
  --cache examples/windController-newStyle/verificationResult
```

where the last line tells Vehicle where to write out the result of the verification
which can then be used by Agda in the next step.

The intermediate Marabou queries can be found in `examples/windController/verificationResult`.

## Compiling to specification to an ITP backend

### Agda

The (verified) specification may then be compiled to Agda by running the command:

```bash
vehicle export \
  --target Agda \
  --cache examples/windController-newStyle/verificationResult \
  --output examples/windController-newStyle/agdaProof/WindControllerSpec.agda
```

The full proof of safety which makes use of the generated Agda version of the
specification in `agdaProof/WindControllerSpec.agda` is found in
`agdaProof/SafetyProof.agda`.
