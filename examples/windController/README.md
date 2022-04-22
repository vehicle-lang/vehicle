Car controller example
======================

A simple car controller that is formally proven to always keep the car on the road in the face of noisy sensor data and an unpredictable cross-wind. The network
specification is verified in Marabou and then this result is connected to Agda and
combined with a larger proof to prove that the car never leaves the road. A full
description of the setup can be found in Section 2.1 of the [Vehicle paper](https://arxiv.org/pdf/2202.05207v1.pdf)):

This folder contains the following files:

- `controller.onnx` - the neural network used to implement the controller.

- `windController.vcl` - the specification describing the desired behaviour.

- `agdaProof/SafetyProof.agda` - the Agda proof the car never laves the road.

Verifying using Marabou
-----------------------

The controller can be verified against the specification by running the following command:
```bash
vehicle verify \
  --specification examples/windController/windController.vcl \
  --network controller:examples/windController/controller.onnx \
  --verifier Marabou \
  --proofCache examples/windController/windController.vclp
```
where the last line tells Vehicle where to write out the proof cache which can
then be used by Agda in the next step.

(Optional) If you would like to see the intermediate Marabou queries generated, you can
run the following command:
```bash
vehicle compile \
  --target Marabou \
  --specification examples/windController/windController.vcl \
  --outputFile examples/windController/windController-queries \
  --network controller:examples/windController/controller.onnx
```
which will put them in the `windController-queries` folder.

Compiling to specification to Agda
----------------------------------

The (verified) specification may then be compiled to Agda by running the command:
```bash
vehicle export \
  --itp Agda \
  --proofCache examples/windController/windController.vclp \
  --outputFile examples/windController/agdaProof/WindControllerSpec.agda
```

The full proof safety which makes uses of the generated Agda version of the specification in `agdaProof/WindControllerSpec.agda` is found in `agdaProof/SafetyProof.agda`.

Generated files
---------------

The outputs of the above Vehicle commands can be found in the test suite:
  - [Automatically generated Marabou queries](https://github.com/vehicle-lang/vehicle/tree/dev/test/Test/Compile/Golden/windController/windController-output-marabou)
  - [Automatically generated Agda code](https://github.com/vehicle-lang/vehicle/blob/dev/test/Test/Compile/Golden/windController/windController-output.agda)
