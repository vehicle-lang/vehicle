# ACAS Xu example

ACAS Xu is a collection of 45 neural networks that together make up a collision avoidance system
for automonous unmanned aircraft.
The partial verification of the system was first described in the seminal
[Reluplex paper](https://arxiv.org/abs/1702.01135).
This example demonstrates how the entire specification, consisting of all
10 properties, can be written in a single file.
Unlike the equivalent low-level Marabou queries, the specification is written at a high-level and is understandable by a non-expert.

## Input files

- `acasXu.vcl` - the specification describing the desired behaviour.

- `acasXu_1_7.onnx`, `acasXu_1_8.onnx`, `acasXu_1_9.onnx` - 3 out of the 45 networks. The remainder can be found [here](https://github.com/NeuralNetworkVerification/Marabou/tree/master/resources/onnx/acasxu).

## Verifying using Marabou

The following command verifies `property3` for the network `acasXu_1_7.onnx`:
```bash
vehicle \
  verify \
  --specification examples/acasXu/acasXu.vcl \
  --verifier Marabou \
  --network acasXu:examples/acasXu/acasXu_1_7.onnx \
  --property property3
```
The same property can be verified for the other two networks in the folder. The remaining
properties apply to other network components

## Output files

The outputs of the above Vehicle commands can be found in the test suite:

- [Automatically generated Marabou queries](https://github.com/vehicle-lang/vehicle/tree/dev/vehicle/tests/golden/compile/acasXu/acasXu.inputquery)
