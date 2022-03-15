[![Tests](https://github.com/wenkokke/vehicle/actions/workflows/ci.yml/badge.svg)](https://github.com/wenkokke/vehicle/actions/workflows/ci.yml)

# Introduction to Vehicle

Vehicle is a high-level dependently-typed domain-specific language for writing verification queries for neural networks. As well as providing a much higher-level interface for writing properties than the input formats of neural network verifiers such as Marabou or Eran, it also seamlessly connects the results of these verification tools with interactive theorem provers (ITPs) such as Agda.

This allows the user to maintain the abstraction of the neural network as a black-box component within their ITP code. This then facilitates the formal verification of the correctness of larger systems that include neural network components.

## Some examples

Some examples of Vehicle projects with end-to-end integration with Agda are as follows:

1. A simple car controller that keeps the car on the road in the face of noisy sensor data and an unpredicatable cross-wind (from Section 2.1 of the [Vehicle paper](https://arxiv.org/pdf/2202.05207v1.pdf)):
  - [Folder](https://github.com/vehicle-lang/vehicle/blob/dev/examples/windController/)
  - [Vehicle spec](https://github.com/vehicle-lang/vehicle/blob/dev/examples/windController/windController.vcl)
  - [Proof of overall correctness in Agda](https://github.com/vehicle-lang/vehicle/blob/dev/examples/windController/agdaProof/SafetyProof.agda)

We also have several non-ITP integrated specifications. Notable ones include:

1. Property 6 of the ACASXu collision avoidance system ([paper](https://arxiv.org/abs/1702.01135))
- [Vehicle spec](https://github.com/vehicle-lang/vehicle/blob/dev/test/specs/acasXu-property6/acasXu-property6.vcl) / [Marabou queries](https://github.com/vehicle-lang/vehicle/tree/dev/test/Test/Compile/Golden/acasXu-property6/acasXu-property6-output-marabou) / [Agda code](https://github.com/vehicle-lang/vehicle/blob/dev/test/Test/Compile/Golden/acasXu-property6/acasXu-property6-output.agda)

The full list of example specifications can be found [here](https://github.com/vehicle-lang/vehicle/tree/dev/test/specs) and the corresponding output of the Vehicle compiler [here](https://github.com/vehicle-lang/vehicle/tree/dev/test/Test/Compile/Golden).

## Verifier backends

At the moment, we have support for Marabou. However we require a branch of the main Marabou repo to which we've added native support for Onnx files, so it is recommended you install it via `cabal run build init` command. We are working on getting it merged.

## Interactive Theorem Prover backends

At the moment, we support Agda.

# Long term goals

Our long-term goals include:

- Automatic generation of loss functions for both property-guided training and counter-example search.

- Adding a `dataset` keyword to transparently bind datasets so as to support robutness queries.

If you are interested in adding support for a particular verifier or ITP then open an issue on the [Issue Tracker](https://github.com/wenkokke/vehicle/issues) to discuss it with us.