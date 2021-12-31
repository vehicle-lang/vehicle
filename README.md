[![Tests](https://github.com/wenkokke/vehicle/actions/workflows/ci.yml/badge.svg)](https://github.com/wenkokke/vehicle/actions/workflows/ci.yml)

# Introduction to Vehicle

Vehicle is a high-level dependently-typed domain-specific language for writing verification queries for neural networks. As well as providing a much higher-level interface for writing properties than the input formats of neural network verifiers such as Marabou or Eran, it also seamlessly connects the results of these verification tools with interactive theorem provers (ITPs) such as Agda.

This allows the user to maintain the abstraction of the neural network as a black-box component within their ITP code. This then facilitates the formal verification of the correctness of larger systems that include neural network components.

## Some examples

Some examples of Vehicle files are as follows:

1. Property 6 of the ACASXu unmanned aerial vehicle collision avoidance system ([paper](https://arxiv.org/abs/1702.01135)):
  - [Vehicle code](https://github.com/wenkokke/vehicle/blob/dev/examples/network/acasXu/property6/property6.vcl)
  - [Generated Marabou queries](https://github.com/wenkokke/vehicle/blob/dev/examples/network/acasXu/property6/property6-output-marabou)
  - [Generated Agda code](https://github.com/wenkokke/vehicle/blob/dev/examples/network/acasXu/property6/property6-output.agda)

2. The safety of a simple car controller that uses a neural-network ([paper](https://www.cs.utexas.edu/~boyer/controller.pdf)):
  - [Vehicle code](https://github.com/wenkokke/vehicle/blob/dev/examples/network/acasXu/property6/property6.vcl)
  - Generated Marabou code (coming soon)
  - [Generated Agda code](https://github.com/wenkokke/vehicle/blob/dev/examples/network/windController/windController-output.agda)
  - [Proof of overall correctness in Agda](https://github.com/wenkokke/vehicle/blob/dev/examples/network/windController/safety.agda)

The full list of example properties can be found [here](https://github.com/wenkokke/vehicle/tree/dev/examples).

## Verifier backends

At the moment, we are working on integration with Marabou.

## Interactive Theorem Prover backends

At the moment, we are working on integration with Agda.

# Long term goals

Our long-term goals include:

- Automatic generation of loss functions for both property-guided training and counter-example search.

- Adding a `dataset` keyword to transparently bind datasets so as to support robutness queries.

If you are interested in adding support for a particular verifier or ITP then open an issue on the [Issue Tracker](https://github.com/wenkokke/vehicle/issues) to discuss it with us.