[![Tests](https://github.com/vehicle-lang/vehicle/actions/workflows/ci.yml/badge.svg)](https://github.com/vehicle-lang/vehicle/actions/workflows/ci.yml)

[![Documentation](https://readthedocs.org/projects/vehicle-lang/badge/?version=latest)](https://vehicle-lang.readthedocs.io/en/latest/)


# Vehicle

Vehicle is a high-level dependently-typed domain-specific language for enforcing logical specifications for neural networks. As well as providing a much higher-level interface for writing properties than the input formats of neural network verifiers such as Marabou or Eran, it also seamlessly connects the results of these verification tools with interactive theorem provers (ITPs) such as Agda.

This allows the user to maintain the abstraction of the neural network as a black-box component within their ITP code. This then facilitates the formal verification of the correctness of larger systems that include neural network components.


## Documentation

- [User manual](https://vehicle-lang.readthedocs.io/en/latest/) - currently a work in progress.

## Some examples

#### ACAS Xu

The complete specification of the ACAS Xu collision avoidance system from the [Reluplex paper](https://arxiv.org/abs/1702.01135) in a single file:
- [Folder](https://github.com/vehicle-lang/vehicle/blob/dev/examples/acasXu/)
- [Specification](https://github.com/vehicle-lang/vehicle/blob/dev/examples/acasXu/acasXu.vcl)
- [Automatically generated Marabou queries](https://github.com/vehicle-lang/vehicle/tree/dev/test/Test/Compile/Golden/acasXu/acasXu-output-marabou)

#### Simple car controller

A simple car controller that is formally proven to always keep the car on the road in the face of noisy sensor data and an unpredictable cross-wind. Neural network spec is verified in Marabou and then this result is connected to Agda to prove the desired safety property (from Section 2.1 of the [Vehicle paper](https://arxiv.org/pdf/2202.05207v1.pdf)):
  - [Folder](https://github.com/vehicle-lang/vehicle/blob/dev/examples/windController/)
  - [Specification](https://github.com/vehicle-lang/vehicle/blob/dev/examples/windController/windController.vcl)
  - [Automatically generated Marabou queries](https://github.com/vehicle-lang/vehicle/tree/dev/test/Test/Compile/Golden/windController/windController-output-marabou)
  - [Automatically generated Agda code](https://github.com/vehicle-lang/vehicle/blob/dev/test/Test/Compile/Golden/windController/windController-output.agda)
  - [Overall proof of correctness in Agda](https://github.com/vehicle-lang/vehicle/blob/dev/examples/windController/agdaProof/SafetyProof.agda)

#### Other

The full list of example specifications in the test suite can be found [here](https://github.com/vehicle-lang/vehicle/tree/dev/test/specs) and the corresponding output of the Vehicle compiler can be found [here](https://github.com/vehicle-lang/vehicle/tree/dev/test/Test/Compile/Golden).

## Support

#### Neural network formats

- [ONNX](https://onnx.ai/)

#### Dataset formats

- [IDX](http://yann.lecun.com/exdb/mnist/)

#### Verifier backends

- [Marabou](https://github.com/NeuralNetworkVerification/Marabou)
   - We require a branch of the main Marabou repo to which we've added native support for ONNX files, so it is recommended you install it via `cabal run build init` command. We have an [open PR](https://github.com/NeuralNetworkVerification/Marabou/pull/553) to get it merged into Marabou.

#### Interactive Theorem Prover backends

- [Agda](https://agda.readthedocs.io/)

## Long term goals

Our long-term goals include:

- Automatic generation of loss functions for both property-guided training and counter-example search.

If you are interested in adding support for a particular verifier or ITP then open an issue on the [Issue Tracker](https://github.com/wenkokke/vehicle/issues) to discuss it with us.