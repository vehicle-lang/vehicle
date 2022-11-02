[![Build](https://github.com/vehicle-lang/vehicle/actions/workflows/build.yml/badge.svg)](https://github.com/vehicle-lang/vehicle/actions/workflows/build.yml)
[![readthedocs status](https://readthedocs.org/projects/vehicle-lang/badge/?version=latest)](https://vehicle-lang.readthedocs.io/en/latest/)
[![pre-commit.ci status](https://results.pre-commit.ci/badge/github/vehicle-lang/vehicle/dev.svg)](https://results.pre-commit.ci/latest/github/vehicle-lang/vehicle/dev)

# Vehicle

Vehicle is a system for embedding logical specifications into neural networks.
At its heart is the Vehicle specification language, a high-level, functional language for writing mathematically-precise specifications for your networks.

These specifications can then automatically be compiled down to loss functions to be
used when training your network.
After training, the same specification can be compiled down to low-level neural network verifiers such as Marabou which either prove that the specification holds or produce a counter-example. Such a proof is far better than simply testing, as you can prove that
the specifiation holds for _all_ inputs.
Verified specifications can also be exported to interactive theorem provers (ITPs)
such as Agda.
This in turn allows for the formal verification of larger software systems
that use neural networks as subcomponents.
The generated ITP code is tightly linked to the actual deployed network, so changes
to the network will result in errors when checking the larger proof.

**Note**: Vehicle is a work in progress. No stable version is yet available and breaking
changes will occur frequently so use at your own risk. While verification mode and exporting to ITPs is functional, training mode is still under active development.

## Documentation

- [User manual](https://vehicle-lang.readthedocs.io/en/latest/) - currently a work in progress.

## Examples

Each of the following examples comes with an explanatory README file:

- [ACAS Xu](https://github.com/vehicle-lang/vehicle/blob/dev/examples/acasXu/) - The complete specification of the ACAS Xu collision avoidance system from the [Reluplex paper](https://arxiv.org/abs/1702.01135) in a single file.

- [Car controller](https://github.com/vehicle-lang/vehicle/blob/dev/examples/windController/) - A neural network controller that is formally proven to always keep a simple model of a car on the road in the face of noisy sensor data and an unpredictable cross-wind.

- [MNIST robustness](https://github.com/vehicle-lang/vehicle/blob/dev/examples/mnist-robustness/) - A classifier for the MNIST dataset that is proven to be robust around the images in the dataset.

In addition to the above, further examples of specifications can be found in the [test suite](https://github.com/vehicle-lang/vehicle/tree/dev/test/specs)
and the corresponding output of the Vehicle compiler can be found [here](https://github.com/vehicle-lang/vehicle/tree/dev/test/Test/Compile/Golden).

## Support

If you are interested in adding support for a particular format/verifier/ITP
then open an issue on the [Issue Tracker](https://github.com/wenkokke/vehicle/issues)
to discuss it with us.

#### Neural network formats

- [ONNX](https://onnx.ai/)

#### Dataset formats

- [IDX](http://yann.lecun.com/exdb/mnist/)

#### Verifier backends

- [Marabou](https://github.com/NeuralNetworkVerification/Marabou)

#### Interactive Theorem Prover backends

- [Agda](https://agda.readthedocs.io/)
