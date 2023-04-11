# MNIST robustness example for loss function training

This is an example of a specification for the widely studied adversarial
robustness problem.

At a high-level the specification states that any small small pertubation to the
input, e.g. adjusting a few pixels, should not significantly change the output
of the network.

Although this example is specialised to image classification, in particular
to the MNIST dataset, it should be relatively easy to tweak to other domains.

This folder contains the following files:

- `test_mnist_custom_loss.py` - the python script for running the custom loss translation and training
a neural network. Will return accuracy and constraint accuracy on test set.

- `mnist.vcl` - the specification describing the desired behaviour. This file is different
to the specification found in examples/mnist-robustness/mnist-robustness.vcl as it uses a slightly
different definition of robustness (both are commonly used in literature)

- `mnist_constraint_accuracy.py` - python script containing methods for calculating constraint accuracy
for robustness property


## Instructions

To execute simply run `test_mnist_custom_loss.py`.

This test file uses default method of translation from constraint to loss function. To change this and see the avilable options
see `vhcile\vehicle-python\vehicle\command_line.py`. To find more information about avilable options either see
the file `vehicle\src\Vehicle\Backend\LossFunction\Logics.hs` or the paper "Logic of Differentiable Logics: Towards a Uniform Semantics of DL".
