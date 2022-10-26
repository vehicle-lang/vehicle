MNIST robustness example
========================

This is an example of a specification for the widely studied adversarial
robustness problem.
At a high-level the specification states that any small small pertubation to the
input, e.g. adjusting a few pixels, should not significantly change the output
of the network.

Although this example is specialised to image classification, in particular
to the MNIST dataset, it should be relatively easy to tweak to other domains.

This folder contains the following files:

- `mnist.onnx` - (MISSING) - the neural network used to implement the controller.

- `mnist-robustness.vcl` - the specification describing the desired behaviour.

- `test-images.idx` (MISSING) - the dataset of training input images.

- `test-labels.idx` (MISSING) - the dataset of training output labels.

Verifying using Marabou
-----------------------

The outputs of the above Vehicle commands can be found in the test suite:

A network can be verified against the specification by running the following command:
```bash
vehicle verify \
  --specification examples/mnist-robustness/mnist-robustness.vcl \
  --network controller:examples/mnist-robustness/controller.onnx \
  --parameter epsilon:0.1 \
  --dataset trainingImages:examples/mnist-robustness/test-images.idx \
  --dataset trainingLabels:examples/mnist-robustness/test-labels.idx \
  --verifier Marabou
```
