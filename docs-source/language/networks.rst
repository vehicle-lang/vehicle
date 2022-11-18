Networks
========

.. contents::
   :depth: 1
   :local:

Basics
------

Networks are declared by adding a ``@network`` annotation to a function
declaration.
For example, the following declares a network ``myNetwork`` which takes
a 2-dimension tensor of size 28 x 28 and returns a vector of 10 rationals:

.. code-block:: agda

   @network
   myNetwork : Tensor Rat [28, 28] -> Vector Rat 10

Note that although no implementation for the network is provided directly in the
specification, ``myNetwork`` can still be used in the specification as any other
declared function would be.

This follows the Vehicle philosophy that specifications should independent of
any particular network, and should be able to be used to train/test/verify a
range of candidate networks implementations.

An implementation of a given network in the specification is passed in at
compile time via the ``--network`` command line option.
For example, you can provide a concrete implementation ``path/to/network/network.onnx``
for ``myNetwork`` by passing ``--network myNetwork:path/to/network/network.onnx``.

Supported formats
-----------------

At the moment only the `ONNX <https://onnx.ai/>`_ format is supported by Vehicle.
There are various libraries to convert trained models to the ONNX format:

- Converting from `Tensorflow <https://onnxruntime.ai/docs/tutorials/tf-get-started.html>`_
- Converting from `PyTorch <https://pytorch.org/tutorials/advanced/super_resolution_with_onnxruntime.html>`_

If you would be interested in other formats being supported, please get in touch.

Limitations
-----------

Multiple input/outputs
~~~~~~~~~~~~~~~~~~~~~~

At the moment Vehicle supports networks with a single input and output node.
Therefore all declarations annoated with ``@network`` must have a type of the
form ``Tensor Rat dims1 -> Tensor Rat dims2`` where ``dims1`` and ``dims2``
are a non-empty list of constants.

Although we are aware that common network representations such as ONNX,
Tensorflow, and Pytorch can have multiple input and output nodes,
these are not currently supported by Vehicle.

This is a matter of implementation rather than a fundamental limitation.
Please get in touch if you would be interested in this functionality
being added to Vehicle.

Boolean networks
~~~~~~~~~~~~~~~~

The following are not currently allowed:

.. code-block:: agda

   -- Reason: boolean inputs are not current allowed
   -- Justification: lack of support for verifiers or training methods
   @network
   myNetwork : Tensor Bool [4] -> Tensor Rat [1]
