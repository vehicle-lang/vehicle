Networks
========

.. contents::
   :depth: 1
   :local:

Basics
------

Networks are declared as follows using the ``network`` annotation:

.. code-block:: agda

   @network
   myNetwork : Tensor Rat [784] -> Tensor Rat [10]

At the moment Vehicle supports networks with a single input and output node.
Therefore expected type is of the form ``Tensor Rat dims1 -> Tensor Rat dims1``
where ``dims1`` and ``dims1`` are a non-empty list of constants.

Supported formats
-----------------

The actual implementation of the network is provided when using the
specification during training or verification.

At the moment only the `ONNX <https://onnx.ai/>`_ format is supported by Vehicle.

There are various libraries to convert trained models to the ONNX format:

- `Tensorflow <https://onnxruntime.ai/docs/tutorials/tf-get-started.html>`_
- `PyTorch <https://pytorch.org/tutorials/advanced/super_resolution_with_onnxruntime.html>`_

If you would be interested in other formats being supported, please get in touch.

Limitations
-----------

Multiple input/outputs
~~~~~~~~~~~~~~~~~~~~~~

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
