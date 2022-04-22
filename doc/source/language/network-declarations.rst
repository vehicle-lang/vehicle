Network declarations
====================

.. autosummary::
   :toctree: generated

Basics
------

Networks are declared as follows using the :code:`network` keyword:

.. code-block:: agda

   network myNetwork : Tensor Rat [784] -> Tensor Rat [10]

At the moment Vehicle supports networks with a single input and output node.
Therefore expected type is of the form :code:`Tensor A [m] -> Tensor B [n]`
where :code:`A` and :code:`B` are [numeric types] and :code:`m` and :code:`n`
are known constants.

For example the following are allowed and are all equivalent:

.. code-block:: agda

   network myNetwork : Tensor Nat [4] -> Tensor Rat [1]

   network myNetwork : Tensor Nat [4] -> Rat

   network myNetwork : Nat -> Nat -> Nat -> Nat -> Rat

   network myNetwork : Nat -> Nat -> Nat -> Nat -> Tensor Rat [1]

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

For example the following is not a valid type of network:

.. code-block:: agda

   network myNetwork : Tensor Rat [5] -> Tensor Rat [10] -> Tensor Rat [2]

This is a matter of implementation rather than a fundemental limitation.
Please get in touch if you would be interested in this functionality
being added to Vehicle.

Boolean networks
~~~~~~~~~~~~~~~~

The following are not currently allowed:

.. code-block:: agda

   -- Reason: boolean inputs are not current allowed
   -- Justification: lack of support for verifiers or training methods
   network myNetwork : Tensor Bool [4] -> Tensor Rat [1]
