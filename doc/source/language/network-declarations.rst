Network declarations
====================

.. autosummary::
   :toctree: generated

Basics
------

Networks are declared as follows using the :code:`network` keyword:

.. code-block:: agda

   network myNetwork : TYPE

Note that no implementation is provided in the specification, instead the network

At the moment Vehicle supports networks with a single input and output nodes.
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

At the moment only the `ONNX <https://onnx.ai/>`_ format is supported by the core
Vehicle compiler.

Limitations
-----------

Multiple input/outputs
~~~~~~~~~~~~~~~~~~~~~~

Although we are aware that common graph representations such as ONNX,
Tensorflow, and Pytorch files can have multiple input and output nodes,
these are not currently supported by Vehicle.

For example the following is not a valid type of network:

.. code-block:: agda

   network myNetwork : Tensor Rat [5] -> Tensor Rat [10] -> Tensor Rat [2]

This is a matter of implementation rather than a fundmental limitation.
Please get in touch if you would be interested in this functionality
being added to Vehicle.

Boolean networks
~~~~~~~~~~~~~~~~

The following are not currently allowed:

.. code-block:: agda

   -- Reason: boolean inputs are not current allowed
   -- Justification: lack of support for verifiers or training methods
   network myNetwork : Tensor Bool [4] -> Tensor Rat [1]
