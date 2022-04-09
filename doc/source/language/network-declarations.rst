Network declarations
====================

.. autosummary::
   :toctree: generated

Networks are declared as follows using the `network` keyword:

.. code-block:: agda

   network myNetwork : TYPE

Note that no implementation is provided in the specification, instead the network

At the moment Vehicle supports networks with a single input and output nodes.
Although we are aware that net
Therefore expected type is of the form `Tensor A [m] -> Tensor B [n]` where `A` and
`B` are [numeric types] and `m` and `n` are concrete constants.


For example the following are allowed and are all equivalent:

.. code-block:: agda

   network myNetwork : Tensor Nat [4] -> Tensor Rat [1]

   network myNetwork : Tensor Nat [4] -> Rat

   network myNetwork : Nat -> Nat -> Nat -> Nat -> Rat

   network myNetwork : Nat -> Nat -> Nat -> Nat -> Tensor Rat [1]

The following are not currently allowed:

.. code-block:: agda

   -- Reason: boolean inputs are not current allowed
   -- Justification: lack of support for verifiers or training methods
   network myNetwork : Tensor Bool [4] -> Tensor Rat [1]

   -- Reason:
   network myNetwork : Nat -> Rat

   --
   network myNetwork : Nat -> Nat -> Nat -> Nat -> Rat

   network myNetwork : Nat -> Nat -> Nat -> Nat -> Tensor Rat [1]