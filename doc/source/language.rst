Language
========

.. autosummary::
   :toctree: generated

   Vehicle

Vehicle is a functional, dependently-typed language.

Functions
---------

At it's heart, Vehicle is a functional language.



Function application is implicit by juxtaposition, i.e. `f x` is the function
`f` applied to `x`.


Arithmetic
----------

Logic
-----

Vehicle contains the following logical operators

- `Bool`
- `true`
- `false`
- `if then else`
- `and`
- `or`
- `=>`
- `not`
- `==`


Quantifiers
-----------

Vehicle supports both universal and existential quantifiers. For example:

.. code-block:: agda

   forall x y . x and y == y and x
   exists x . f x >= 0.0

One constraint of both the training and verification tools is that you may not mix
quantifiers within the same property. For example the following is not allowed:

.. code-block:: agda

   forall x . exists y. f x == y


Lists
-----

- `List`
- `map`
- `fold`

- No lookup function, have to quantify over.

Tensors
-------

- `Tensor A [1,2,3]`
- `map`
- `fold`
- `!`

Not currently supported
- addition
- multiplication

Lambdas
-------

Anonymous lambda functions can be written using the `\` symbol.

Network declarations
--------------------

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


Dataset declarations
--------------------

Function declarations
---------------------

Properties
----------

Idioms
------

- Naming indices and then looking them up

- Argmin/argmax