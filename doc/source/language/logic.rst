Logic
=====

.. autosummary::
   :toctree: generated

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