Basic logic
===========

.. autosummary::
   :toctree: generated

Booleans
--------

Like many systems Vehicle contains booleans. The type of boolean values is
:code:`Bool`, and there are two values :code:`True` and :code:`False`.

The following table contains the basic boolean operations:

.. list-table::
   :widths: 25 15 40 20
   :header-rows: 1

   * - Operation
     - Name
     - Type
     - Example
   * - Conjunction
     - :code:`and`
     - :code:`Bool -> Bool -> Bool`
     - :code:`x and y`
   * - Disjunction
     - :code:`or`
     - :code:`Bool -> Bool -> Bool`
     - :code:`x or y`
   * - Implication
     - :code:`=>`
     - :code:`Bool -> Bool -> Bool`
     - :code:`x => y`
   * - Negation
     - :code:`not`
     - :code:`Bool -> Bool`
     - :code:`not x`

Conditionals
------------

Conditional statements are written using the syntax :code:`if .. then .. else ..`
and have type :code:`Bool -> A -> A -> A` for any type :code:`A`.
For example:

.. code-block:: agda

   if f x > 0 then x < 0 else x > 0

In a functional language like Vehicle (and unlike in imperative languages)
all statements must return a value. Therefore it is not possible to
omit the :code:`else` branch when writing a conditional.

Due to decidability issues, specifications that will be exported to a
theorem prover may not contain :code:`if then else` statements whose
condition involves quantification over a variable with an infinite
domain. For example, the following is not allowed:

.. code-block:: agda

   if (forall x. f x > 0) then 2 else 3

.. note::

   As discussed in the Tips and Tricks section, :code:`if then else`
   should be used sparingly, as each conditional in the final normalised
   expression approximately doubles the time taken to verify the specification.

Equality
--------

Two expressions of the same type can be declared to be equal
or not equal using the :code:`==` and :code:`!=` operators respectively.

The type of these operators are :code:`A -> A -> Bool` where :code:`A` can be any
of the following types:

- :code:`Bool`, :code:`Nat`, :code:`Int`, :code:`Rat`.
- :code:`Index n` for any value of :code:`n`.
- :code:`List A` if type :code:`A` also supports the operators.
- :code:`Tensor A dims` if type :code:`A` also supports the operators.

For example:

.. code-block:: agda

   forall x . x != 0 => f x == 1