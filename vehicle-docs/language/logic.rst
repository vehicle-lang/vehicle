Logic
=====

.. contents::
   :depth: 1
   :local:

Booleans
--------

Like many languages, Vehicle has booleans. The type of boolean values is
:code:`Bool`, and there are two values :code:`True` and :code:`False`.

The available operations over booleans are:

.. list-table::
   :widths: 25 15 40 20
   :header-rows: 1

   * - Operation
     - Symbol
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

.. note::

   As discussed in the Tips and Tricks section, :code:`if then else`
   should be used sparingly, as each conditional in the final normalised
   expression approximately doubles the time taken to verify the specification.

.. note::

   Due to decidability and query dependency issues, the condition of an
   :code:`if then else` statement may not contain a quantification over
   a variable with an infinite domain. For example, the following is not allowed:

   .. code-block:: agda

      if (forall (x : Rat) . f x > 0) then 2 else 3

Equality
--------

Two expressions of the same type can be tested for equality/inequality
using the :code:`==` and :code:`!=` operators respectively.

The type of these operators are :code:`A -> A -> Bool` where :code:`A` can be any
of the following types:

- :code:`Bool`, :code:`Nat`, :code:`Int`, :code:`Rat`.
- :code:`Index d` for any value of :code:`d`.
- :code:`List A` if type :code:`A` also supports the operators.
- :code:`Vector A n` if type :code:`A` also supports the operators.
- :code:`Tensor A dims` if type :code:`A` also supports the operators.

For example:

.. code-block:: agda

   forall x . x != 0 => f x == 1
