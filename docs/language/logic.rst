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
   :widths: 17 24 26 33
   :header-rows: 1

   * - Operation
     - Syntax
     - Type
     - Support
   * - Conjunction
     - :code:`x and y`
     - :code:`Bool → Bool → Bool`
     - |backendall_full|
   * - Disjunction
     - :code:`x or y`
     - :code:`Bool → Bool → Bool`
     - |backendall_full|
   * - Implication
     - :code:`x => y`
     - :code:`Bool → Bool → Bool`
     - |backendall_full|
   * - Negation
     - :code:`not x`
     - :code:`Bool → Bool`
     - |backendall_full|
   * - Conditional
     - :code:`if x then y else z`
     - :code:`Bool → A → A → A`
     - | |backendloss_part|
       | |backendverification_full|
       | |backendagda_full|
       | |backendrocq_full|
       | |backendimandra_full|
       | |backendisabelle_full|

Conditionals
------------

Conditional statements are written as follows:

.. code-block:: agda

   if f x > 0 then x < 0 else x > 0

In a functional language like Vehicle
all statements must return a value. Therefore it is not possible to
omit the :code:`else` branch when writing a conditional.

.. note::

   As discussed in the Tips and Tricks section, :code:`if then else`
   should be used sparingly, as each conditional in the final normalised
   expression approximately doubles the time taken to verify the specification.

Equality
--------

Two expressions of the same type can be tested for equality/inequality
using the :code:`==` and :code:`!=` operators respectively.

The type of these operators are :code:`A → A → Bool` where :code:`A` can be any
of the following types:

- :code:`Bool`, :code:`Nat`, :code:`Int`, :code:`Real`.
- :code:`Index d` for any value of :code:`d`.
- :code:`List A` if type :code:`A` also supports the operators.
- :code:`Vector A n` if type :code:`A` also supports the operators.
- :code:`Tensor A dims` if type :code:`A` also supports the operators.

For example:

.. code-block:: agda

   forall x . x != 0 => f x == 1
