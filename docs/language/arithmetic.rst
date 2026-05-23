Arithmetic
==========

.. contents::
   :depth: 1
   :local:

Naturals
--------

The most basic type of number in Vehicle are the natural numbers.
The type of natural numbers is written as ``Nat``.

The available operations over naturals are:

.. list-table::
   :widths: 18 14 35 33
   :header-rows: 1

   * - Operation
     - Syntax
     - Type
     - Support
   * - Add
     - :code:`x + y`
     - :code:`Nat → Nat → Nat`
     - |backendall_full|
   * - Multiply
     - :code:`x * y`
     - :code:`Nat → Nat → Nat`
     - |backendall_full|
   * - Divide
     - :code:`x / y`
     - :code:`Nat → Nat → Real`
     - |backendall_full|
   * - Compare
     - | ``x < y``
       | ``x > y``
       | ``x <= y``
       | ``x >= y``
       | ``x == y``
       | ``x != y``
     - ``Nat → Nat → Bool``
     - |backendall_full|

Note that inequalities can be chained, so that ``x < y <= z`` will be
expanded to ``x < y and y <= z``.

Reals
-----

The type of real numbers is written as ``Real``.

The available operations over reals are:

.. list-table::
   :widths: 18 14 35 33
   :header-rows: 1

   * - Operation
     - Syntax
     - Type
     - Support
   * - Add
     - :code:`x + y`
     - :code:`Real → Real → Real`
     - |backendall_full|
   * - Subtract
     - :code:`x - y`
     - :code:`Real → Real → Real`
     - |backendall_full|
   * - Multiply
     - :code:`x * y`
     - :code:`Real → Real → Real`
     - | |backendloss_full|
       | |backendverification_part| (:ref:`⤴ <verifying-linearity-limitations>`)
       | |backendagda_full|
       | |backendrocq_full|
       | |backendimandra_full|
       | |backendisabelle_full|
   * - Divide
     - :code:`x / y`
     - :code:`Real → Real → Real`
     - | |backendloss_full|
       | |backendverification_part| (:ref:`⤴ <verifying-linearity-limitations>`)
       | |backendagda_full|
       | |backendrocq_full|
       | |backendimandra_full|
       | |backendisabelle_full|
   * - Negation
     - :code:`- y`
     - :code:`Real → Real`
     - |backendall_full|
   * - Compare
     - | ``x < y``
       | ``x > y``
       | ``x <= y``
       | ``x >= y``
       | ``x == y``
       | ``x != y``
     - ``Real → Real → Bool``
     - |backendall_full|
   * - Min
     - :code:`min x y`
     - :code:`Real → Real → Real`
     - |backendall_full|
   * - Max
     - :code:`max x y`
     - :code:`Real → Real → Real`
     - |backendall_full|


.. note::

   We are aware that the disconnect between the semantics of real numbers
   and floating point can lead to soundness bugs in verification. Adding floating
   point types with configurable precision is on our road map.
