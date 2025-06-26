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
   :widths: 25 15 40 20
   :header-rows: 1

   * - Operation
     - Symbol
     - Type
     - Example
   * - Addition
     - :code:`+`
     - :code:`Nat -> Nat -> Nat`
     - :code:`x + y`
   * - Multiplication
     - :code:`*`
     - :code:`Nat -> Nat -> Nat`
     - :code:`x * y`
   * - Division
     - :code:`/`
     - :code:`Nat -> Nat -> Real`
     - :code:`x / y`
   * - Less than or equal
     - :code:`<=`
     - :code:`Nat -> Nat -> Bool`
     - :code:`x <= y`
   * - Less than
     - :code:`<`
     - :code:`Nat -> Nat -> Bool`
     - :code:`x < y`
   * - Greater than or equal
     - :code:`>=`
     - :code:`Nat -> Nat -> Bool`
     - :code:`x >= y`
   * - Greater than
     - :code:`>`
     - :code:`Nat -> Nat -> Bool`
     - :code:`x >= y`
   * - Min
     - :code:`min`
     - :code:`Nat -> Nat -> Bool`
     - :code:`min x y`
   * - Max
     - :code:`max`
     - :code:`Nat -> Nat -> Bool`
     - :code:`max x y`

Note that inequalities can be chained, so that ``x < y <= z`` will be
expanded to ``x < y and y <= z``.

Reals
-----

The type of real numbers is written as ``Real``.

The available operations over reals are:

.. list-table::
   :widths: 25 15 40 20
   :header-rows: 1

   * - Operation
     - Symbol
     - Type
     - Example
   * - Addition
     - :code:`+`
     - :code:`Real -> Real -> Real`
     - :code:`x + y`
   * - Subtraction
     - :code:`-`
     - :code:`Real -> Real -> Real`
     - :code:`x - y`
   * - Multiplication
     - :code:`*`
     - :code:`Real -> Real -> Real`
     - :code:`x * y`
   * - Division
     - :code:`/`
     - :code:`Real -> Real -> Real`
     - :code:`x / y`
   * - Negation
     - :code:`-`
     - :code:`Real -> Real`
     - :code:`- y`
   * - Less than or equal
     - :code:`<=`
     - :code:`Real -> Real -> Bool`
     - :code:`x <= y`
   * - Less than
     - :code:`<`
     - :code:`Real -> Real -> Bool`
     - :code:`x < y`
   * - Greater than or equal
     - :code:`>=`
     - :code:`Real -> Real -> Bool`
     - :code:`x >= y`
   * - Greater than
     - :code:`>`
     - :code:`Real -> Real -> Bool`
     - :code:`x >= y`
   * - Min
     - :code:`min`
     - :code:`Real -> Real -> Bool`
     - :code:`min x y`
   * - Max
     - :code:`max`
     - :code:`Real -> Real -> Bool`
     - :code:`max x y`


.. note::

   We are aware that the disconnect between the semantics of real numbers
   and floating point can lead to soundness bugs in verification. Adding floating
   point types with configurable precision is on our road map.
