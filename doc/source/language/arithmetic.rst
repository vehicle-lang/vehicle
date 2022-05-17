Arithmetic
==========

.. contents::
   :depth: 1
   :local:

Naturals
--------

The most basic type of number in Vehicle are the natural numbers.
The type of natural numbers is written as `Nat`.

The following table contains the list of builtin operations:

.. list-table::
   :widths: 25 15 40 20
   :header-rows: 1

   * - Operation
     - Name
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

Rational
--------

Rational numbers in Vehicle are stored using arbitrary precision.
The type of natural numbers is written as `Rat`.

The following table contains the list of builtin operations:

.. list-table::
   :widths: 25 15 40 20
   :header-rows: 1

   * - Operation
     - Name
     - Type
     - Example
   * - Addition
     - :code:`+`
     - :code:`Rat -> Rat -> Rat`
     - :code:`x + y`
   * - Subtraction
     - :code:`-`
     - :code:`Rat -> Rat -> Rat`
     - :code:`x - y`
   * - Multiplication
     - :code:`*`
     - :code:`Rat -> Rat -> Rat`
     - :code:`x * y`
   * - Division
     - :code:`/`
     - :code:`Rat -> Rat -> Rat`
     - :code:`x / y`
   * - Negation
     - :code:`-`
     - :code:`Rat -> Rat`
     - :code:`- y`
   * - Less than or equal
     - :code:`<=`
     - :code:`Rat -> Rat -> Bool`
     - :code:`x <= y`
   * - Less than
     - :code:`<`
     - :code:`Rat -> Rat -> Bool`
     - :code:`x < y`
   * - Greater than or equal
     - :code:`>=`
     - :code:`Rat -> Rat -> Bool`
     - :code:`x >= y`
   * - Greater than
     - :code:`>`
     - :code:`Rat -> Rat -> Bool`
     - :code:`x >= y`


.. note::

   We are aware that the disconnect between the semantics of rational/real numbers
   and floating point can lead to soundness bugs in verification. Adding floating
   point types with configurable precision is on our road map.
