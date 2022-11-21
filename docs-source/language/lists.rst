Lists
=====

.. contents::
   :depth: 1
   :local:

Basics
------

Lists are collections of an arbitrary number of elements of a single type.
The type of a list of elements of type ``A`` is written as ``List A``, e.g.
``List Nat`` is a list of natural numbers.

Lists can be created using the ``[x_1, ..., x_n]`` syntax, e.g. ``[5, 2, 6]``.

Operations
----------

The following operations over lists are currently supported:

.. list-table::
   :widths: 15 12 43 30
   :header-rows: 1

   * - Operation
     - Symbol
     - Type
     - Example
   * - Map
     - :code:`map`
     - :code:`(A -> B) -> List A -> List B`
     - :code:`map (\x -> x + 1) xs`
   * - Fold
     - :code:`fold`
     - :code:`(A -> B -> B) -> B -> List A -> B`
     - :code:`fold (\x y -> x + y) 0 xs`

Absence of a lookup function
----------------------------

It is a deliberate design decision that there is no operation to
lookup the element at a given position in a list.
Therefore, the individual elements of a list can only be accessed by
quantifying over them, e.g.

.. code-block:: agda

   myList : List Rat
   myList = [1, 1, 2, 3, 5, 8]

   positive : Bool
   positive = forall x in myList . x >= 0

This is because every Vehicle specification is a mathematical formula, and in
order to maintain this relationship we need to guarantee the absence of
out-of-bounds errors.
If you really need to look up the value at an arbitrary index then you should be
using a ``Vector`` instead of a ``List``.
