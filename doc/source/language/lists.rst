Lists (Docs TODO)
=================

.. contents::
   :depth: 1
   :local:

Basics
------

Lists are designed to be collections of an arbitrary number of elements of type A

- `List`
- `literals`

Operations
----------

- `map`
- `fold`
- `quantifiers`

Absence of a lookup function
----------------------------

It is a deliberate design decision that there is no operation in Vehicle to
lookup the element at a given position in a list.
Therefore, the individual elements of a list can only be accessed by
quantifying over them, e.g.

.. code-block:: agda

   list : List Rat
   list = [1, 1, 2, 3, 5, 8]

   positive : Bool
   positive = forall x in list . x >= 0

This is because every Vehicle specification is a mathematical formulae, and in
order to maintain this relationship we need to guarantee the absence of
out-of-bounds errors.
If you really need to lookup the value at an arbitrary index then you should be
using a one dimensional :code:`Tensor` instead of a :code:`List`.