Advanced topics (TODO)
======================

.. contents::
   :depth: 1
   :local:

This page contains advanced topics about the specification language.
These shouldn't be necessary to read in order to get going with Vehicle
but may be of interest to advanced users.

Dependent functions
-------------------

Unlike many common strongly-typed languages, Vehicle is dependently typed
and so if you have a function type ``A -> B``, the type ``B`` may depend
on the value passed for ``A``.
For example ``(n : Nat) -> Tensor Rat [n]`` is the type of a function that
takes in a natural number ``n`` and returns a vector of rationals of length
``n``.

Type-classes
------------
