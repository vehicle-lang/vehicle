Functions
=========

.. contents::
   :depth: 1
   :local:

Functions make up the backbone of the language.
Function application is written by juxtaposition, i.e. ``f x`` represents
the function ``f`` applied to input ``x``.

The function type is written ``A -> B`` where ``A`` is the input
type and ``B`` is the output type.
For example ``Tensor Rat [10,10] -> Bool`` is the type of functions from
``10 x 10`` matrix of rational numbers to a single boolean value.
As is standard in functional languages, the function arrow associates to
the right so ``A -> B -> C`` stands for ``A -> (B -> C)``.

New functions can be introduced in two ways.
Firstly, anonymous lambda functions can be declared using the ``\x ->``
notation, e.g.

.. code-block:: agda

   \x -> x + 2

is a function that adds 2 to its input.
Alternatively, the same function could be given a name and an explicit type
as follows:

.. code-block:: agda

  add2 : Nat -> Nat
  add2 x = x + 2

and can then be used later in the specification by referring to this name.