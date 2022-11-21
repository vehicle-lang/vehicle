Functions
=========

.. contents::
   :depth: 1
   :local:

Functions make up the backbone of the Vehicle language.

Function types
--------------

The function type is written ``A -> B`` where ``A`` is the input
type and ``B`` is the output type.
For example ``Rat -> Bool`` is the type of functions from
a rational number to a boolean value.

As is standard in functional languages, the function arrow associates to
the right so ``A -> B -> C`` is therefore equivalent to ``A -> (B -> C)``.
The type ``A -> (B -> C)`` is a function that takes something of type `A`
and returns a function from `B` to `C`.
In contrast ``(A -> B) -> C`` is a function that takes another function
from ``A -> B`` as its first argument and returns something of type ``C``.

Function application
--------------------

As in most functional languages, function application is written
by juxtaposition of the function with its arguments. For example, given
a function ``f`` of type ``Rat -> Bool -> Rat`` and arguments ``x`` of
type ``Rat`` and ``y`` of type ``Bool``, the application of ``f`` to ``x``
and ``y`` is written ``f x y`` and this expression has type ``Bool``.

This is unlike imperative languages such as Python, C or Java where
you would write ``f(x,y)``.

Function declarations
---------------------

Declarations may be used to define new functions. A declaration is of the form

.. code-block:: agda

   <name> : <type>
   <name> [<args>] = <expr>

For example the function ``increment`` that takes adds ``1`` to a natural number may be
defined as follows:

.. code-block:: agda

  increment : Nat -> Nat
  increment x = x + 1

and can then be used later in the specification.

Lambdas
-------

Alternatively, anonymous functions can be declared using the
``\x -> ...`` notation, e.g.

.. code-block:: agda

   \x -> x + 1

is also a function that increments its input.
Lambda functions can be applied in the same way as normal,
for example:

.. code-block:: agda

   (\x -> x + 1) 1

will evaluate to ``2``.

Variables bound by lambdas may optionally be annotated by
their type as follows:

.. code-block:: agda

   \(x : Nat) -> x + 1

Multiple parameter lambdas are also supported:

.. code-block:: agda

   \x y -> x + 2 * y
