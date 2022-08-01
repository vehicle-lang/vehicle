Quantifiers
===========

.. contents::
   :depth: 1
   :local:

Quantifying over infinite sets
------------------------------

One of the main advantages of Vehicle compared to a testing framework is
that it can be used to state and prove specifications that describe the
network's behaviour over an infinite set of values.

Suppose you have the following network which produces two outputs:

.. code-block:: agda

   network f : Tensor Rat [10, 10] -> Tensor Rat [2]

and would like to specify that *for any input the network's first
output is always positive*.
This can be achieved by using the ``forall`` quantifier as follows:

.. code-block:: agda

   forall x . f x ! 0 > 0

which brings a new variable ``x`` of type ``Tensor Rat [10, 10]`` into
scope. The variable ``x`` has no assigned value and therefore represents
an arbitrary input.

Similarly, if trying to specify that *there exists at least one input for which
the network's first output is positive*, the ``exists`` quantifier can be
used as follows:

.. code-block:: agda

   exists x . f x ! 0 > 0

As with lambda functions, the quantified variables can be annotated with
their types:

.. code-block:: agda

   exists (x : Tensor Rat [10, 10]) . f x ! 0 > 0

and multiple variables can be quantified over at once:

.. code-block:: agda

   exists x i . f x ! i > 0

In many cases you don't want the property to hold over *all* the
values in the set, but only a (still infinite) subset of them.
For example, network inputs are frequently normalised to lie
within the range ``[0,1]``. If the quantified variable's domain is not
also restricted to this range, then Vehicle will produce spurious
counter-examples to the specification.

In general such restrictions can be achieved by combining a quantifier
with an implication as follows:

.. code-block:: agda

   forall x. (forall i j . 0 <= x ! i ! j <= 1) => f x ! 0 > 0

Quantifying over finite sets
----------------------------

While most specifications will quantify over at least one variable
with an infinite domain, sometimes one might also want to quantify
over a finite set of values. There are multiple ways of doing this:

Quantifying over an ``Index`` type
++++++++++++++++++++++++++++++++++

The first approach is to quantify over a variable with the ``Index``
type. For example:

.. code-block:: agda

   pointwiseLess : Vector Rat 3 -> Vector Rat 3 -> Bool
   pointwiseLess x y = forall (i : Index 3) . x ! i < y ! i

will get automatically expanded to:

.. code-block:: agda

   pointwiseLess : Vector Rat 3 -> Vector Rat 3 -> Bool
   pointwiseLess x y = x ! 0 < y ! 0 and x ! 1 < y ! 1 and x ! 1 < y ! 1

The type annotations on the quantified variable ``i`` are included for clarity
but are not need in practice as they can be inferred by the compiler.

The ``in`` keyword
++++++++++++++++++

Alternatively quantifiers can be modified with the ``in`` keyword to
quantify over all the values contained within a ``List``, ``Vector`` or ``Tensor``:

.. code-block:: agda

   myList : List Rat
   myList = [0.4, 1.1, 0.2]

   myListInRange : Bool
   myListInRange = forall x in myList . 0 <= f x <= 1

During compilation Vehicle will automatically expand this out
to a sequence of conjunctions as follows:

.. code-block:: agda

   myListInRange : Bool
   myListInRange = 0 <= f 0.4 <= 1 and 0 <= f 1.1 <= 1 and 0 <= f 0.2 <= 1


The ``foreach`` quantifier
++++++++++++++++++++++++++

Finally a common use of the finite quantification is to assert that the
property holds over every element of a dataset, e.g.

.. code-block:: agda

   dataset dataset : List (Tensor Rat [28, 28])

   ...

   robust : Bool
   robust = forall x in dataset . robustAround x

The problem with this formulation of the specification is that Vehicle
will only report whether the network is robust around *all* the elements
in the dataset. This is unlikely to be true.

Instead the ``foreach`` quantifier may be used. Instead of returning a
single value of type ``Bool`` it returns a ``List`` / ``Tensor``
of ``Bool`` values. When used a property, Vehicle will therefore report
on the verification status of each individual element.

.. code-block:: agda

   dataset trainingDataset : List (Tensor Rat [28, 28])

   ...

   robust : List Bool
   robust = foreach x in trainingDataset . robustAround x

Unlike the ``forall`` keyword, the ``foreach`` keyword cannot be used to
quantify over infinite types. It can be used to quantify over ``Index``
types.

Limitations
-----------

One hard constraint enforced by both training and
verification tools is that you may not use both a ``forall` and
an ``exists` that quantify over infinite domains within the same property.
For example, the following is not allowed:

.. code-block:: agda

   network f : Vecotr Rat 2 -> Vector Rat 1

   surjective : Bool
   surjective = forall y . exists x. f x == y

This remains true even if you move one or more of the quantifiers to
separate functions. For example, the following is not allowed either:

.. code-block:: agda

   network f : Vector Rat 2 -> Rat

   hits : Vector Rat 2 -> Bool
   hits y = exists x . f x == y

   surjective : Bool
   surjective = forall y . hits y

However, you can have both types of quantifiers within the same
specification as long as they belong to different properties.
For example, the following *is* allowed:

.. code-block:: agda

   network f : Vector Rat 2 -> Rat

   prop1 : Bool
   prop1 y = exists x . f x >= 2

   prop2 : Bool
   prop2 = forall x . 1 <= f x <= 3
