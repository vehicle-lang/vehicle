Quantifiers
===========

.. contents::
   :depth: 1
   :local:

Quantifying over infinite domains
---------------------------------

One of the main advantages of Vehicle compared to a testing framework is
that it can be used to state and prove specifications that describe the
network's behaviour over an infinite set of values.

Suppose you have the following network which produces two outputs:

.. code-block:: agda

   network f : Tensor Rat [10] -> Tensor Rat [2]

and would like to specify that *for any input the network's first
output is always positive*.
This can be achieved by using the :code:`forall` quantifier as follows:

.. code-block:: agda

   forall x . f x ! 0 > 0

which brings a new variable :code:`x` of type :code:`Tensor Rat [10]` into
scope. The variable :code:`x` has no assigned value and therefore represents
an arbitrary input.

Similarly, if trying to specify that *there exists at least one input for which
the network's first output is positive*, the :code:`exists` quantifier can be
used as follows:

.. code-block:: agda

   exists x . f x ! 0 > 0

As with lambda functions, the quantified variables can be annotated with
their types:

.. code-block:: agda

   exists (x : Tensor Rat [10]) . f x ! 0 > 0

and multiple variables can be quantified over at once:

.. code-block:: agda

   exists x i . f x ! i > 0

Limitations
~~~~~~~~~~~

One hard constraint enforced by both training and
verification tools is that you may not use both a :code:`forall` and
an :code:`exists` that quantify over infinite domains within the same property.
For example, the following is not allowed:

.. code-block:: agda

   network f : Tensor Rat [2] -> Rat

   surjective : Bool
   surjective = forall y . exists x. f x == y

This remains true even if you move one or more of the quantifiers to
separate functions. For example, the following is not allowed either:

.. code-block:: agda

   network f : Tensor Rat [2] -> Rat

   hits : Tensor Rat [2] -> Bool
   hits y = exists x . f x == y

   surjective : Bool
   surjective = forall y . hits y

However, you can have both types of quantifiers within the same
specification as long as they belong to different properties.
For example, the following *is* allowed:

.. code-block:: agda

   network f : Tensor Rat [2] -> Rat

   prop1 : Bool
   prop1 y = exists x . f x >= 2

   prop2 : Bool
   prop2 = forall x . 1 <= f x <= 3

Restricting an infinite domain
------------------------------

In many cases you don't want the property to hold over *all* the
values in the domain, but only a (still infinite) subset of them.
For example, network inputs are frequently normalised to lie
within the range [0,1]. If you don't also restrict the quantified
variable's domain to this range, then Vehicle will produce spurious
counter-examples to your specification.

In general such restrictions can be achieved by combining a quantifier
with an implication as follows:

.. code-block:: agda

   forall x . 0 <= x <= 1 => f x ! 0 > 0

Quantifying over finite domains
-------------------------------

While most specifications will quantify over at least one variable
with an infinite domain, sometimes one might also want to quantify
over a finite set of values. This change be achieved by
modifying the quantifier with the :code:`in` keyword to quantify over
all the values contained within a :code:`List` or a :code:`Tensor`:

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

The one remaining case is quantifying over the indices of a tensor as follows:

.. code-block:: agda

   pointwiseLess : Tensor Rat [3] -> Tensor Rat [3] -> Bool
   pointwiseLess x y = forall i . x ! i < y ! i

which will get automatically expanded to:

.. code-block:: agda

   pointwiseLess : Tensor Rat [3] -> Tensor Rat [3] -> Bool
   pointwiseLess x y = forall i in [0, 1, 2] . x ! i < y ! i

The :code:`individual` keyword
------------------------------

A common use of the :code:`forall ... in ...` construct is to quantify
over a dataset, for example as follows:

.. code-block:: agda

   dataset dataset : List (Tensor Rat [784])

   ...

   robust : Bool
   robust = forall x in dataset . robustAround x

The problem with this formulation of the specification is that Vehicle
will only report whether the network is robust around *all* the elements
in the dataset. This is unlikely to be true.

Instead it is possible to modify the quantifier with the :code:`individual`
keyword, which will result in Vehicle reporting how many and which of the
elements in the dataset the network is robust around:

.. code-block:: agda

   dataset trainingDataset : List (Tensor Rat [784])

   ...

   robust : Bool
   robust = forall individual x in trainingDataset . robustAround x

The :code:`individual` keyword can be added to any quantified variable
that ranges over a finite domain.

Limitations
~~~~~~~~~~~

Currently quantifiers that use the :code:`individual` keyword must
be at the top level of a property. For example, the following is
allowed:

.. code-block:: agda

   dataset trainingDataset : List Rat

   monotonic : Bool
   monotonic = forall individual x in trainingDataset . f x <= 0

but this is not:

.. code-block:: agda

   dataset trainingDataset : List Rat

   property : Bool
   property = f 0.2 <= 3 or forall individual x in trainingDataset . f x <= 0

Another restriction is that when quantifying over multiple variables
at once with the :code:`individual` keyword, all the variables must have
finite domains. For example the following is allowed:

.. code-block:: agda

   tensor : Tensor Rat [2]
   tensor = [0.1, 0.2, 0.3]

   monotonic : Bool
   monotonic = forall individual i j . i < j => f (tensor ! i) <= f (tensor ! j)

but the following is not:

.. code-block:: agda

   monotonic : Bool
   monotonic = forall individual i x . f x < tensor ! i

Instead the latter must be written as:

.. code-block:: agda

   tensor : Tensor Rat [3]
   tensor = [0.1, 0.2, 0.3]

   monotonic : Bool
   monotonic = forall individual i . forall x . f x < tensor ! i
