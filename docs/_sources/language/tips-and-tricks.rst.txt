Tips and tricks
===============

.. contents::
   :depth: 1
   :local:

Naming indices
--------------

When individual indices of the input and output vectors have meaningful
semantics, the readability of the spec can be significantly enhanced by
assigning names to the indices.

For example consider a network responsible for controlling an agent
navigating in a grid based world. The agent can either move in one
of the 4 directions or stay still and, given some information about
the current state of the world, the network calculates a score for
each possible action.

.. code-block:: agda

   @network
   action : Tensor Rat [12] -> Tensor 5 [Rat]

Suppose the first and second outputs are the scores for staying still
and moving up respectively, and that we would like to encode the constraint
that in the state :code:`x` the network assigns a higher score to
staying still than moving up. This can be written as:

.. code-block:: agda

   action x ! 0 > action x ! 1

However, a reader of the spec will not know what indices 0 and 1
correspond to. Instead, by assigning names for these indices:

.. code-block:: agda

   stayStill = 0
   moveUp    = 1
   moveRight = 2
   moveDown  = 3
   moveLeft  = 4

this can be rewritten as:

.. code-block:: agda

   action x ! stayStill > action x ! moveUp

which is significantly more understandable.

Use relations, not computation
------------------------------

A general trick is to try and define your specification in terms of
relations rather than computation, especially computation that involves
conditional statements.

For example, suppose you had an image classifier that assigns 10 classes
a score between :code:`0` and :code:`1`

.. code-block:: agda

   network classify : Tensor Rat [24, 24] -> Tensor Rat [10]

and wanted to encode that it did not answer confidently
for some out-of-distribution input :code:`x`.
A "computational" approach to encoding this constraint would be to
calculate the maximum score and then require it to be less than 0.2:

.. code-block:: agda

   max : Rat -> Rat -> Rat
   max x y = if x <= y then x else y

   largestScore : Tensor Rat [10] -> Rat
   largestScore xs = fold max 0 xs

   isUncertainAbout : Tensor Rat [24, 24] -> Bool
   isUncertainAbout x = largestScore (classify x) <= 0.2

However, this definition would experience an exponential blow-up when
compiled down to low-level verification queries, as each branch of the
10 :code:`if` statements would need to be explored. In total 1024 queries
would be generated.

This blow-up can be avoided by instead using a "relational" approach to
encoding the constraint, instead stating that all classes scores must be less
than 0.2:

.. code-block:: agda

   isUncertainAbout : Tensor Rat [24, 24] -> Bool
   isUncertainAbout x = forall i . x ! i <= 0.2

In summary, prefer to use relations to express your constraints and
only perform computation and use :code:`if` statements when you absolutely
have to.

Useful functions
----------------

We will now describe some functions that are useful building blocks when
writing specifications.

:code:`argmin`
~~~~~~~~~~~~~~

.. code-block:: agda

   isArgmin : Index n -> Tensor Rat [n] -> Bool
   isArgmin i x = forall j . i != j => x ! i < x ! j

:code:`argmin`
~~~~~~~~~~~~~~

.. code-block:: agda

   isArgmax : Index n -> Tensor Rat [n] -> Bool
   isArgmax i x = forall j . i != j => x ! i > x ! j

:code:`advises`
~~~~~~~~~~~~~~~

For a classification task where the network produces a score
for each class and the class with the lowest score is chosen,
the definition :code:`isArgmin` can be extended as follows
to form a predicate that says the network advises the `i`th class
when applied to input `x`:

.. code-block:: agda

   @network
   classify : Tensor Rat [24, 24] -> Tensor Rat [10]

   advises : Index 10 -> Tensor Rat [24, 24] -> Bool
   advises i x = forall j . i != j => classify x ! i < classify x ! j
