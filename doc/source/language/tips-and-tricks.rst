Tips and tricks
===============

.. autosummary::
   :toctree: generated

Naming indices
--------------

When individual indicies of the input and output vectors have meaningful
semantics, the readability of the spec can be significantly enhanced by
assigning names to the indices.

For example consider a network responsible for controlling an agent
navigating a simple grid based world in which it can move in one of the
4 directions or stay still.

.. code-block:: agda

   network action : Tensor Rat [12] -> Tensor 5 [Rat]

The network calculates a score for each possible action, given some
information about the state of the world. For example if the first
and second outputs are the scores for staying still and moving up
respectively, then the constraint that when applied to some input `x`
the network assigns a higher score to staying still then moving up
can then be written as:

.. code-block:: agda

   action x ! 0 > action x ! 1

However, by assigning names for these indices:

.. code-block:: agda

   stayStill = 0
   moveUp    = 1
   moveRight = 2
   moveDown  = 3
   moveLeft  = 4

this can be rewritten as:

.. code-block:: agda

   action x ! stayStill > action x ! moveUp

which is significantly more understandable for a new reader of the spec.

Use relations, not computation
------------------------------

A general trick is to try and define your specification in terms of
relations rather than computation, especially computation that involves
conditional statements which can cause an exponential blow-up in the
number of verification queries.

For example, suppose you had a classifier that assigns each of 10 classes
a score between :code:`0` and :code:`1`

.. code-block:: agda

   network classify : Tensor Rat [24, 24] -> Tensor Rat [10]

and wanted to express that the classifier was never answered confidently
for some nonsense input `x`. A "computational" approach to encoding this
constraint would be to calculate the maximum score and then require it
to be less than 0.2 as follows:

.. code-block:: agda

   max : Rat -> Rat -> Rat
   max x y = if x <= y then x else y

   maxOf : Tensor Rat [10] -> Rat
   maxOf xs = foldr max 0 xs

   isUncertainAbout : Tensor Rat [24, 24] -> Bool
   isUncertainAbout x = maxOf (classify x) <= 0.2

However, this definition would experience an exponential blow-up when
compiled down to low-level verification queries, as each branch of the
10 :code:`if` statements would need to be explored. In total 1024 queries
would be generated.

This blow-up can be avoided by instead using a "relational" approach to
encoding the constraint by stating that all classes scores must be less
than 0.2:

.. code-block:: agda

   isUncertainAbout : Tensor Rat [24, 24] -> Bool
   isUncertainAbout x = forall i . x ! i <= 0.2

In summary, only perform computation and use :code:`if` statements when
you absolutely have to.


Useful functions
----------------

We will now describe some functions that are useful building blocks for
building up specifications.

- Argmin and argmax

.. code-block:: agda

   isArgmin : forallT {n} . Fin n -> Tensor A n -> Bool
   isArgmin i x = forall j . x ! i < x ! j

   isArgmax : forallT {n} . Fin n -> Tensor A n -> Bool
   isArgmax i x = forall j . x ! i > x ! j

- For a classification task where the network produces a score
for each class and the class with the lowest score is chosen,
the definition :code:`isArgmin` can be extended as follows
to form a predicate that says the network advises the `i`th class
when applied to input `x`:

.. code-block:: agda

   network classify : Tensor Rat [24, 24] -> Tensor Rat [10]

   advises : Fin 5 -> Tensor Rat [24, 24] -> Bool
   advises i x = forall j . classify x ! i < classify x ! j