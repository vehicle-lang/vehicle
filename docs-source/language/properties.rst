Properties
==========

.. contents::
   :depth: 1
   :local:

Basics
------

A property is any self-contained statement that you wish to check using Vehicle.
Properties are declared by annotating declarations with the ``@property`` annotation:

.. code-block:: agda

   @property
   robust : Bool
   robust = forall x . f x ! 0 >= 0.0

Only declarations with the types ``Bool``, ``Vector Bool n`` or ``Tensor Bool n`` may
be annotated as properties. In the latter case, Vehicle will report the status of each
element of the ``Vector`` or ``Tensor`` individually.

The ``foreach`` quantifier
--------------------------

A common use of the ``forall`` quantifier is to assert that a
predicate holds over every element of a dataset, e.g.

.. code-block:: agda

   @dataset
   trainingDataset : List (Tensor Rat [28, 28])

   ...

   robust : Bool
   robust = forall x in trainingDataset . robustAround x

The problem with this formulation of the specification is that Vehicle
will only report whether the network is robust around *all* the elements
in the dataset. This is unlikely to be true.

Instead the ``foreach`` quantifier may be used. Instead of returning a
single value of type ``Bool`` it constructs a ``Tensor``
of ``Bool`` values. When used a property, Vehicle will therefore report
on the verification status of each individual element.

.. code-block:: agda

   @dataset
   trainingDataset : List (Tensor Rat [28, 28])

   ...

   robust : List Bool
   robust = foreach x in trainingDataset . robustAround x

Unlike the ``forall`` keyword, the ``foreach`` keyword cannot be used to
quantify over infinite types. It _can_ be used to quantify over ``Index``
types.
