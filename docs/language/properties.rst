Properties
==========

.. contents::
   :depth: 1
   :local:

Single properties
-----------------

A property is any self-contained statement that you wish to check using Vehicle.
Properties are declared by annotating declarations with the ``@property`` annotation:

.. code-block:: agda

   @property
   robust : Bool
   robust = forall x . f x ! 0 >= 0.0

Only declarations with the types ``Bool``, ``Vector Bool n`` or ``Tensor Bool n`` may
be annotated as properties. In the latter case, Vehicle will report the status of each
element of the ``Vector`` or ``Tensor`` individually.

Multiple properties
-------------------

Vehicle also supports marking vectors or tensors of Booleans as properties.
This is particularly useful when checking that a property holds over
every element of a dataset, e.g.

.. code-block:: agda

   @dataset
   trainingDataset : Vector (Tensor Real [28, 28]) 100

   ...

   @property
   robust : Vector Bool 100
   robust = foreach i . robustAround (x ! i)

When this specification is passed to the verification backend, Vehicle will report the
verifications status of each of hte individual elements of the dataset.
This is in contrast to if a ``forall`` quantifier was used, e.g.

.. code-block:: agda

   @property
   robust : Bool
   robust = forall i . robustAround (x ! i)

where Vehicle would only report whether the network is robust around *all* the elements
in the dataset.
