Properties (TODO)
=================

.. contents::
   :depth: 1
   :local:

Properties are any declaration with the type ``Bool``, ``Vector Bool n`` or
``Tensor Bool n``.

The ``foreach`` quantifier
++++++++++++++++++++++++++

Finally a common use of the ``forall`` quantifier is to assert that the
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
single value of type ``Bool`` it constructs a ``Tensor``
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