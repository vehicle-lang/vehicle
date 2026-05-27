Tensors
=======

.. contents::
   :depth: 1
   :local:

Basics
------

Tensors are the basic abstraction underlying most neural network libraries.
If the ``Vector`` type can be thought of as representing a fixed-length
array, then the ``Tensor`` type can be thought of as a multi-dimensional array.

Tensor types are written as ``Tensor A ds`` where ``A`` is the type
of data stored within the tensor and ``ds`` is a list of natural numbers
that represent its dimensions. For example ``Tensor Real [24, 24]`` would be
a 24-by-24 matrix of real numbers.

Note that a 0-dimensional tensor is equivalent to the raw value in Vehicle,
e.g. the type ``Tensor Real []`` can be used interchangably as `Real`.

Creation
--------

As tensors are really just vectors underneath the hood, they can be
created by the same three mechanisms:

#. Use the same syntax as lists, e.g. the 2-by-2 identity matrix can
   be defined as follows:

   .. code-block:: agda

     identity : Tensor Real [2, 2]
     identity = [ [1, 0], [0, 1] ]

   As with the ``Vector`` type, the type-checker will ensure that all tensors are of the correct size.
   For example, the following would result in an error:

   .. code-block:: agda

     identity : Tensor Real [2, 2]
     identity = [ [1, 0, 1] , [0, 1, 1] ]

   as the second dimension is ``2`` but three elements have been provided.

#. The ``foreach`` syntax:

   .. code-block:: agda

     identity : Tensor Real [1000,1000]
     identity = foreach i j . if i == j then 1 else 0

#. The final way tensors can be created is to load them as a ``dataset``, e.g.

   .. code-block:: agda

     @dataset
     myLargeTensor : Tensor Real [10000, 10000]

   See the section on datasets for more details.

Operations
----------

The following operations over tensors are currently supported:

.. list-table::
   :widths: 11 22 34 33
   :header-rows: 1

   * - Operation
     - Syntax
     - Type
     - Support
   * - Lookup
     - ``e ! i``
     - | ``Tensor A [d, ds] →``
       | ``Index d →``
       | ``Tensor A ds``
     - |backendall_full|
   * - Foreach
     - ``foreach i . e``
     - | ``(Index d → Tensor A ds) →``
       | ``Tensor A [d, ds]``
     - | |backendloss_part|
       | |backendverification_full|
       | |backendagda_full|
       | |backendrocq_full|
       | |backendimandra_full|
       | |backendisabelle_full|
   * - Comparisons
     - | ``x < y``
       | ``x > y``
       | ``x <= y``
       | ``x >= y``
       | ``x == y``
       | ``x != y``
     - | ``Tensor A ds →``
       | ``Tensor A ds →``
       | ``Bool``
       | (if ``A`` supports comparisons)
     - |backendall_full|
   * - | Pointwise
       | comparisons
     - | ``x .< y``
       | ``x .> y``
       | ``x .<= y``
       | ``x .>= y``
       | ``x .== y``
       | ``x .!= y``
     - | ``Tensor A ds →``
       | ``Tensor A ds →``
       | ``Tensor Bool ds``
       | (if ``A`` supports comparisons)
     - |backendall_full|
   * - | Pointwise
       | and
     - ``t1 and t2``
     - | ``Tensor Bool ds →``
       | ``Tensor Bool ds →``
       | ``Tensor Bool ds``
     - |backendall_full|
   * - | Pointwise
       | or
     - ``t1 or t2``
     - | ``Tensor Bool ds →``
       | ``Tensor Bool ds →``
       | ``Tensor Bool ds``
     - |backendall_full|
   * - | Reduce
       | and
     - ``reduceAnd e t``
     - | ``Bool →``
       | ``Tensor Bool ds →``
       | ``Bool``
     - |backendall_full|
   * - | Reduce
       | or
     - ``reduceOr e t``
     - | ``Bool →``
       | ``Tensor Bool ds →``
       | ``Bool``
     - |backendall_full|
   * - | Pointwise
       | add
     - ``t1 + t2``
     - | ``Tensor A ds →``
       | ``Tensor A ds →``
       | ``Tensor A ds``
       | (if ``A`` supports ``+``)
     - |backendall_full|
   * - | Pointwise
       | subtract
     - ``t1 - t2``
     - | ``Tensor A ds →``
       | ``Tensor A ds →``
       | ``Tensor A ds``
       | (if ``A`` supports ``-``)
     - |backendall_full|
   * - | Pointwise
       | multiply
     - ``t1 * t2``
     - | ``Tensor A ds →``
       | ``Tensor A ds →``
       | ``Tensor A ds``
       | (if ``A`` supports ``*``)
     - |backendall_full|
   * - | Pointwise
       | divide
     - ``t1 / t2``
     - | ``Tensor A ds →``
       | ``Tensor A ds →``
       | ``Tensor A ds``
       | (if ``A`` supports ``/``)
     - |backendall_full|
   * - | Reduce
       | add
     - ``reduceAdd e t``
     - | ``A → Tensor A ds → A``
       | (if ``A`` supports ``+``)
     - |backendall_full|
   * - | Reduce
       | multiply
     - ``reduceMul e t``
     - | ``A → Tensor A ds → A``
       | (if ``A`` supports ``*``)
     - |backendall_full|
   * - | Reduce
       | min
     - ``reduceMin e t``
     - | ``A → Tensor A ds → A``
       | (if ``A`` supports ``min``)
     - |backendall_full|
   * - | Reduce
       | max
     - ``reduceMax e t``
     - | ``A → Tensor A ds → A``
       | (if ``A`` supports ``max``)
     - |backendall_full|



Non-constant dimensions
-----------------------

As with vectors, although the dimensions of a tensor are usually a
list of constants (e.g. ``[1, 2, 3]``), in practice they can be any
valid expression of type ``List Nat``.
For example:
  -  ``Tensor Real [2 + d]`` is the type of vectors of length ``2 + d``.

  -  ``Tensor Real (10 :: ds)`` is the type of tensors whose first dimension
     is of size 10 and then has remaining dimensions ``ds``.
