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
   :widths: 15 10 30 15 30
   :header-rows: 1

   * - Operation
     - Symbol
     - Type
     - Example
     - Description
   * - Lookup
     - ``!``
     - ``Tensor A [d, ds] -> Index d -> Tensor A ds``
     - ``t ! i``
     - Extract the value at a given index of the tensor.
   * - Foreach
     - ``!``
     - ``(Index d -> Tensor A ds) -> Tensor A [d, ds]``
     - ``foreach i . 0``
     - Constructs a new tensor by specifying each outermost row in terms of the row's index.
   * - Comparisons
     - | ``<=``
       | ``<``
       | ``>=``
       | ``>``
       | ``==``
       | ``!=``
     - ``Tensor A ds -> Tensor A ds -> Bool``
     - ``t1 <= t2``
     - Check that all pairs of elements in the tensor satisfy the comparison.
   * - Pointwise comparisons
     - | ``.<=``
       | ``.<``
       | ``.>=``
       | ``.>``
       | ``.==``
       | ``.!=``
     - ``Tensor A ds -> Tensor A ds -> Tensor Bool ds``
     - ``t1 .<= t2``
     - Compare all the elements of the tensor pointwise.
   * - Pointwise addition
     - ``+``
     - ``Tensor A ds -> Tensor A ds -> Tensor A ds``
     - ``t1 + t2``
     - Pointwise add the values in two tensors together. Only valid
       if addition is defined for the type of elements ``A``.
   * - Pointwise subtraction
     - ``-``
     - ``Tensor A ds -> Tensor A ds -> Tensor A ds``
     - ``t1 - t2``
     - Pointwise subtract the values in the first tensor from the values
       in the second. Only valid if subtraction is defined for the type of
       elements ``A``.


Transpose
---------

``transpose t`` reverses the dimension order of a tensor:
``Tensor A [d_1, ..., d_n]`` becomes ``Tensor A [d_n, ..., d_1]``.
For a 2-tensor this is the standard matrix transpose.

A transpose that is indexed down to a scalar — ``(transpose t) ! i_1 ! ... ! i_n``
— is eliminated at compile time (it equals ``t ! i_n ! ... ! i_1``). Constant
tensors (``transpose (const v ds)``) and rank-2 list literals
(``transpose [[1.0, 2.0], [3.0, 4.0]]``) are folded directly.

Backend support
~~~~~~~~~~~~~~~

- Verifier (Marabou / VNN-LIB): any rank.
- Loss: any rank.
- Agda, Isabelle ITP: any rank.
- Imandra ITP: any rank, for ``Real``, ``Bool`` and ``Nat`` element types.
- Rocq ITP: rank 2 only. Targeting Rocq with a higher-rank transpose is an
  error.


Non-constant dimensions
-----------------------

As with vectors, although the dimensions of a tensor are usually a
list of constants (e.g. ``[1, 2, 3]``), in practice they can be any
valid expression of type ``List Nat``.
For example:

  -  ``Tensor Real [2 + d]`` is the type of vectors of length ``2 + d``.

  -  ``Tensor Real (10 :: ds)`` is the type of tensors whose first dimension
     is of size 10 and then has remaining dimensions ``ds``.
