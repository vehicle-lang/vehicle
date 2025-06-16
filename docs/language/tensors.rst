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
that represent its dimensions. For example ``Tensor Rat [24, 24]`` would be
a 24-by-24 matrix of rational numbers.

Note that a 0-dimensional tensor is equivalent to the raw value in Vehicle,
e.g. the type ``Tensor Rat []`` can be used interchangably as `Rat`.

Creation
--------

As tensors are really just vectors underneath the hood, they can be
created by the same three mechanisms:

#. Use the same syntax as lists, e.g. the 2-by-2 identity matrix can
   be defined as follows:

   .. code-block:: agda

     identity : Tensor Rat [2, 2]
     identity = [ [1, 0], [0, 1] ]

   As with the ``Vector`` type, the type-checker will ensure that all tensors are of the correct size.
   For example, the following would result in an error:

   .. code-block:: agda

     identity : Tensor Rat [2, 2]
     identity = [ [1, 0, 1] , [0, 1, 1] ]

   as the second dimension is ``2`` but three elements have been provided.

#. The ``foreach`` syntax:

   .. code-block:: agda

     identity : Tensor Rat [1000,1000]
     identity = foreach i j . if i == j then 1 else 0

#. The final way tensors can be created is to load them as a ``dataset``, e.g.

   .. code-block:: agda

     @dataset
     myLargeTensor : Tensor Rat [10000, 10000]

   See the section on datasets for more details.

Operations
----------

The following operations over tensors are currently supported:

.. list-table::
   :widths: auto
   :header-rows: 1

   * - Operation
     - Symbol
     - Type
     - Example
     - Description
   * - Lookup
     - :code:`!`
     - :code:`Tensor A [d, ds] -> Index d -> Tensor A ds`
     - :code:`t ! i`
     - Extract the value at a given index of the tensor.
   * - Foreach
     - :code:`!`
     - :code:`(Index d -> Tensor A ds) -> Tensor A [d, ds]`
     - :code:`foreach i . 0`
     - Constructs a new tensor by specifying each outermost row in terms of the row's index.
   * - Comparisons
     - :code:`<=`
     | :code:`<`
     | :code:`>=`
     | :code:`>`
     - :code:`Tensor A ds -> Tensor A ds -> Bool`
     - :code:`t1 <= t2`
     - Check that all pairs of elements in the tensor satisfy the comparison.
   * - Pointwise comparisons
     - :code:`.<=`
     | :code:`.<`
     | :code:`.>=`
     | :code:`.>`
     - :code:`Tensor A ds -> Tensor A ds -> Tensor Bool ds`
     - :code:`t1 .<= t2`
     - Compare all the elements of the tensor pointwise.
   * - Pointwise addition
     - :code:`+`
     - :code:`Tensor A ds -> Tensor A ds -> Tensor A ds`
     - :code:`t1 + t2`
     - Pointwise add the values in two tensors together. Only valid
       if addition is defined for the type of elements ``A``.
   * - Pointwise subtraction
     - :code:`-`
     - :code:`Tensor A ds -> Tensor A ds -> Tensor A ds`
     - :code:`t1 - t2`
     - Pointwise subtract the values in the first tensor from the values
       in the second. Only valid if subtraction is defined for the type of
       elements ``A``.


Non-constant dimensions
-----------------------

As with vectors, although the dimensions of a tensor are usually a
list of constants (e.g. ``[1, 2, 3]``), in practice they can be any
valid expression of type ``List Nat``.
For example:

  -  ``Tensor Rat [2 + d]`` is the type of vectors of length ``2 + d``.

  -  ``Tensor Rat (10 :: ds)`` is the type of tensors whose first dimension
     is of size 10 and then has remaining dimensions ``ds``.
