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

As might be expected, the ``Tensor`` is really just a convenient wrapper
around multiple ``Vector`` types, and therefore obeys the following laws:

#. A tensor type with an empty list of dimensions is equivalent to its element type, e.g. ``Tensor Rat []`` is equivalent to ``Rat``.

#. A tensor type with an non-empty list of dimensions ``d :: ds`` is equivalent to a vector of length ``d`` whose elements are tensors of dimensions ``ds``, e.g. ``Tensor Rat [2,3,4]`` is equivalent to ``Vector (Tensor Rat [3,4]) 2`` (and therefore ``Vector (Vector (Vector Rat 4) 3) 2)``).

Other laws follow directly from these two. For example:

- The type of a tensor of tensors is equivalent to a type of tensors with the dimensions concatenated, e.g. ``Tensor (Tensor Rat [24, 20]) [100]`` is equivalent to ``Tensor Rat [100, 24, 20]``.

Creation
--------

As tensors are really just vectors underneath the hood, they can be
created by the same three mechanisms:

#. Use the same syntax as lists, e.g. the 2-by-2 identity matrix can
   be defined as follows:

   .. code-block:: agda

     identity : Tensor Rat [2, 2]
     identity = [ [1, 0], [0, 1] ]

   Again the type-checker will ensure that all tensors are of the correct size.
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
   :widths: 15 12 38 15 20
   :header-rows: 1

   * - Operation
     - Symbol
     - Type
     - Example
     - Description
   * - Lookup
     - :code:`!`
     - :code:`Tensor A (d :: ds) -> Index d -> Tensor A ds`
     - :code:`t ! i`
     - Extract the value at a given index of the tensor.
   * - Map
     - :code:`map`
     - :code:`(Tensor A ds -> Tensor B ds) -> Tensor A (d :: ds) -> Vector B (d :: ds)`
     - :code:`map f t`
     - Apply the function ``f`` to every value in the tensor.
   * - Addition
     - :code:`+`
     - :code:`Tensor A ds -> Tensor A ds -> Tensor A ds`
     - :code:`t1 + t2`
     - Pointwise add the values in two tensors together. Only valid
       if addition is defined for the type of elements ``A``.
   * - Subtraction
     - :code:`-`
     - :code:`Tensor A ds -> Tensor A ds -> Tensor A ds`
     - :code:`t1 - t2`
     - Pointwise subtract the values in the first tensor from the values
       in the second. Only valid if subtraction is defined for the type of
       elements ``A``.
   * - Fold
     - :code:`fold`
     - :code:`(Tensor A ds -> B -> B) -> B -> Tensor A (d : ds) -> B`
     - :code:`fold f e v`
     - Reduce the tensor to a single value by iterating the function `f`
       repeatedly with the head of the tensor.


Non-constant dimensions
-----------------------

As with vectors, although the dimensions of a tensor are usually a
list of constants (e.g. ``[1, 2, 3]``), in practice they can be any
valid expression of type ``List Nat``.
For example:

  -  ``Tensor Rat [2 + d]`` is the type of vectors of length ``2 + d``.

  -  ``Tensor Rat (10 :: ds)`` is the type of tensors whose first dimension
     is of size 10 and then has remaining dimensions ``ds``.
