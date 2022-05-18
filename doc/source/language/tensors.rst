Tensors
=======

.. contents::
   :depth: 1
   :local:

Tensors, which can be though of as multi-dimensional arrays of data,
are the basic data type underlying most neural network abstractions.

Type
----

The tensor type is written as ``Tensor A dims`` where ``A`` is the type
of data stored within the tensor and ``dims`` is a list of natural numbers
that encode its dimensions. For example ``Tensor Rat [24, 24]`` would be
a 24-by-24 matrix of rational numbers.

A tensor type with an empty list of dimensions is equivalent to its element
type, e.g. ``Tensor Rat []`` is equivalent to ``Rat``.

The type of a tensor of tensors is equivalent to a type of tensors with
the dimensions concatenated, e.g. ``Tensor (Tensor Rat [24, 20]) [100]``
is equivalent to ``Tensor Rat [100, 24, 20]``.

Creation
--------

There are two ways to create instances of a tensor type.
The first is to use the same syntax as when creating a ``List``,
i.e. ``[x_1, ..., x_n]``.
For example, the 2-by-2 identity matrix can be defined as follows:

.. code-block:: agda

   identity : Tensor Rat [2, 2]
   identity = [ [1, 0], [0, 1] ]

The type-checker will ensure that all tensors are of the correct size.
For example, the following would result in an error:

.. code-block:: agda

   identity : Tensor Rat [1, 3]
   identity = [ [1, 0, 1] , [0, 1, 1] ]

as the first dimension is ``1`` but two elements have been provided.

Manually writing out large tensors using this syntax is clearly infeasible.
Therefore the second way tensors can be created is to load them as a
dataset, e.g.

.. code-block:: agda

   dataset myLargeTensor : Tensor Rat [10000, 10000]

See the section on datasets for more details.

Operations
----------

The following operations over tensors are currently supported:

.. list-table::
   :widths: 15 12 53 20
   :header-rows: 1

   * - Operation
     - Symbol
     - Type
     - Example
   * - Lookup
     - :code:`!`
     - :code:`Tensor A (d :: ds) -> Index d -> Tensor A ds`
     - :code:`t ! i`
   * - Fold
     - :code:`fold`
     - :code:`(Tensor A ds -> B -> B) -> B -> Tensor A (d : ds) -> B`
     - :code:`fold f 0 t`

Indexing
--------

The type of the lookup operator ``!`` given above requires that it
takes something of type ``Index d`` as its second argument.
The set of valid instances of this type are the natural numbers
``{0, 1, ..., d-1}``.
This therefore eliminates out of bounds errors by ensuring that
one can never index into a tensor using a value greater than the
size of the first dimension.

Indices can be written as any other natural number would be, and
the type-checker will automatically infer they should be of type
``Index`` from their use.

For example:

.. code-block:: agda

   dataset myTensor : Tensor Rat [10]

   firstElement : Rat
   firstElement = myTensor ! 0

is valid but the following is not as ``10`` is out of bounds:

.. code-block:: agda

   invalidElement : Rat
   invalidElement = myTensor ! 10

Most arithmetic operations over ``Index`` type are not closed with
respect to the type, e.g. adding ``3 : Index 5`` and ``4 : Index 5``
results in ``7`` which is not a member of ``Index 5``. Consequently
the set of operations supported by the type is extremely limited:

.. list-table::
   :widths: 25 15 40 20
   :header-rows: 1

   * - Operation
     - Symbol
     - Type
     - Example
   * - Less than or equal
     - :code:`<=`
     - :code:`Index d -> Index d -> Bool`
     - :code:`x <= y`
   * - Less than
     - :code:`<`
     - :code:`Index d -> Index d -> Bool`
     - :code:`x < y`
   * - Greater than or equal
     - :code:`>=`
     - :code:`Index d -> Index d -> Bool`
     - :code:`x >= y`
   * - Greater than
     - :code:`>`
     - :code:`Index d -> Index d -> Bool`
     - :code:`x >= y`

Non-constant dimensions
-----------------------

Although the dimensions of a tensor are usually a list of constants (e.g.
``[1, 2, 3]``), in reality they can be any valid expression of type ``List Nat``.
For example:
-  ``Tensor Rat [2 + d]`` is the type of vectors of length ``2 + d``.
-  ``Tensor Rat (10 :: ds)`` is the type of tensors whose first dimension
is of size 10 and then has remaining dimensions ``ds``.

Similarly, the size of the ``Index`` type can be any valid expression of
type ``Nat``, e.g. ``Index (2 + d)``.