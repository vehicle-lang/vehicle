Vectors
=======

.. contents::
   :depth: 1
   :local:

Basics
------

The ``Vector`` type represents a mathematical vector, or in programming
terms can be thought of as a fixed-length array.
One potentially unusual aspect in Vehicle is that the size of the vector
(i.e the number of items it contains) must be known statically
at compile time. This allows Vehicle to check for the presence of
out-of-bounds errors at compile time rather than run time.

The full type is therefore written as ``Vector A n``, which
represents the type of vectors with ``n`` elements of type ``A``.
For example, ``Vector Rat 10`` is a vector of length 10 that contains
rational numbers,  and ``Vector (List Nat) 2`` is a vector of length 2
that contains lists of natural numbers.

Creation
--------

There are three ways to create an instance of a vector:

#. Use the same syntax as when creating a ``List``, i.e. ``[x_1, ..., x_n]``.
   For example:

   .. code-block:: agda

     myVector1 : Vector Rat 3
     myVector1 = [ 0.1, 0.3, -0.1 ]

   The type-checker will ensure that all vectors written in this way are of
   the correct size. For example the following would result in an error:

   .. code-block:: agda

     myBadVector : Vector Rat 3
     myBadVector = [ 0.1, 0.3 ]

   as the size of the vector is ``3`` but only 2 elements have been provided.

   While it is possible to use this method to write out small vectors,
   writing out large vectors this way is clearly impractical.

#. Therefore the second method is to use the ``foreach`` constructor,
   which is used to provide a value for each index ``i``. This method is
   useful if the vector has some regular structure. For example:

   .. code-block:: agda

     myVector2 : Vector Rat 100
     myVector2 = foreach i . 0

   constructs a vector of 100 rationals all of which are ``0``, and

   .. code-block:: agda

     myVector3 : Vector Rat 100
     myVector3 = foreach i . if i < 20 then 1 else 0

   constructs a vector of 100 rationals where the first 20 elements are ``1``
   and the remaining are ``0``.

   When using the ``foreach`` to construct a vector of size ``n``` the type
   of variable ``i`` is ``Index n``. See the Index section for more details.

#. The final way vectors can be created is to load them as a ``dataset``, e.g.

   .. code-block:: agda

     @dataset
     myVector4 : Vector Rat 10000

   which allows the loading of large vectors with no regular structure.
   See the section on datasets for more details.

Operations
----------

The following operations over vectors are currently supported:

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
     - :code:`Vector A d -> Index d -> A`
     - :code:`v ! i`
     - Extract the value at a given index of the vector.
   * - Map
     - :code:`map`
     - :code:`(A -> B) -> Vector A d -> Vector B d`
     - :code:`map f v`
     - Apply the function ``f`` to every value in the vector.
   * - Addition
     - :code:`+`
     - :code:`Vector A d -> Vector A d -> Vector A d`
     - :code:`v1 + v2`
     - Pointwise add the values in two vectors together. Only valid
       if addition is defined for the type of elements ``A``.
   * - Subtraction
     - :code:`-`
     - :code:`Vector A d -> Vector A d -> Vector A d`
     - :code:`v1 - v2`
     - Pointwise subtract the values in the first vector from the values
       in the second. Only valid if subtraction is defined for the type of
       elements ``A``.
   * - Fold
     - :code:`fold`
     - :code:`(A -> B -> B) -> B -> Vector A d -> B`
     - :code:`fold f e v`
     - Reduce the vector to a single value by iterating the function `f`
       repeatedly with the head of the vector.

Indexing
--------

The type of the lookup operator ``!`` given above requires that it
takes a value of type ``Index d`` as its second argument.
The set of valid instances of this type are the natural numbers
``{0, 1, ..., d-1}``.
This therefore eliminates out of bounds errors by ensuring that
one can never index into a vector using a value greater than its
size.

Indices can be written as any other natural number would be, and
the type-checker will automatically infer they should be of type
``Index`` from their use.

For example:

.. code-block:: agda

   @dataset
   myVector : Vector Rat [10]

   firstElement : Rat
   firstElement = myVector ! 0

is valid but the following is not as ``10`` is out of bounds:

.. code-block:: agda

   invalidElement : Rat
   invalidElement = myVector ! 10

Most arithmetic operations over ``Index`` type are not closed with
respect to the type, e.g. adding ``3 : Index 5`` and ``4 : Index 5``
results in ``7`` which is not a member of ``Index 5``. Consequently
the set of operations supported by ``Index`` types is extremely limited:

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

Non-constant sizes
------------------

Although the size of a vector is usually a constant (e.g. ``10``),
Vehicle allows them to be any valid expression of type ``Nat``.
For example if ``d`` is some other variable then:

  - ``Vector Rat (1 + d)`` is the type of vectors of length ``1 + d``.

  - ``Vector Rat (2 * d)`` is the type of vectors of length ``2 * d``.

Similarly, the size of the ``Index`` type can be any valid expression of
type ``Nat``, e.g. ``Index (1 + d)``.
