Type declarations
=================

Although Vehicle comes with enough builtin types to write a wide range
of specifications, specifications can often be made more readable by using
the :code:`type` keyword to defining synonyms for types that are used repeatedly
throughout a specification.

For example, when defining a robustness specification for the MNIST dataset
which contains 24x24 greyscale images, in order to avoid having to repeatedly
write :code:`Tensor Rat [24, 24]`, you could declare the :code:`Image` type
as follows:

.. code-block:: agda

  type Image = Tensor Rat [24, 24]

  network classify : Image -> Tensor Rat [10]

  dataset trainingDataset : List Image

  robustAround : Image -> Bool
  robustAround x = ...

Declared types can also have arguments. For example we
can declare :code:`Vector` as a synonym for 1-dimensional tensors:

.. code-block:: agda

  type Vector A n = Tensor A [n]

  network classify : Vector Rat 10 -> Vector Rat 1
