Type declarations
=================

Although Vehicle's builtin types are sufficient to write a wide range
of specifications, specifications can often be made more readable by using
the :code:`type` keyword to defining meaningful synonyms for those that are
used repeatedly.

For example, when defining a robustness specification for the MNIST dataset
which contains 24x24 greyscale images, in order to avoid having to repeatedly
write :code:`Tensor Rat [24, 24]`, one could declare :code:`Image` as a
synonym for it and use it as follows:

.. code-block:: agda

  type Image = Tensor Rat [24, 24]

  network classify : Image -> Tensor Rat [10]

  dataset trainingDataset : List Image

  robustAround : Image -> Bool
  robustAround x = ...

Declared types can also have arguments. For example we
can declare :code:`Vector` as a synonym for 1-dimensional tensors as follows:

.. code-block:: agda

  type Vector A n = Tensor A [n]

  network classify : Vector Rat 10 -> Vector Rat 1
