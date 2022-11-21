Datasets
========

.. contents::
   :depth: 1
   :local:

Dataset declarations allow specifications to reference external data at scale,
without having to replicate it manually in the specification.

Basics
------

Datasets are declared as follows using the ``@dataset`` annotation:

.. code-block:: agda

   @dataset
   myDataset : Tensor Rat [28, 28]

Datasets can be any type :code:`t` that can be constructed from the following
grammar:

.. code-block:: agda

   t ::= List t | Vector t n | Tensor t ns | s
   s ::= Index n | Nat | Int | Rat

where :code:`n` is a known constant and :code:`ns` is a list of known constants.

Once declared, datasets can be used as any other named ``List``, ``Vector`` or ``Tensor``
would be, e.g.

.. code-block:: agda

   forall x in myDataset . robustAround x

Supported formats
-----------------

The actual implementation of the dataset is provided when using the
specification during training or verification.

At the moment only the ``IDX`` format is supported. A full description of this
format can be found at the bottom of this `page <http://yann.lecun.com/exdb/mnist/>`_.

There are numerous libraries for converting datasets into this format:
 - Python - https://github.com/ivanyu/idx2numpy
 - C++ - https://github.com/JohnnyVM/idx-library
 - Haskell - https://hackage.haskell.org/package/mnist-idx

If you would be interested in other formats being supported, please get in touch.

Multiple, related datasets
--------------------------

Notice that the type restrictions on datasets means that you can't currently
have heterogeneously-typed data within a single dataset.
However, a common use case for heterogenous datasets is to import a training
dataset with both training examples and training labels.

An explanation of how to achieve this with inferrable parameters is given in
the parameters documentation.
