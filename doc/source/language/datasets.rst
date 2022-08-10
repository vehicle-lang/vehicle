Datasets
========

.. contents::
   :depth: 1
   :local:

There are two main reasons a specification may want to reference some external
dataset. The first is that the specification works with bounds over a large
number of
Many specifications

Basics
------

Datasets are declared as follows using the :code:`dataset` annotation:

.. code-block:: agda

   @dataset
   myDataset : Tensor Rat [784]

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