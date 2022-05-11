Datasets
========

.. autosummary::
   :toctree: generated

There are two main reasons a specification may want to reference some external
dataset. The first is that the specification works with bounds over a large
number of
Many specifications

Basics
------

Datasets are declared as follows using the :code:`dataset` keyword:

.. code-block:: agda

   dataset myDataset : Tensor Rat [784]

Datasets should either be of the type :code:`List` or :code:`Tensor`, and can
contain elements of any numeric type,
e.g. :code:`Index n`, :code:`Nat`, :code:`Int`, :code:`Rat` etc,

Once declared, datasets can be used as any other named ::code:`List` or :code:`Tensor`
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