Differentiable Logics
=====================

|backendall_full|

.. contents::
   :depth: 1
   :local:

Basics
------

Vehicle's loss function backend allows you to translate logical specifications into
differentiable loss functions that can be used to train neural networks.
This is achieved by using a *Differentiable Logic* which maps the Boolean operations such as ``and``, ``or``, ``not`` in the specification to real-valued operations.

A differentiable logic has the following type signature:

.. code-block:: agda

  record DifferentiableTensorLogic where
    { trueElement               : Real
    , falseElement              : Real
    , pointwiseLessThan         : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
    , pointwiseLessEqualThan    : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
    , pointwiseGreaterThan      : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
    , pointwiseGreaterEqualThan : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
    , pointwiseEqual            : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
    , pointwiseNotEqual         : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
    , pointwiseNegation         : Tensor Real dims -> Tensor Real dims
    , pointwiseConjunction      : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
    , pointwiseDisjunction      : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
    , reduceConjunction         : Real -> Tensor Real dims -> Real
    , reduceDisjunction         : Real -> Tensor Real dims -> Real
    }

While Vehicle comes with several built-in differentiable logics, you can also define your own custom differentiable logic by implementing the `DifferentiableTensorLogic` interface.

For example:

.. code-block:: agda

  CustomDL : DifferentiableTensorLogic
  CustomDL =
    { trueElement                = 0
    , falseElement               = 1000000
    , pointwiseLessThan          = \{dims} x y -> max (const 0 dims) (x - y)
    , pointwiseLessEqualThan     = \{dims} x y -> max (const 0 dims) (x - y)
    , pointwiseGreaterThan       = \{dims} x y -> max (const 0 dims) (y - x)
    , pointwiseGreaterEqualThan  = \{dims} x y -> max (const 0 dims) (y - x)
    , pointwiseEqual             = \{dims} x y -> - (max (const 0 dims) (x - y) + max (const 0 dims) (y - x))
    , pointwiseNotEqual          = \{dims} x y -> (max (const 0 dims) (x - y) + max (const 0 dims) (y - x))
    , pointwiseNegation          = \{dims} x -> (const 1 dims) / x
    , pointwiseConjunction       = \x y -> x + y
    , pointwiseDisjunction       = \x y -> x * y
    , reduceConjunction          = \e xs -> reduceAdd e xs
    , reduceDisjunction          = \e xs -> reduceMul e xs
    }

Notes on the logic above:
  1. Vehicle does not yet currently support infinite values, so we use a large constant to represent falsehood.
  2. Strict and non-strict inequalities are treated the same in this logic, but you could define them differently if desired.

This custom logic can then be used in the loss function backend as described in :doc:`../training`.
