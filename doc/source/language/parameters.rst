Parameters
==========

There are several reasons that one might want to set the value of
a constant at compile time rather than when writing the spec. For example,
the value may not be known ahead of time, or you might want to reuse the
same spec with multiple different values e.g. assign different values
for epsilon in a robustness specification.

Basics
------

Parameters can be declared using the :code:`parameter` keyword as follows:

.. code-block:: agda

   parameter epsilon : Rat

Similar to networks and datasets, parameters are passed in at compile time via
the :code:`--parameter` command line option. For example setting :code:`epsilon` to
the value :code:`0.1` can be achieved using :code:`--parameter epsilon:0.1`.

Unlike networks and datasets, there are no restrictions on the type of
parameters and, in theory, their value can be any valid Vehicle expression.
However, in practice we recommend to restrict to passing numeric or boolean
constants.

Implicit parameters
-------------------

Sometimes the value of the parameter can be inferred from other parts of the
specification, but is still inconvenient to pass in at compile time.
For example, a common scenario is that the specification contains two datasets,
one that contains the training or test inputs, and one that contains the
corresponding outputs.
It is important to guarantee that the two datasets have the same size in their
first dimension, but you don't want to specify the size upfront as you may add
some more data later.

The solution in this case is to use an *implicit* parameter as follows:

.. code-block:: agda

   implicit parameter n : Nat

   dataset trainingInputs  : Tensor Rat [n, 10]
   dataset trainingOutputs : Tensor Rat [n, 3]

Unlike normal parameters, implicit parameters cannot be passed in at the
command line using the ``--parameter`` option.
Instead the compiler will try to automatically infer their value.

For example, in the code above if the datasets passed in at compile time both
have 20 training samples then the compiler will deduce that the value of ``n``is 20.
If the datasets are of different sizes then compilation will fail with a
suitable error.

