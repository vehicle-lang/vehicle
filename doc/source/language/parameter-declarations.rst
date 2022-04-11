Parameter declarations
======================

There are several reasons that one might want to set the value of 
a constant at compile time rather than when writing the spec. For example, 
the value may not be known ahead of time, or you might want to reuse the
same spec with multiple different values e.g. assign different values 
for epsilon in a robustness specification.

This can be achieved using the `parameter` keyword:
.. code-block:: agda

   parameter epsilon : Rat

Like networks and datasets, parameters are passed in at compile time via
the `--parameter` command line option. For example setting `epsilon` to
the value `0.1` can be achieved using `--parameter epsilon:0.1`.

Unlike networks and datasets, there are no restrictions on the type of
parameters and, in theory, their value can be any valid Vehicle expression.
However, in practice we recommend to restrict to passing numeric or boolean
constants.