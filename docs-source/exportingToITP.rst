Exporting a specification to an ITP
===================================

The end goal for most Vehicle projects is to verify that the neural network
obeys the specification. However, in some projects it is also desirable to use
the specification to prove that some larger development that *uses* the neural
network is also functionally correct.

Take the car example in the list of example Vehicle projects. Here the end
goal is to prove that the car being steered by the neural network never leaves
the road. This safety property is not just a property of the neural network, but
is instead a temporal property about the model of the entire system, e.g. the car,
the road, the physics of the system. The problem is that Vehicle is not designed
for modelling or reasoning about anything except neural networks.

To get around this, Vehicle is capable of exporting a verified specification to
*Interactive Theorem Provers* (ITPs), which are much more general purpose systems
that are capable of formalising and reasoning about arbitrary code and models.

This can be done using the :code:`vehicle export` command:

.. code-block:: bash

  vehicle export \
    --target Agda \
    --proofCache examples/windController/windController.vcl-cache \
    --outputFile examples/windController/agdaProof/WindControllerSpec.agda

Command-line options
--------------------

The table below contains the full list of command line arguments available
for the :code:`export` command.

.. option:: --target, -t

    Set which ITP to export the specification to.
    At the moment the only supported option is :code:`Agda`.

.. option:: --proofCache, -c

    Provide Vehicle the location of the proof cache from which the
    exported specification should be generated. If not provided then
    all ``@property`` declarations will be converted into ``postulates``.

.. option:: --outputFile, -o

    Set the name and location of the generated output file.

.. option:: --modulePrefix, -m

    Set the prefix for the generated module. For example, setting  to
    ``Baz.agda`` with a prefix of ``Foo.Bar`` will result in the Agda module
    with the name `Foo.Bar.Baz`."

Supported backends
------------------

Currently only the Agda interactive theorem prover is supported, but adding
support for new ones should be relatively simple, assuming that they have
the ability to call out to external solvers. Please get in touch if you are
interested in adding support for a new ITP.

Agda
~~~~

The Agda backend generates a new Agda module with the functions in the
specification lifted to the Agda type :code:`Set`. The proofs of the
properties are provided by a macro called :code:`checkSpecification`.
This calls Vehicle in :code:`check` mode on the proof cache, which
then checks the status of the specification. Consequently no
expensive reverification occurs when you try to type-check the Agda
module.

The generated Agda module provides a full interface which can then be
used to build and prove properties about a model of the larger system.
See the car example project for a demonstration of this.

Limitations
***********

Postulated resources
####################

When exporting a specification, the parameters are inserted into the
Agda version of the specification but at the moment the networks and
datasets are left as postulates. This allows them to be used and
programmed with abstractly but not to be evaluated or inspected.

This is a not a fundemental limitation, as it would be possible to
gain the ability to evaluate them by implementing an Agda tactic
that martialed the arguments, and then unmartialed results. This would
however be a significant undertaking.

Poor interaction with Agda's caching mechanism
##############################################

Due to its caching machinary, currently Agda only calls the
:code:`checkSpecification` macro when first type-checking the
generated module or its contents changes. This means that it won't
automatically detect changes to the backing network within a single
Agda session. See `issues #73 <https://github.com/vehicle-lang/vehicle/issues/73>`_
for an upstream link to the proposed fix on Agda's end to guarantee
the macro gets called every time the generated module is type-checked.
