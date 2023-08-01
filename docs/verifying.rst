Verifying a specification
=========================

.. autosummary::
   :toctree: generated

Given a suitable specification, Vehicle can be used to check whether a particular
neural network satisfies it.

To do this, Vehicle relies on a type of tool called a *neural network verifier*.
As input, such tools take in a *query*, a set
of equality and inquality constraints relating the inputs and outputs of the neural network.
They then attempt to find to an assignment to the inputs that satisfy all the constraints
at the same time.
They can therefore be thought of as domain-specific SMT solvers.

However, these queries are written in a very low-level format and are often
extremely large. Furthermore they often don't support operations such as disjunction.
Consequently they are very difficult to write or read, and a single high-level
property often needs to be split up into many queries in a non-obvious manner.

However, Vehicle is capable of generating queries automatically from a specification
and verifying them using neural network verifiers.
Using the satisfiability results from the verifiers, Vehicle can then reconstruct the
truth status for the high-level original properties as well as witnesses and counter-examples.

Simple verification
-------------------

The simplest way to verify a specification is to GET Vehicle to both generate
the queries and perform the verification in a single step.
This may be performed using the ``verify`` mode and passing a ``.vcl`` file as
the value of the ``specification`` argument.

.. code-block:: bash

  vehicle verify \
    --specification my/project/mnist-robustness.vcl \
    --network classify:my/project/mnist.onnx \
    --dataset trainingImages:my/project/mnist-trainingImages.idx \
    --dataset trainingLabels:my/project/mnist-trainingLabels.idx \
    --parameter epsilon:0.1 \
    --verifier Marabou

The table below contains the full list of command line arguments available
for the ``verify`` command when ``specification`` is a ``.vcl`` file.

.. option:: --specification, -s

    The ``.vcl`` file containing the specification to compile.

.. option:: --property, -y

    The name of a property in the specification to verify. You may provide this
    option multiple times to verify multiple properties at once.
    If not provided, then by default all properties in the specification are compiled.
    All declarations provided via this option must be annotated with
    a ``@property`` annotation in the specification file.

.. option:: --network, -n

    Provide the implementation of a network declared in the specification.
    Its value should consist of a colon-separated pair of the name of the network
    in the specification and a file path, i.e.

    .. code-block:: bash

      --network NAME:FILEPATH

    Can be used multiple times if the specification involves more than one network.

.. option:: --dataset, -d

    Provide a dataset declared in the specification.
    Its value should consist of a colon-separated pair of the name of the dataset
    in the specification and a file path, i.e.

    .. code-block:: bash

      --dataset NAME:FILEPATH

    Can be used multiple times if the specification involves more than one dataset.

.. option:: --parameter, -p

    Provide a parameter referenced in the specification.
    Its value should consist of a colon-separated pair of the name of the parameter
    in the specification and its value, i.e.

    .. code-block:: bash

      --parameter NAME:VALUE

    Can be used multiple times to provide multiple parameters.

.. option:: --verifier, -v

    Which verifier should be used to perform the verification.
    At the moment the only supported option is :code:`Marabou`.

.. option:: --verifierLocation, -l

    Location of the executable for the verifier. If not provided, then Vehicle
    will search for the name of the executable in the ``PATH`` environment variable.

.. option:: --cache, -c

    The location to write out the verification cache that provides a permanent record
    of the results of the verification. See the sections below for more detail.

.. warning::

    The :code:`verify` command is not atomic.
    Verification involves repeatedly loading the network(s) from disk
    and Vehicle will not detect changes to the networks that occur
    while the command is running.

Advanced verification
---------------------

There are several reasons why one might want to check the verification status of
a specification some time after having initially called ``verify``:

  1. The verification could be part of an automated test suite in a continuous
  integration framework.

  2. The specification could have been exported to an interactive theorem prover
  whose workflow consists of regularly rechecking the validity of proofs.

Unfortunately, depending on the size of the network and the complexity of the
specification, verification can be a very expensive procedure taking hours or days.
Therefore, simply calling ``verify`` every time you want to check the status
of a specification may not be desirable.

Vehicle allows you to avoid unnecessary re-verification using the notion of
a __verification cache__.

Structure of a verification cache
+++++++++++++++++++++++++++++++++

Firstly, every verification cache contains:

- ``.vcl-cache-index`` - this file stores the critical information for the cache,
  including:
    - the list of properties contained within the cache
    - the file path and hash of any external resources that were used to create the
    cache. In particular the original specification, the networks and the datasets
    passed to the compiler.

Next, for each property named ``<property>`` in the original specification
the cache initially contains the following files:

- ``<property>-query1.txt``, ``<property>-query2.txt``, etc. - these files are the
  list of queries that need to passed to the verifier to ascertain whether the
  property holds or not.

- ``<property>.vcl-plan`` - this file contains all the information necessary
  to reconstruct the status of the property as written in the original
  specification file, from the results of the individual queries.

After verification, the cache will also contain the following files:

- ``<property>.vcl-result`` - this file is generated after verification and stores
  whether the property was found to be true or false.

- ``<property>-assignments`` - this folder contains `.idx` files that store
  any assignments found by the verifier for the infinite quantified variables in
  the original specification.
  These assignments represent either counter-examples to ``forall`` statements or
  witnesses to ``exists`` statements.

Generate a verification cache
+++++++++++++++++++++++++++++

A verification cache can be generated by passing a suitable ``target`` to
the ``vehicle compile`` command, e.g.

.. code-block:: bash

  vehicle compile \
    --target MarabouQueries
    --output my/project/robustness-cache
    --specification my/project/mnist-robustness.vcl \
    --network classify:my/project/mnist.onnx \
    --dataset trainingImages:my/project/mnist-trainingImages.idx \
    --dataset trainingLabels:my/project/mnist-trainingLabels.idx \
    --parameter epsilon:0.1 \

The ``--output`` argument determines where the verification cache will be written to.

The full list of relevant command line options are:

.. option:: --target, -t

    The compilation target. There is currently one query format supported:
    ``MarabouQueries``.

.. option:: --output, -o

    The output directory in which to store the compiled queries and the verification plan.

Other arguments are the same as those described in ``verify`` mode above.

Calling the verifier
++++++++++++++++++++

It is possible to use the ``verify`` command to verify a specification via its
pre-generated verification cache.
Suppose the folder ``my/project/robustness-cache`` was generated by the ``vehicle compile`` command
as described above, then specification can be verified by using the ``vehicle verify`` command
and passing the folder ``my/project/robustness-cache`` to the ``--specification`` argument instead of
the original ``.vcl`` file, e.g.

.. code-block:: bash

  vehicle verify \
    --specification my/project/robustness-cache
    --verifier Marabou

The full list of available command line arguments are as follows:

.. option:: --specification, -p

    The location of the verification cache previously generated by Vehicle.

.. option:: --verifier, -v

    See description above for ``verify`` mode.

.. option:: --verifierLocation, -l

    See description above for ``verify`` mode.

Unlike, the previous invocation of the ``verify`` mode, you do not need to pass in the
location of the network and datasets as the cache already contains their location.

Validating a verification cache
+++++++++++++++++++++++++++++++

The :code:`validate` command can then be used to check the status of a
verification cache as follows:

.. code-block:: bash

   vehicle validate \
    --cache my/project/robustness-cache

Vehicle will read the verification cache, and use its contents to find and rehash
the networks and datasets that were used during the original verification
of the specification.
If the new hashes match those stored in the verification cache then the check passes,
otherwise the ``validate`` command will exit with an error.

.. note::

    For obvious reasons, moving or renaming any of the networks or datasets
    or the original specification will result in the ``validate`` command failing.

Inspecting the queries
++++++++++++++++++++++

If you would like to inspect the queries for the verifier generated by Vehicle, there are
two options:

  1. Call the ``compile`` command and omit the ``--output`` argument entirely
     to print the queries to the command line.

  2. Generate the verification cache as described above, and look at the queries
     contained within it.

Limitations of verification
---------------------------

As you might expect, verification is a very hard problem. Therefore there are
several limitations that users should be aware of.

Linearity
~~~~~~~~~

Quantified variables in the specification must be used in a linear manner.
For example, neither of the following is allowed:

.. code-block:: agda

  @network
  f : Vector Rat 2 -> Vector Rat 2

  @property
  p1 : Bool
  p1 = forall x . x * x > 2 => f [ x , 2 ] >= 0.5

  @property
  p2 : Bool
  p2 = forall x y . x * y > 2 => f [ x , y ] >= 0.5

In ``p1`` the variable ``x`` is used to calculate a non-linear value ``x * x``,
and  in ``p2`` the variables ``x`` and ``y`` are used to create a non-linear
value ``x * y``.

In the case where you do try to verify a non-linear property, Vehicle will use
its sophisticated auxiliary type-system to help you pinpoint the source of the
non-linearity.

Quantifiers
~~~~~~~~~~~

While verifiers can be used to verify both universal properties (i.e. with ``forall``)
and existential properties (i.e. with ``exists``) they cannot verify properties with
*alternating* quantifiers where one type of quantifier is used within the scope of the
other type of quantifier. Here are some examples.

.. code-block:: agda

  @network
  f : Vector Rat 2 -> Vector Rat 1

  @property
  good1 : Bool
  good1 = forall x . f x ! 0 >= 0.5

  @property
  good2 : Bool
  good2 = exists x . f x ! 0 >= 0.75

Property ``good1`` and ``good2`` can both be verified as they each only use a single
type of quantifier.

.. code-block:: agda

  @property
  bad1 : Bool
  bad1 = forall y . exists x . f x == y

In contrast property ``bad1`` cannot be verified as it contains a alternating ``forall``
and ``exists``.

.. code-block:: agda

  @property
  good3 : Bool
  good3 = (forall x . f x ! 0 >= 0.5) and (exists y . f y ! 0 >= 0.75)

However, property ``good3`` can be verified even though it contains both a ``forall``
and an ``exists`` as the quantifiers are not alternating (i.e. it can split into
two to form ``good1`` and ``good2``.)

.. code-block:: agda

  @property
  bad2 : Bool
  bad2 = forall x . not (forall x . f x != y)

Note, that as shown by property ``bad2`` alternating quantifiers is not a syntactic
property but a logical one. This property can also not be verified despite only
containing ``forall`` quantifiers. This is because under the rules of classical
first order logic, ``bad2`` is logically equivalent to ``bad1``.

In the case where you do try to verify a property with alternating quantifiers,
Vehicle will use its sophisticated auxiliary type-system to help you pinpoint the
source of the alternation.

Network architecture
~~~~~~~~~~~~~~~~~~~~

Verifiers tend to only support certain layer types and activation functions.
At the moment Vehicle doesn't perform any compatability checking, so please
consult the verifier's own documentation.

Performance
~~~~~~~~~~~

Verification has been shown to be an NP-complete problem so in the worst-case
all verification algorithms will take an infeasibly long time to run.
However, as with many NP-complete algorithms, in the common case performance
can be surprisingly good.

How long it takes to verify a property depends on several factors:

  1. The complexity of the property. The more SAT queries that a property
  is compiled down to, the longer it will take to verify them all. Language
  features that are likely to increase the number of queries generated are

    i. ``if`` statements
    ii. ``and`` statements underneath a ``forall`` quantifier
    iii. ``or`` statements underneath a ``exists`` quantifier

  2. The complexity of the network. The larger the number of nodes in the
  network, the longer it will take the verifier to run the query.
  In general, networks with a small number of wide layers will be easier to
  verify than networks with a large number of narrow layers.

  3. How "close" the network is to satisfying each query. If a query is easily
  satisfiable, or easily non-satisfiable then the verifier will return an
  answer quickly. The closer to the boundary the network lies with respect to
  the query, the longer it will take the verifier to make a decision.
  Unfortunately this is almost impossible to quantify to advance.
