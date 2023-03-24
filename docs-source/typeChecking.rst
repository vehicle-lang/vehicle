Type checking a specification
=============================

Type-checking determines whether a specification makes sense mathematically,
independent of what networks, datasets and parameters it will be used with.

This includes:
  - that operations are applied to the right types of data
    (e.g. that it doesn't add a ``Vector`` to a ``Bool``).
  - that variables are used consistently
    (e.g. the same variable ``x`` isn't used as an ``Nat`` in one place and
    as a ``Tensor`` in another).
  - that there are no out-of-bounds errors when indexing into ``Vector`` and
    ``Tensor``s
    (e.g. If ``xs`` is a vector of size 2 then we don't use index into it
    at any position other than ``0`` and ``1``).

Note most other Vehicle
commands will type-check the specification automatically.

Command-line interface
----------------------

A specification can be type-checked on the command-line using the
:code:`vehicle check` mode.

.. code-block:: bash

  vehicle check \
    --specification my/project/specification.vcl

The table below contains the full list of possible arguments:

.. option:: --specification, -s

    The ``.vcl`` file containing the specification.

.. option:: --typeSystem, -t

    The type-system to use when checking the specification. 90\% of the time
    you should not provide any value for this option as the default value ``Standard``
    is what you want. However there are two further options ``Linearity`` and
    ``Polarity`` which means are used by Vehicle to diagnose errors with how
    quantified variables are used
    (see this `paper <https://dl.acm.org/doi/10.1145/3573105.3575674>`_ for details).
