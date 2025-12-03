Type-checking a specification
=============================

Type-checking determines whether a specification is well-formed mathematically,
independently of the actual networks, datasets and parameters it will be used with.

The standard type system includes checks that:
  - that operations are applied to the right types of data
    (e.g. that it doesn't add a ``Vector`` to a ``Bool``).
  - that variables are used consistently
    (e.g. the same variable ``x`` isn't used as an ``Nat`` in one place and
    as a ``Tensor`` in another).
  - that there are no out-of-bounds errors when indexing into ``Vector`` and
    ``Tensor``
    (e.g. if ``xs`` is a vector of size 2 then we don't use index into it
    at any position other than ``0`` and ``1``).

Note that most other Vehicle commands will type-check the specification with the
standard type-system automatically as part of their operation.

While 99\% of the time the ``Standard`` type system is what you want, Vehicle also supports
other type-systems as part of its internal operation:
  - ``Standard``
  - ``Linearity`` (see this `paper <https://dl.acm.org/doi/10.1145/3573105.3575674>`_ for details)
  - ``Polarity`` (see this `paper <https://dl.acm.org/doi/10.1145/3573105.3575674>`_ for details)
  - ``Decidability``

Command-line interface
----------------------

A specification can be type-checked on the command-line using the
:code:`vehicle typecheck` mode.

.. code-block:: bash

  vehicle typecheck \
    --specification my/project/specification.vcl

The table below contains the full list of possible arguments:

.. option:: --specification, -s

    The ``.vcl`` file containing the specification.

.. option:: --typeSystem, -t

    Optional. The type-system to use when checking the specification.

Python interface
----------------

A specification can be type-checked via the Python interface using the
:code:`vehicle_lang.typecheck` module:

.. currentmodule:: vehicle_lang.typecheck

.. autoclass:: TypeSystem
  :members:

.. autofunction:: typecheck
