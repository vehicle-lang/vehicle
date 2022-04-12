Verification
============

Given a neural network and a specification, Vehicle can be used to check
whether the network obeys the specification.
At the moment, verification can be performed at the command-line.

.. autosummary::
   :toctree: generated


Verifying a specification
-------------------------

A specification can be verified by using the :code:`vehicle verify` command.

.. code-block:: bash

  vehicle verify \
    --inputFile my/project/mnist-robustness.vcl \
    --network classify:my/project/mnist.onnx \
    --dataset trainingImages:my/project/mnist-trainingImages.idx \
    --dataset trainingLabels:my/project/mnist-trainingLabels.idx \
    --parameter epsilon:0.1 \
    --verifier Marabou \

.. warning::

    The :code:`verify` command is not atomic.
    Verification involves repeatedly loading the network(s) from disk
    and Vehicle will not detect changes to the networks that occur
    while the command is running.

    If such changes do occur, the verification result may not be sound.
    Unlike networks, datasets are only loaded once and therefore do not suffer
    from such race conditions.

The table below contains the full list of command line arguments available
for the :code:`verify` command.

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

    Set which verifier should be used to perform the specification.
    At the moment the only supported option is :code:`Marabou`.

.. option:: --proofCache, -c

    Set the location to write out the proof cache containing the results.
    If this argument is not provided then no proof cache will be generated.

Checking a verification result
------------------------------

There are several reasons why one might want to check the status of a specification
some time after having initially called :code:`verify`:

  1. The verification could be part of an automated test suite in a continuous
  integration framework.

  2. The specification could have been exported to an interactive theorem prover
  whose workflow consists of regularly rechecking the validity of proofs.

Unfortunately, depending on the size of the network and the complexity of the
specification, verification can be a very expensive procedure taking hours or days.
Therefore it is important to avoid unnecessary reverification.

To solve this problem, after successfully verifying a specication
Vehicle can write out a *proof cache* file.
This file contains:

- The original text of the specification.
- The status of the specification.
- The values of the provided parameters.
- The file paths of the networks and datasets provided to the original
  :code:`verify` command along with a hash of the contents of each file.

The :code:`check` command can then be run to use the proof cache to check
the status of the specification as follows:

.. code-block:: bash

   vehicle check --proofCache /my/project/spec.vclp

Vehicle will read the proof cache, and use its contents to find and rehash
the networks and datasets used in the verification of the specification.
If the new hashes match those stored in the proof cache then the check passes,
otherwise the check command will exit with an error.

.. note::

    For obvious reasons, moving or renaming any of the networks or datasets
    will result in the :code:`check` command failing.

Supported backends
------------------