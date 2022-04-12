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
    and Vehicle will not detect changes that occur while the command is running.
    If such changes do occur, the verification result may not be sound.
    In contrast, datasets are only loaded once and therefore do not suffer from
    such problems.

The table below contains the full list of command line arguments available
for the :code:`verify` command.

.. list-table:: Title
   :widths: 15 10 75
   :header-rows: 1

   * - Option
     - Abbreviation
     - Description

   * - :code:`--network`
     - :code:`-n`
     - Provide a network referenced in the specification.
     Its value should consist of a colon-separated pair of the name of the network
     in the specification and a file path
     e.g. `--network classify:/path/to/my/network.onnx`.
     Can be used multiple times to provide multiple networks.

   * - :code:`--dataset`
     - :code:`-d`
     - Provide a dataset referenced in the specification.
     Its value should consist of a colon-separated pair of the name of the dataset
     in the specification and a file path
     e.g. `--dataset classify:/path/to/my/project/dataset.idx`.
     Can be used multiple times to provide multiple datasets.

   * - :code:`--parameter`
     - :code:`-p`
     - Provide a parameter referenced in the specification.
     Its value should consist of a colon-separated pair of the name of the parameter
     in the specification and its value
     e.g. `--parameter epsilon:0.1`.
     Can be used multiple times to provide multiple parameters.

   * - :code:`--verifier`
     - :code:`-v`
     - Set which verifier should be used to perform the specification.
     At the moment the only supported option is :code:`Marabou`.

   * - :code:`--proofCache`
     - :code:`-c`
     - A location to write out the proof cache that stores the verification results.
     If this argument is not provided then no proof cache will be generated.
     e.g. `--proofCache /path/to/my/project/spec.vclp`.

Checking a verification result
------------------------------

There are several reasons why one might want to check the status of a specification
some time after having initially called :code:`verify`:

1. The verification could be part of an automated test suite run in a continuous
integration framework.

2. The specification could have been exported to an interactive theorem prover
whose workflow consists of regularly rechecking the validity of proofs.

Unfortunately, depending on the size of the network and the complexity of the
specification, verification can be a very expensive procedure taking hours or days.
Therefore it is important to avoid unnecessary reverification.

To solve this problem, after successfully verifying a specication
Vehicle can write out a _proof_ _cache_ file.
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