Welcome to Vehicle's documentation!
===================================

.. note::

   This project is under active development and no stable version yet exists.

**Vehicle** is a tool for enforcing specifications on neural networks.
Write your specification once in the high-level Vehicle language and
then:

 1. incorporate it during the training of your network (coming soon).
 2. use it to generate counter-examples that can be used in further training or rigorously prove that no such counter-examples exist.
 3. export the proof seamlessly to an Interactive Theorem Prover.

Contents
--------

.. toctree::
   :maxdepth: 2

   installation.md
   CONTRIBUTING.md
   language/index.rst
   typeChecking.rst
   training.rst
   verification.rst
   exportingToITP.rst
   other/index.rst
   backends/verifiers/overview.md
