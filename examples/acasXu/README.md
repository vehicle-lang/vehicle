ACAS Xu example
===============

ACAS Xu is a collision avoidance system for automonous unmanned aircraft, and its
verification was first described in the seminal [Reluplex paper](https://arxiv.org/abs/1702.01135). This example demonstrates how the entire specification, consisting of all
10 properties, can be written in a single file. Unlike the equivalent low-level Marabou queries, the specification is written at a high-level and is understandable by a non-expert.

Input files
-----------

- (MISSING) the AcasXu networks.

- `acasXu.vcl` - the specification describing the desired behaviour.

Verifying using Marabou
-----------------------

TODO

Output files
------------

The outputs of the above Vehicle commands can be found in the test suite:

- [Automatically generated Marabou queries](https://github.com/vehicle-lang/vehicle/tree/dev/test/Test/Compile/Golden/acasXu/acasXu-output-marabou)