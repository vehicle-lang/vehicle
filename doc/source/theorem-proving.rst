Exporting a specification to an ITP
===================================

The end goal for most Vehicle projects is to verify that the neural network
obeys its specification. However, in some projects it is also desirable to use
this fact to prove that some larger development that *uses* the neural network
is also functionally correct.

Take the car example in the list of example Vehicle projects. Here the end
goal is to prove that the car being steered by the neural network never leaves
the road. This safety property is not just a property of the neural network, but
is instead a temporal property about the model of the entire system, e.g. the car,
the road, the physics of the system.


Supported backends
------------------

Agda