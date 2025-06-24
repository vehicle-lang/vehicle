# Autoencoder Example

A simple autoencoder along with a formal proof of a closure property based on the identity property verified by vehicle.

This folder contains the following files:

- `spec.vcl` - the vehicle specification containing the identity property.

- `rocqProof/Proof.v` - the Rocq proof of closure utilising the compiled specification.

## Compiling the specification to Rocq

The specification can be compiled to Rocq by running the following command:

```bash
vehicle export \
    --target Rocq \
    --spcification examples/autoencoderError/spec.vcl \
    --ouput examples/autoencoderError/rocqProof/autoencoderErrorSpec.v
```

The full proof making use of this generated Rocq specification in `rocqProof/autoencoderErrorSpec.v` is found in `rocqProof/Proof.v`.
