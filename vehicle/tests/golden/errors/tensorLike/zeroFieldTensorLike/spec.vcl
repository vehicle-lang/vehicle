-- Tests that records with zero fields cannot be cast to tensors.
@tensor
record Test where {}
