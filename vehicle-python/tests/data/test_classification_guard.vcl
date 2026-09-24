@network
f : Tensor Real [1] -> Tensor Real [3]

-- The "class i wins" idiom, as in examples/acasXu/acasXu.vcl and the mnist-robustness
-- specification: a decidable guard `i != j` over a loss-valued comparison. Inverting that
-- guard scores the comparison only at `j == i`, where it is trivially satisfied, so the
-- loss stops depending on the network at all.
wins : Index 3 -> Tensor Real [1] -> Bool
wins i x = forall j . i != j => f x ! i >= f x ! j

@property
p : Bool
p = forall (x : Tensor Real [1]) . 0.0 <= x ! 0 <= 1.0 => wins 1 x
