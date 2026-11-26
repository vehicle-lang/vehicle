-- Tests the error message when a network input is computed from the output of
-- another network, which Vehicle cannot currently bound.

@network
encode : Tensor Real [2] -> Tensor Real [1]

@network
decode : Tensor Real [1] -> Tensor Real [2]

@property
p : Bool
p = forall x . (forall i . 0 <= x ! i <= 1) => 0 <= decode (encode x) ! 0
