@network
net : Tensor Real [1] -> Tensor Real [1]

@parameter
epsilon : Real

@parameter
factor : Real

-- Test that domains formed by operations over purely constant terms
-- extract without errors. E.g. epsilon * factor instead of just epsilon.
-- Regression test for unlinearisable VMulRatTensor/VDivRatTensor when pure constants.
@property
boundedByEpsilonFactor : Bool
boundedByEpsilonFactor = forall x . (-epsilon * factor) < x ! 0 < (epsilon * factor) => (-(epsilon / factor)) < net x ! 0 < (epsilon / factor)
