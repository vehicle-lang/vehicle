robustAround : Real -> Bool
robustAround image = forall perturbation . (0 <= image - perturbation) => True

@dataset
trainingImages : Vector Real 1

@property
robust : Vector Bool 1
robust = foreach i . robustAround (trainingImages ! i)
