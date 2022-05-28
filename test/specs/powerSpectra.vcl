nBins = 300

type InputVector = Tensor Rat [6]
type OutputVector = Tensor Rat [2500]

network spectra : InputVector -> OutputVector



iLB : Tensor Rat [6]
iLB = [ .... ]

iUB : Tensor Rat [6]
iUB = [ .... ]

weights : Tensor Rat [2500, nBins]
weights = [ .... ]

binCovariances : Tensor Rat [nBins, nBins]
binCovariances = _

measuredMean : Tensor Rat [nBins]
measuredMean = _



inputBetweenBounds : InputVector -> Bool
inputBetweenBounds x = forall i . (iLB ! i) <= input i <= (iUB ! i)

sensibleSpectra : InputVector -> Bool
sensibleSpectra x =
  ((weights ** spectra x) - measuredMean) .
  binCovariances .
  ((weights ** spectra x) - measuredMean)

sensiblePrediction : Bool
sensiblePrediction = forall (x : InputVector) . inputBetweenBounds x => sensibleSpectra x
