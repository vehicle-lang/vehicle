nBins = 300

type InputVector = Tensor Real [6]
type OutputVector = Tensor Real [2500]

network spectra : InputVector -> OutputVector



iLB : Tensor Real [6]
iLB = [ .... ]

iUB : Tensor Real [6]
iUB = [ .... ]

weights : Tensor Real [2500, nBins]
weights = [ .... ]

binCovariances : Tensor Real [nBins, nBins]
binCovariances = _

measuredMean : Tensor Real [nBins]
measuredMean = _



inputBetweenBounds : InputVector -> Prop
inputBetweenBounds x = forall i . (iLB ! i) <= input i <= (iUB ! i)

sensibleSpectra : InputVector -> Prop
sensibleSpectra x =
  ((weights ** spectra x) - measuredMean) .
  binCovariances .
  ((weights ** spectra x) - measuredMean)

sensiblePrediction : Prop
sensiblePrediction = forall (x : InputVector) . inputBetweenBounds x => sensibleSpectra x
