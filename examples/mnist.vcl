network mnist : Tensor Real (784 :: []) -> Tensor Real (10 :: [])

epsilon : Real
epsilon = 0.1

delta : Real
delta = 0.1

lInf : forall n. Tensor Real n -> Real
lInf x = max x

inLInfBall : forall n. Tensor Real (n :: []) -> Real -> Tensor Real (n :: []) -> Bool
inLInfBall x e y = lInf (x - y) <= e

validInput : forall n. Tensor Real (n :: []) -> Bool
validInput x = all (\ xi . 0 <= xi <= 1)

robustAround : forall n. Tensor Real (n :: []) -> Bool
robustAround x = forall z : inLInfBall x epsilon z => inLInfBall (mnist x) delta (mnist z)

robustness : Bool
robustness = forall x : inTrainingData x => robustAround x