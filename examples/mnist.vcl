network mnist : Tensor Real [784] -> Tensor Real [10]

epsilon : Real
epsilon = 0.1

delta : Real
delta = 0.1

lInf : forall n. Tensor Real n -> Real
lInf x = max x

lInfBall : forall n. Tensor Real [n] -> Real -> Tensor Real [n] -> Bool
lInfBall x e y = lInf (x - y) <= e

validInput : forall n. Tensor Real [n] -> Bool
validInput x = all (\xi -> 0 <= xi && xi <= 1) x

robustAround : forall n. Tensor Real [n] -> Bool
robustAround x = all (\z -> lInfBall (mnist x) delta (mnist z)) (lInfBall x epsilon)

robustness : Bool
robustness = all robustAround trainingData
