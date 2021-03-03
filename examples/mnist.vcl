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
validInput x = forall xi : x. 0 <= xi && xi <= 1

robustAround : forall n. Tensor Real [n] -> Bool
robustAround x = forall z : lInfBall x epsilon. lInfBall (mnist x) delta (mnist z)

robustness : Bool
robustness = forall x : trainingData. robustAround x
