network mnist : Tensor Real [784] -> Tensor Real [10]
dataset trainingData : Tensor Real [2, 784]

epsilon : Real
epsilon = 0.1

delta : Real
delta = 0.1

max : Real -> Real -> Real
max x y = if x >= y then x else y

lInf : forall n. Tensor Real n -> Real
lInf x = max x

lInfBall : forall n. Tensor Real [n] -> Real -> Tensor Real [n] -> Bool
lInfBall x e y = lInf (x - y) <= e

validInput : forall n. Tensor Real [n] -> Bool
validInput x = forall i in [0..2] . 0 <= x ! i and x ! i <= 1

robustAround : Tensor Real [784] -> Bool
robustAround x = forall z . lInfBall (mnist x) delta (mnist z) => lInfBall x epsilon z

robustness : Bool
robustness = forall x in trainingData . robustAround x
