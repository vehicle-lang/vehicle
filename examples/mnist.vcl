network mnist : Tensor Real [784] -> Tensor Real [10]
dataset trainingData : Tensor Real [2, 784]

epsilon : Real
epsilon = 0.1

delta : Real
delta = 0.1

max : Real -> Real -> Real
max x y = if x >= y then x else y

lInf : forall n. Tensor Real n -> Real
lInf {n} x = max x

lInfBall : forall n. Tensor Real [n] -> Real -> Tensor Real [n] -> Bool
lInfBall {n} x e y = lInf {n} (x - y) <= e

validInput : forall n. Tensor Real [n] -> Bool
validInput {n} x = all {Real} (\xi -> 0 <= xi and xi <= 1) x

robustAround : Tensor Real [784] -> Bool
robustAround x = all {Real} (\z -> lInfBall {784} (mnist x) delta (mnist z)) (lInfBall {10} x epsilon)

robustness : Bool
robustness = all {Tensor Real [784]} robustAround trainingData
