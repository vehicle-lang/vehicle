@network
f : Tensor Rat [2,2] -> Tensor Rat [2,2]

zeroD : Tensor Rat []
zeroD = 2.5

oneD : Tensor Rat [2]
oneD = [zeroD, 1]

twoD : Tensor Rat [2, 2]
twoD = [oneD, [2, 3]]

lookup2D : Rat
lookup2D = twoD ! 0 ! 1

addition : Tensor Rat [2, 2]
addition = twoD + twoD

subtraction : Tensor Rat [2, 2]
subtraction = twoD - twoD

@property
p : Bool
p = forall i j . (f subtraction + addition) ! i ! j >= 0