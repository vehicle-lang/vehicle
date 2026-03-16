@network
f : Tensor Real [2,2] -> Tensor Real [2,2]

zeroD : Tensor Real []
zeroD = 2.5

oneD : Tensor Real [2]
oneD = [zeroD, 1]

twoD : Tensor Real [2, 2]
twoD = [oneD, [2, 3]]

lookup2D : Real
lookup2D = twoD ! 0 ! 1

addition : Tensor Real [2, 2]
addition = twoD + twoD

subtraction : Tensor Real [2, 2]
subtraction = twoD - twoD

@property
p : Bool
p = forall i j . (f subtraction + addition) ! i ! j >= 0
