zeroD : Tensor Nat []
zeroD = 2

oneD : Tensor Nat [2]
oneD = [zeroD, 1]

twoD : Tensor Nat [2, 2]
twoD = [oneD, [2,3]]

lookup2D : Nat
lookup2D = twoD ! 0 ! 1
