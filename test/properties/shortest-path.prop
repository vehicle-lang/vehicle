
{-
Tensors with 1 dimensional inputs should automatically get collapsed
The following is equivalent to
  network shortestPath : Tensor (1 :: []) Nat -> Tensor (1 :: []) Nat
-}
network shortestPath : Nat -> Nat

validInput : Nat -> Bool
validInput x = 0 <= x <= 15

extendsSomePath : Nat -> Bool
extendsSomePath i = exists j . validInput j && shortestPath i = shortestPath j + 1

usesShortestPath : Nat -> Bool
usesShortestPath i = forall j . validInput j && shortestPath i <= shortestPath j + 1

--- The shortest path to the origin is always the trivial path
identityPath : Bool
identityPath = shortestPath 0 = 0

-- Any path from a non-origin node must be an extension of another path
extension : Bool
extension = forall i . validInput i && i != 0 => extendsSomePath i

-- The shortest path must be at least as short as any path on offer from its neighbours
shortest : Bool
shortest = forall i . validInput i && i != 0 => usesShortestPath i