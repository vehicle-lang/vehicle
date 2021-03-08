-- Tensors with 1 dimensional inputs should automatically get collapsed,
-- i.e., `Tensor [1] Int` should be equivalent to `Int`.

network shortestPath : Int -> Int

validInput : Int -> Bool
validInput x = 0 <= x && x <= 15

extendsSomePath : Int -> Bool
extendsSomePath i = any (\j -> shortestPath i == shortestPath j + 1) validInput

usesShortestPath : Int -> Bool
usesShortestPath i = all (\j -> shortestPath i <= shortestPath j + 1) validInput

--- The shortest path to the origin is always the trivial path.
identityPath : Bool
identityPath = shortestPath 0 == 0

-- Any path from a non-origin node must be an extension of another path.
extension : Bool
extension = all (\i -> i != 0 => extendsSomePath i) validInput

-- The shortest path must be at least as short as any path on offer from its neighbours.
shortest : Bool
shortest = all (\i -> i != 0 => usesShortestPath i) validInput
