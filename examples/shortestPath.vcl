-- Tensors with 1 dimensional inputs should automatically get collapsed,
-- i.e., `Tensor [1] Int` should be equivalent to `Int`.

network shortestPath : Int -> Int

validInputs : List Int
validInputs = [0..15]

extendsSomePath : Int -> Prop
extendsSomePath i = some j in validInputs . shortestPath i == shortestPath (j + 1)

usesShortestPath : Int -> Prop
usesShortestPath i = every j in validInputs . shortestPath i <= shortestPath (j + 1)

--- The shortest path to the origin is always the trivial path.
identityPath : Prop
identityPath = shortestPath 0 == 0

-- Any path from a non-origin node must be an extension of another path.
extension : Prop
extension = every i in validInputs . i != 0 => extendsSomePath i

-- The shortest path must be at least as short as any path on offer from its neighbours.
shortest : Prop
shortest = every i in validInputs . i != 0 => usesShortestPath i