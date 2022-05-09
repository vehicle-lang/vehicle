network shortestPath : Index 15 -> Int

extendsSomePath : Int -> Prop
extendsSomePath i = exists j . shortestPath i == shortestPath j + 1

usesShortestPath : Int -> Prop
usesShortestPath i = forall j . shortestPath i <= shortestPath j + 1

-- The shortest path to the origin is always the trivial path.
identityPath : Prop
identityPath = shortestPath 0 == 0

-- Any path from a non-origin node must be an extension of another path.
extension : Prop
extension = forall i . i != 0 => extendsSomePath i

-- The shortest path must be at least as short as any path on offer from its neighbours.
shortest : Prop
shortest = forall i . i != 0 => usesShortestPath i