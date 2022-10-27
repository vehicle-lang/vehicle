@network
shortestPath : Index 15 -> Int

extendsSomePath : Int -> Bool
extendsSomePath i = exists j . shortestPath i == shortestPath j + 1

usesShortestPath : Int -> Bool
usesShortestPath i = forall j . shortestPath i <= shortestPath j + 1

-- The shortest path to the origin is always the trivial path.
@property
identityPath : Bool
identityPath = shortestPath 0 == 0

-- Any path from a non-origin node must be an extension of another path.
@property
extension : Bool
extension = forall i . i != 0 => extendsSomePath i

-- The shortest path must be at least as short as any path on offer from its neighbours.
@property
shortest : Bool
shortest = forall i . i != 0 => usesShortestPath i
