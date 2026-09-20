-- Vector literals mixing values that do and do not carry gradients.

@network
f : Tensor Real [2] -> Tensor Real [1]

-- A constant and a variable, built in a function.
mkVec : Real -> Vector Real 2
mkVec v = [1.0, v]

@property
helper : Bool
helper = forall (x : Real) . 0.0 <= x <= 1.0 => f [ mkVec x ! 0, x ] ! 0 >= 0.5

-- The same, written directly in the property.
@property
direct : Bool
direct = forall (x : Real) . 0.0 <= x <= 1.0 => f [ ([1.0, x] : Vector Real 2) ! 1, x ] ! 0 >= 0.5

-- Nested vectors, only one of whose rows mentions the variable.
nest : Real -> Vector (Vector Real 2) 2
nest v = [[1.0, 2.0], [v, 3.0]]

@property
nested : Bool
nested = forall (x : Real) . 0.0 <= x <= 1.0 => f [ nest x ! 1 ! 0, x ] ! 0 >= 0.5

-- A vector of tensors.
pair : Tensor Real [2] -> Vector (Tensor Real [2]) 2
pair v = [[0.5, 0.5], v]

@property
ofTensors : Bool
ofTensors = forall (x : Real) . 0.0 <= x <= 1.0 => f (pair [x, x] ! 1) ! 0 >= 0.5

-- Elements without a `Real` have no gradient, and keep the standard typing.
picks : Vector (Index 1) 2
picks = [0, 0]

@property
indices : Bool
indices = forall (x : Real) . 0.0 <= x <= 1.0 => f [x, x] ! (picks ! 1) >= 0.5
