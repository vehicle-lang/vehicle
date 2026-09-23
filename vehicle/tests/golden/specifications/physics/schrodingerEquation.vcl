--------------------------------------------------------------------------------
-- Inputs and outputs

@tensor
record Input = { time : Real, position : Real }

type Output = Real

--------------------------------------------------------------------------------
-- Network

@network
model : Input -> Output

sech : Real -> Real
sech = -- Taylor expansion approx.
pi : Real
pi = -- Approx
--------------------------------------------------------------------------------
-- Property

inDomain : Bool
inDomain : Input -> Bool
inDomain i = -5 < i.position < 5 and 0 < i.time < pi/2

boundaryCondition1 : Bool
boundaryCondition1 = forall x .
    inDomain  => model { time = 0, position = x } == 2 * sech x

HeatTransfer : Bool
HeatTransfer = forall i . (d model)/(d position) i == a * (d^2 model)/(d position) i
    -- with du = (d model)/(d position) i, dudxx  =

HeatTransfer = forall i . d[model]/d[position] i == a * d^2[model]/d[position] i


@property
correct : Bool
correct = boundaryCondition1 and boundaryCondition2 and HeatTransfer
