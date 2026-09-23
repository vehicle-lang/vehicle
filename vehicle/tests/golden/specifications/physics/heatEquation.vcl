--------------------------------------------------------------------------------
-- Inputs and outputs

-- type Input = Tensor Real [2]

@tensor
record Input = { position : Real, time : Real }

type Output = Real

--------------------------------------------------------------------------------
-- Network

@network
model : Input -> Output


--------------------------------------------------------------------------------
-- Property

boundaryCondition1 : Bool
boundaryCondition1 = forall t . model { position = 0, time = t } == 100

boundaryCondition2 : Bool
boundaryCondition2 = forall t . model { position = 1, time = t } == 0

HeatTransfer : Bool
-- HeatTransfer = forall i . (d model)/(d position) i == a * (d^2 model)/(d position) i
    -- with du = (d model)/(d position) i, dudxx  =

HeatTransfer = forall i . delta^2[model]/delta[position^2] i == a * delta[model]/delta[position] i


@property
correct : Bool
correct = boundaryCondition1 and boundaryCondition2 and HeatTransfer
