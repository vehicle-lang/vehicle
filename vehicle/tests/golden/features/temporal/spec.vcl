@network
f : Tensor Real [1] -> Tensor Real [4]

signal : Tensor Real [4]
signal = f [0.0]

@property
alwaysSmall : Bool
alwaysSmall = (globally[0,3] (signal <. const 1.0 [4])) ! 0

@property
eventuallyAlwaysSmall : Bool
eventuallyAlwaysSmall = (finally[0,3] (globally[0,3] (signal <. const 1.0 [4]))) ! 0

@property
boundedExists : Bool
boundedExists = exists v . 0.0 <= v <= 1.0
                  and ((globally[0,3] (const v [4] <. signal)) ! 0)

@property
boundedInsideTemporal : Bool
boundedInsideTemporal =
  exists v . (globally[0,3] (const 0.0 [4] <. const v [4]
                          and const v [4] <. const 1.0 [4]
                          and const v [4] <. signal)) ! 0

@property
safeUntilSmall : Bool
safeUntilSmall = (until[0,3] (const 0.0 [4] <. signal)
                             (signal <. const 0.5 [4])) ! 0

@property
notAlwaysSmall : Bool
notAlwaysSmall = not ((globally[0,3] (signal <. const 1.0 [4])) ! 0)

@property
foreachInsideTemporal : Bool
foreachInsideTemporal = (globally[0,3] (foreach t . signal ! t <= 1.0)) ! 0
