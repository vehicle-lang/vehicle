--------------------------------------------------------------------------------
-- Full specification of the HorizontalCAS



--distanceToIntruder   = 0
--angleToIntruder      = 1
--intruderHeading      = 2
--Speed                = 3
--intruderSpeed        = 4

--type OutputVector = Vector Rat 5

--clearOfConflict = 0
--weakLeft        = 1
--weakRight       = 2
--strongLeft      = 3
--strongRight     = 4



--------------------------------------------------------------------------------
-- The network

@network
HorizontalCAS : Vector Rat 5 -> Vector Rat 5


--------------------------------------------------------------------------------
-- Property 6

-- If the intruder is sufficiently far away, the network advises COC.

--intruderFarAway : Vector Rat 5 -> Bool
--intruderFarAway x = 12000 <= (x ! 0) <= 62000



@property
property6 : Bool
property6 = (((HorizontalCAS [0, 0, 0, 1, 1]) ! 0  < (HorizontalCAS [0, 0, 0, 1, 1]) ! 1) 
                                            and ((HorizontalCAS [0, 0, 0, 1, 1]) ! 0 < (HorizontalCAS [0, 0, 0, 1, 1]) ! 2)
                                            and ((HorizontalCAS [0, 0, 0, 1, 1]) ! 0 < (HorizontalCAS [0, 0, 0, 1, 1]) ! 3)
                                            and ((HorizontalCAS [0, 0, 0, 1, 1]) ! 0 < (HorizontalCAS [0, 0, 0, 1, 1]) ! 4))



