variable = 12

smallIndex : Index 13
smallIndex = variable

bigIndex : Index 42
bigIndex = variable

@property
p : Bool
p = (smallIndex == smallIndex) and (bigIndex == bigIndex)
