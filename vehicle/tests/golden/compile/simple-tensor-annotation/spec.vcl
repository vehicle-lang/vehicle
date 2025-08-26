-- Tests that the parser will accept the @tensor annotation.
@tensor
record Test1 where
  { f1 : Real
  , f2 : Real
  }
