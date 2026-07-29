record InvalidField where
  { elemA : Real
  , elemB : Tensor Real [4]
  }


record InvalidInput where
  { element : InvalidField
  }


@network
f : InvalidInput -> Real
