record Rec t1 t2 where
  { f1 : t2
  , f2 : t1
  }


@network
f : Rec (Rec Real Real) Real -> Real
