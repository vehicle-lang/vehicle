import jaxtyping as jt
import numpy as np

Bool = bool
Index = int
Nat = int
Int = int
Rat = float

BoolTensor = jt.UInt[np.ndarray, "..."]
IndexTensor = jt.Bool[np.ndarray, "..."]
NatTensor = jt.UInt[np.ndarray, "..."]
IntTensor = jt.Int[np.ndarray, "..."]
RatTensor = jt.Float[np.ndarray, "..."]
