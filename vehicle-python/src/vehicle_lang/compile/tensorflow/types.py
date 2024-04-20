import jaxtyping as jt
import tensorflow as tf  # type: ignore[import-untyped,unused-ignore]
from typing_extensions import TypeAlias

Index: TypeAlias = int
Bool: TypeAlias = bool
Nat: TypeAlias = int
Int: TypeAlias = int
Rat: TypeAlias = float

IndexTensor: TypeAlias = jt.UInt[tf.Tensor, "..."]
BoolTensor: TypeAlias = jt.Bool[tf.Tensor, "..."]
NatTensor: TypeAlias = jt.UInt[tf.Tensor, "..."]
IntTensor: TypeAlias = jt.Int[tf.Tensor, "..."]
RatTensor: TypeAlias = jt.Float[tf.Tensor, "..."]
