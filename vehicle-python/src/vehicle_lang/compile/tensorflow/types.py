import jaxtyping
import tensorflow as tf  # type: ignore
from typing_extensions import TypeAlias

Index: TypeAlias = int
Bool: TypeAlias = bool
Nat: TypeAlias = int
Int: TypeAlias = int
Rat: TypeAlias = float

IndexTensor: TypeAlias = jaxtyping.UInt[tf.Tensor, "..."]
BoolTensor: TypeAlias = jaxtyping.Bool[tf.Tensor, "..."]
NatTensor: TypeAlias = jaxtyping.UInt[tf.Tensor, "..."]
IntTensor: TypeAlias = jaxtyping.Int[tf.Tensor, "..."]
RatTensor: TypeAlias = jaxtyping.Float[tf.Tensor, "..."]
