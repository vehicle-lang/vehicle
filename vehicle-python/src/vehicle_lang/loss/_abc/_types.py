from typing_extensions import TypeVar

################################################################################
# Runtime Interface Types (Matching Vehicle Type System)
################################################################################
#
# This file defines the abstract runtime interface that matches the Vehicle type system.
# The Vehicle type system has:
#   - Scalar types: Rat, Index
#   - Tensor type: Tensor a (generic over element type)
#
# Backends provide concrete implementations:
#   - TensorFlow: Tensor → tf.Tensor
#
################################################################################

# Translation target types (what Translation classes produce)
Program = TypeVar("Program")  # e.g., py.Module
Declaration = TypeVar("Declaration")  # e.g., py.stmt
Expression = TypeVar("Expression")  # e.g., py.expr

# Runtime types (matching Vehicle's type system exactly)

# Scalar types (Vehicle's base types)
Index = TypeVar("Index")  # Vehicle Index → backend scalar
Rat = TypeVar("Rat")  # Vehicle Rat → backend scalar

# Generic tensor type (Vehicle's Tensor a - polymorphic over element type)
Tensor = TypeVar("Tensor")  # Vehicle Tensor a → backend tensor

# Utility type variables
Dimension = TypeVar("Dimension")  # Individual dimension value
Dimensions = TypeVar("Dimensions")  # Collection of dimensions
DimensionIndex = TypeVar("DimensionIndex")  # Index into dimensions

################################################################################
# Architecture Summary
################################################################################
#
# The actual type mapping is:
#
# Haskell Vehicle:
#   data JExpr = ... | RatTensor (Tensor Rat) | AddRatTensor JExpr JExpr | ...
#   data Tensor a = DenseTensor TensorShape (Vector a) | ConstantTensor TensorShape a
#
# Python Vehicle AST (vehicle_lang.ast):
#   class RatTensor(Expression): contents: Tensor
#   class AddRatTensor(Expression): x: Expression, y: Expression
#   class Tensor(AST): shape, value
#   class DenseTensor(Tensor), class ConstantTensor(Tensor)
#
# Python Translation → Python AST:
#   Vehicle RatTensor → py.Call(__vehicle__.RatTensor, [tensor_obj])
#   Vehicle AddRatTensor → py.Call(__vehicle__.AddRatTensor, [x_expr, y_expr])
#
# Python Runtime (TensorFlowBuiltins):
#   def RatTensor(self, tensor: Tensor) -> tf.Tensor: ...
#   def AddRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor: ...
