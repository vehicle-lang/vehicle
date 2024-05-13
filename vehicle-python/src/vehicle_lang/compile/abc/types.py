from typing import Any, Tuple, Union

from typing_extensions import TypeAlias, TypeVar

################################################################################
# Type Variables & Aliases for Interpretation of Vehicle Types
################################################################################

Bool = TypeVar("Bool")
Index = TypeVar("Index")
Nat = TypeVar("Nat")
Int = TypeVar("Int")
Rat = TypeVar("Rat")

BoolTensor = TypeVar("BoolTensor")
IndexTensor = TypeVar("IndexTensor")
NatTensor = TypeVar("NatTensor")
IntTensor = TypeVar("IntTensor")
RatTensor = TypeVar("RatTensor")

Unit: TypeAlias = Tuple[()]

################################################################################
# Vehicle Types for Translations
################################################################################

Program = TypeVar("Program")

Declaration = TypeVar("Declaration")

Expression = TypeVar("Expression")
