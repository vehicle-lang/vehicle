from dataclasses import dataclass
from typing import List, Tuple, Type

import numpy as np
import numpy.typing as npt
from typing_extensions import TypeVar, override
from vehicle_lang.typing import VariableDomain, VehicleVector

__all__: List[str] = [
    "VariableDomain",
    "BoundedVariableDomain",
]

_DType = TypeVar("_DType", bound=np.generic)


@dataclass(frozen=True)
class BoundedVariableDomain(VariableDomain[_DType]):
    lower_bounds: npt.NDArray[_DType]
    upper_bounds: npt.NDArray[_DType]
    dtype: Type[_DType]

    @staticmethod
    def from_bounds(
        lower_bounds: npt.ArrayLike,
        upper_bounds: npt.ArrayLike,
        *,
        dtype: Type[_DType],
    ) -> VariableDomain[_DType]:
        return BoundedVariableDomain(
            lower_bounds=np.array(lower_bounds, dtype=dtype),
            upper_bounds=np.array(upper_bounds, dtype=dtype),
            dtype=dtype,
        )

    @override
    def dimensions(self) -> Tuple[int, ...]:
        return self.lower_bounds.shape

    @override
    def random_value(self) -> npt.NDArray[_DType]:
        return np.divide(np.add(self.lower_bounds, self.upper_bounds), 2.0)

    @override
    def clip(self, point: VehicleVector[_DType]) -> npt.NDArray[_DType]:
        return np.clip(
            np.array(point, dtype=self.dtype), self.lower_bounds, self.upper_bounds
        )
