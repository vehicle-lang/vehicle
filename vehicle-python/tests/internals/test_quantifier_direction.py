"""A quantifier must aggregate its samples in its own direction.

`forall` is the worst case over the domain and `exists` is the best, so the two
must not return each other's value. The sampler here evaluates at fixed points
rather than searching, so only the aggregation is under test.
"""

from typing import Any, Callable, Sequence

import pytest

from ..config import PYTHON_TEST_SPECS_PATH

torch = pytest.importorskip(
    "torch", reason="PyTorch extra is required for backend tests"
)

SPEC = PYTHON_TEST_SPECS_PATH / "test_quantifier_direction.vcl"

# `f` is the identity, so the body `f [x] ! 0 >= 0.5` is violated at 0.0 and satisfied at 1.0.
POINTS = (0.0, 0.5, 1.0)


class FixedPointSampler:
    def get_loss(
        self,
        dims: Sequence[int],
        lower_bound: "torch.Tensor",
        upper_bound: "torch.Tensor",
        search_lambda: Callable[["torch.Tensor"], "torch.Tensor"],
    ) -> "torch.Tensor":
        return torch.stack(
            [
                torch.as_tensor(search_lambda(torch.tensor(point))).reshape(())
                for point in POINTS
            ]
        )


def _loss(logic: Any, property_name: str) -> float:
    from vehicle_lang.loss import pytorch as vpt

    declarations = vpt.load_specification(
        SPEC,
        logic=logic,
        samplers={"x": FixedPointSampler()},
        declarations=[property_name],
    )
    return float(declarations[property_name](lambda x: x))


@pytest.mark.parametrize(
    ("logic_name", "universal", "existential"),
    [
        # VehicleLoss scores `x >= y` as `y - x`, so the body is 0.5, 0.0, -0.5.
        ("VehicleDifferentiableLogic", 0.5, -0.5),
        # DL2Loss clamps at zero, so the body is 0.5, 0.0, 0.0.
        ("DL2DifferentiableLogic", 0.5, 0.0),
    ],
)
def test_quantifiers_are_not_swapped(
    logic_name: str, universal: float, existential: float
) -> None:
    import vehicle_lang.typing as vcl_typing

    logic = getattr(vcl_typing, logic_name)()

    assert _loss(logic, "universal") == pytest.approx(universal)
    assert _loss(logic, "existential") == pytest.approx(existential)
