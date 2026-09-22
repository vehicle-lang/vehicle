"""A search must aggregate its samples in its quantifier's direction.

The compiler only emits existential searches today, so the universal branch is
reached by building the node directly.
"""

from typing import Any

import pytest

from vehicle_lang._ast import _nodes as vcl

torch = pytest.importorskip(
    "torch", reason="PyTorch extra is required for backend tests"
)

SAMPLES = (0.5, 0.0, -0.5)


def _search(quantifier: vcl.Quantifier) -> Any:
    dims = vcl.DimensionNil()
    bound = vcl.RatTensor(vcl.ConstantTensor(shape=(), value=vcl.Finite(0)))
    body = vcl.Lam(vcl.Binder(vcl.MISSING, "x", vcl.RatType()), vcl.Var("x", ()))
    return vcl.SearchRatTensor("x", quantifier, dims, bound, bound, body)


class ConstantSampler:
    """Ignores the search entirely and hands back a fixed set of sample losses."""

    def get_loss(
        self,
        dims: Any,
        lower_bound: Any,
        upper_bound: Any,
        search_lambda: Any,
        quantifier: vcl.Quantifier,
    ) -> "torch.Tensor":
        return torch.tensor(SAMPLES)


@pytest.mark.parametrize(
    ("quantifier", "expected"),
    [("Exists", min(SAMPLES)), ("Forall", max(SAMPLES))],
)  # type: ignore[untyped-decorator]
def test_search_aggregates_in_the_quantifiers_direction(
    quantifier: vcl.Quantifier, expected: float
) -> None:
    from vehicle_lang.loss._pytorch._translation import PyTorchTranslation

    program = vcl.Main(
        [vcl.DefFunction("p", 0, True, vcl.RatType(), _search(quantifier))]
    )
    declarations = PyTorchTranslation().compile(
        program=program,
        path="test_search_aggregation.vcl",
        declaration_context={},
        samplers={"x": ConstantSampler().get_loss},
    )
    assert float(declarations["p"]) == pytest.approx(expected)
