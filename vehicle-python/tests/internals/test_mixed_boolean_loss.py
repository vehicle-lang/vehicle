"""Mixing a decidable `Bool` with a loss value must keep both sides of the operation.

`and`, `or` and `=>` all have a form where one side is decidable and the other is a loss.
The loss backend compiles those to a `where` on the decidable side, and getting the branches
wrong fails silently: the result stops depending on the network, so the loss is constant and
carries no gradient, which looks exactly like a property the model already satisfies.

Each declaration in the specification reduces to a statement about `f` alone once its finite
quantifier is unrolled, so varying only the network's output is enough to tell whether the
loss side survived.
"""

from typing import Any, Callable

import pytest

from ..config import PYTHON_TEST_SPECS_PATH

torch = pytest.importorskip(
    "torch", reason="PyTorch extra is required for backend tests"
)

MIXED_SPEC = PYTHON_TEST_SPECS_PATH / "test_mixed_boolean_loss.vcl"
# Kept in its own file: a specification holding both `Index 2` and `Index 3` makes the
# compiler emit a monomorphised helper whose Python name is not a valid identifier.
GUARD_SPEC = PYTHON_TEST_SPECS_PATH / "test_classification_guard.vcl"

LOGICS = ["VehicleDifferentiableLogic", "DL2DifferentiableLogic"]


def _logic(name: str) -> Any:
    import vehicle_lang.typing as vcl_typing

    return getattr(vcl_typing, name)()


def _declaration(logic: Any, spec: Any, name: str) -> Callable[..., Any]:
    from vehicle_lang.loss import pytorch as vpt

    return vpt.load_specification(spec, logic=logic)[name]


def _satisfied(value: float) -> bool:
    # VehicleLoss is signed, with negative for true; DL2Loss is non-negative with zero for
    # true. Both agree that a positive loss means violated.
    return value <= 0.0


@pytest.mark.parametrize("logic_name", LOGICS)
def test_guarded_implication_keeps_its_consequent(logic_name: str) -> None:
    """`i != j => f x ! i >= f x ! j`, the idiom every classification property uses."""
    wins = _declaration(_logic(logic_name), GUARD_SPEC, "wins")

    def score(scores: list[float]) -> float:
        return float(wins(f=lambda _x: torch.tensor(scores), i=1, x=torch.zeros(1)))

    winning = score([0.0, 10.0, 0.0])
    losing = score([10.0, 0.0, 0.0])

    assert losing > winning, (
        "the loss must depend on the network's answer; equal values mean the rival "
        "comparisons are never scored"
    )
    assert _satisfied(winning) and not _satisfied(losing)


# Which logics each case can be stated in. `mixedAnd` and `mixedImpliesBool` both reduce to a
# negated loss, and DL2Loss cannot represent negation: its `pointwiseNegation` is `1/x`, which
# takes a violated value such as 0.5 to 2.0, still violated. That is why DL2 requires negations
# pushed to the leaves rather than applied to a compound. It is a property of the logic, not of
# the instances under test here, and VehicleLoss exercises the same instances.
@pytest.mark.parametrize(
    ("declaration", "satisfied_when_above_half", "logic_names"),
    [
        # reduces to `not (f x ! 0 >= 0.5)`
        ("mixedAnd", False, ["VehicleDifferentiableLogic"]),
        # reduces to `f x ! 0 >= 0.5`, with no negation anywhere
        ("mixedOr", True, LOGICS),
        # reduces to `not (f x ! 0 >= 0.5)`
        ("mixedImpliesBool", False, ["VehicleDifferentiableLogic"]),
    ],
)
def test_mixed_operation_keeps_its_loss_side(
    declaration: str, satisfied_when_above_half: bool, logic_names: list[str]
) -> None:
    for logic_name in logic_names:
        _check_mixed_operation(logic_name, declaration, satisfied_when_above_half)


def _check_mixed_operation(
    logic_name: str, declaration: str, satisfied_when_above_half: bool
) -> None:
    function = _declaration(_logic(logic_name), MIXED_SPEC, declaration)

    def score(value: float) -> float:
        return float(function(f=lambda _x: torch.tensor([value]), x=torch.zeros(1)))

    above, below = score(1.0), score(0.0)

    assert above != below, "the loss must depend on the network's answer"
    assert _satisfied(above) is satisfied_when_above_half
    assert _satisfied(below) is not satisfied_when_above_half
