"""Semantic conformance tests for the temporal operator builtins (Globally, Finally, Until)."""

# mypy: disable-error-code=untyped-decorator

from __future__ import annotations

from pathlib import Path
from typing import Any, Callable

import pytest

torch = pytest.importorskip("torch", reason="PyTorch is required for conformance tests")
vehicle_stl = pytest.importorskip(
    "vehicle_stl", reason="vehicle-stl is required for conformance tests"
)

from vehicle_lang.loss._pytorch._builtins import (
    PyTorchBuiltins,  # pyright: ignore[reportMissingImports]
)

_TEMPORAL_VCL = Path(__file__).parent / "data" / "test_temporal.vcl"

# ---------------------------------------------------------------------------
# Semantics parametrization — used for point-interval identity tests
# ---------------------------------------------------------------------------

_ALL_SEMANTICS = [
    pytest.param(None, id="exact"),
    pytest.param(vehicle_stl.softmax(1.0), id="softmax"),
    pytest.param(vehicle_stl.logsumexp(1.0), id="logsumexp"),
]


# ---------------------------------------------------------------------------
# Reference implementation — independent of Vehicle and vehicle-stl
# ---------------------------------------------------------------------------


def _sliding_window(
    signal: Any,
    start: int,
    end: int,
    fn: Callable[..., Any],
) -> list[float | None]:
    """Sliding-window reduction; None where the window exceeds the trace."""
    T = signal.shape[0]
    return [
        fn(signal[t + start : t + end + 1]).item() if t + end < T else None
        for t in range(T)
    ]


# ---------------------------------------------------------------------------


class TestGloballyConformance:
    """Globally[a,b] == min(signal[t+a : t+b+1]) for each in-bounds t."""

    @pytest.mark.parametrize(
        "signal_list, start, end",
        [
            pytest.param([0.5, -0.3, 0.2, 0.4], 0, 1, id="mixed_sign"),
            pytest.param([0.5, 0.3, 0.2, 0.4, -0.1, 0.6], 1, 2, id="non_zero_start"),
            pytest.param([0.5, 0.3, 0.2, 0.4, -0.1, 0.6], 0, 2, id="wider_window"),
        ],
    )
    def test_matches_sliding_window_min(
        self, signal_list: list[float], start: int, end: int
    ) -> None:
        signal = torch.tensor(signal_list, dtype=torch.float32)
        result = PyTorchBuiltins().Globally(start, end, signal)
        for t, exp in enumerate(_sliding_window(signal, start, end, torch.min)):
            if exp is not None:
                assert result[t].item() == pytest.approx(exp, abs=1e-5)

    @pytest.mark.parametrize("semantics", _ALL_SEMANTICS)
    def test_point_interval_is_identity(self, semantics: Any) -> None:
        """Globally[k,k] returns signal[t+k] exactly for all semantics."""
        signal = torch.tensor([0.5, -0.3, 0.2, 0.4], dtype=torch.float32)
        result = PyTorchBuiltins(temporal_semantics=semantics).Globally(1, 1, signal)
        for t in range(3):  # t+1 < 4 for t in {0,1,2}
            assert result[t].item() == pytest.approx(signal[t + 1].item(), abs=1e-5)


class TestFinallyConformance:
    """Finally[a,b] == max(signal[t+a : t+b+1]) for each in-bounds t."""

    @pytest.mark.parametrize(
        "signal_list, start, end",
        [
            pytest.param([-0.5, 0.3, -0.2, 0.4], 0, 1, id="mixed_sign"),
            pytest.param([0.5, 0.3, 0.2, 0.4, -0.1, 0.6], 1, 2, id="non_zero_start"),
            pytest.param([0.5, 0.3, 0.2, 0.4, -0.1, 0.6], 0, 2, id="wider_window"),
        ],
    )
    def test_matches_sliding_window_max(
        self, signal_list: list[float], start: int, end: int
    ) -> None:
        signal = torch.tensor(signal_list, dtype=torch.float32)
        result = PyTorchBuiltins().Finally(start, end, signal)
        for t, exp in enumerate(_sliding_window(signal, start, end, torch.max)):
            if exp is not None:
                assert result[t].item() == pytest.approx(exp, abs=1e-5)

    @pytest.mark.parametrize("semantics", _ALL_SEMANTICS)
    def test_point_interval_is_identity(self, semantics: Any) -> None:
        """Finally[k,k] returns signal[t+k] exactly for all semantics."""
        signal = torch.tensor([0.5, -0.3, 0.2, 0.4], dtype=torch.float32)
        result = PyTorchBuiltins(temporal_semantics=semantics).Finally(1, 1, signal)
        for t in range(3):  # t+1 < 4 for t in {0,1,2}
            assert result[t].item() == pytest.approx(signal[t + 1].item(), abs=1e-5)


class TestApproximationOrdering:
    """Smooth semantics have provable bias directions relative to exact min/max.

    For disjunction (Finally/max):
      softmax_max <= exact_max <= logsumexp_max

    For conjunction (Globally/min):
      logsumexp_min <= exact_min <= softmax_min

    These are mathematical facts: softmax_max is a convex combination of values
    so it is always <= max; logsumexp_max = log(sum(exp(T*x)))/T >= max(x) by
    Jensen's inequality. The min orderings follow by symmetry (min(x) = -max(-x)).
    """

    def test_globally_ordering(self) -> None:
        signal = torch.tensor([0.5, 0.3, 0.8, 0.2, 0.6], dtype=torch.float32)
        b_exact = PyTorchBuiltins()
        b_soft = PyTorchBuiltins(temporal_semantics=vehicle_stl.softmax(1.0))
        b_lse = PyTorchBuiltins(temporal_semantics=vehicle_stl.logsumexp(1.0))

        r_exact = b_exact.Globally(0, 2, signal)
        r_soft = b_soft.Globally(0, 2, signal)
        r_lse = b_lse.Globally(0, 2, signal)

        for t in range(3):  # in-bounds: t+2 < 5
            assert r_lse[t].item() <= r_exact[t].item() + 1e-5
            assert r_exact[t].item() <= r_soft[t].item() + 1e-5

    def test_finally_ordering(self) -> None:
        signal = torch.tensor([0.5, 0.3, 0.8, 0.2, 0.6], dtype=torch.float32)
        b_exact = PyTorchBuiltins()
        b_soft = PyTorchBuiltins(temporal_semantics=vehicle_stl.softmax(1.0))
        b_lse = PyTorchBuiltins(temporal_semantics=vehicle_stl.logsumexp(1.0))

        r_exact = b_exact.Finally(0, 2, signal)
        r_soft = b_soft.Finally(0, 2, signal)
        r_lse = b_lse.Finally(0, 2, signal)

        for t in range(3):  # in-bounds: t+2 < 5
            assert r_soft[t].item() <= r_exact[t].item() + 1e-5
            assert r_exact[t].item() <= r_lse[t].item() + 1e-5


class TestUntilConformance:
    """Until[a,b] phi psi: phi holds up to and including the switch point, psi holds there.

    vehicle-stl computes:
      Until[a,b](j) = max_{k=a}^{b} min(phi[j], ..., phi[j+k], psi[j+k])
    """

    def test_exact_expected_value(self) -> None:
        """Manually computed expected values for EXACT semantics.

        phi=[1.0, 1.0, 0.5], psi=[-1.0, 0.8, -1.0], Until[0,1]:
          j=0: max(min(1.0,-1.0), min(1.0,1.0,0.8)) = max(-1.0, 0.8) = 0.8
          j=1: max(min(1.0, 0.8), min(1.0,0.5,-1.0)) = max(0.8, -1.0) = 0.8
        """
        phi = torch.tensor([1.0, 1.0, 0.5], dtype=torch.float32)
        psi = torch.tensor([-1.0, 0.8, -1.0], dtype=torch.float32)
        result = PyTorchBuiltins().Until(0, 1, phi, psi)
        assert result[0].item() == pytest.approx(0.8, abs=1e-5)
        assert result[1].item() == pytest.approx(0.8, abs=1e-5)

    def test_point_interval_is_pointwise_min(self) -> None:
        """Until[k,k](phi, psi)[t] == min(phi[t+k], psi[t+k]) for exact semantics.

        When the interval collapses to a single switch time k, the only candidate
        is k itself: phi must hold up to k (trivially — one step) and psi must
        hold at k. The result reduces to min(phi[t+k], psi[t+k]).
        """
        phi = torch.tensor([0.8, 0.3, 0.6, 0.1], dtype=torch.float32)
        psi = torch.tensor([0.5, 0.9, 0.2, 0.7], dtype=torch.float32)
        result = PyTorchBuiltins().Until(1, 1, phi, psi)
        for t in range(3):  # t+1 < 4
            expected = min(phi[t + 1].item(), psi[t + 1].item())
            assert result[t].item() == pytest.approx(expected, abs=1e-5)


# ---------------------------------------------------------------------------
# Unit tests for lift_to_reduction (independent of the derivation pipeline)
# ---------------------------------------------------------------------------


class TestLiftToReduction:
    """Verify that lift_to_reduction correctly folds a binary op over a window dimension.

    These tests exercise the fold logic directly — independently of the
    VCL compilation and derivation pipeline — so a bug in the fold (wrong
    identity, wrong accumulation order, wrong keepdim behaviour) is caught
    here without relying on ``TestDerivedSemantics`` to surface it.

    The input shape matches what vehicle-stl passes to a ``ReductionOp``:
    a 2D tensor (timesteps, window_length) reduced along the window dimension.
    """

    def test_min_fold_matches_sliding_window(self) -> None:
        """lift_to_reduction(torch.minimum, identity) must match sliding-window min."""
        from vehicle_lang.loss._pytorch._semantics import lift_to_reduction

        reduce_min = lift_to_reduction(torch.minimum, identity=1e6)
        signal = torch.tensor([0.5, -0.3, 0.2, 0.4, 0.1], dtype=torch.float32)
        # Three windows of length 3, stacked: shape (3, 3)
        windows = torch.stack([signal[i : i + 3] for i in range(3)], dim=0)
        result = reduce_min(windows, 1, False)
        for t in range(3):
            expected = signal[t : t + 3].min().item()
            assert result[t].item() == pytest.approx(expected, abs=1e-5)

    def test_max_fold_matches_sliding_window(self) -> None:
        """lift_to_reduction(torch.maximum, identity) must match sliding-window max."""
        from vehicle_lang.loss._pytorch._semantics import lift_to_reduction

        reduce_max = lift_to_reduction(torch.maximum, identity=-1e6)
        signal = torch.tensor([0.5, -0.3, 0.2, 0.4, 0.1], dtype=torch.float32)
        windows = torch.stack([signal[i : i + 3] for i in range(3)], dim=0)
        result = reduce_max(windows, 1, False)
        for t in range(3):
            expected = signal[t : t + 3].max().item()
            assert result[t].item() == pytest.approx(expected, abs=1e-5)

    def test_identity_does_not_affect_inbounds_result(self) -> None:
        """The fold's starting accumulator must not influence in-bounds output.

        Any identity value that is neutral for the op (i.e. >= all signal values
        for min) should leave the result unchanged regardless of its magnitude.
        """
        from vehicle_lang.loss._pytorch._semantics import lift_to_reduction

        signal = torch.tensor([0.1, 0.5, 0.3], dtype=torch.float32)
        windows = signal.unsqueeze(0)  # (1, 3) — single full window
        for identity in [1.0, 1e3, 1e6, 1e9]:
            reduce_min = lift_to_reduction(torch.minimum, identity=identity)
            result = reduce_min(windows, 1, False)
            assert result[0].item() == pytest.approx(
                0.1, abs=1e-5
            ), f"identity={identity} must not affect min([0.1, 0.5, 0.3])=0.1"


# ---------------------------------------------------------------------------
# Integration tests: auto-derivation of temporal semantics from each DL
# ---------------------------------------------------------------------------


def _derive_semantics_from(logic: Any) -> Any:
    """Derive vehicle-stl Semantics from a DifferentiableLogic via the compile path."""
    from vehicle_lang.loss import _ast as _loss_ast
    from vehicle_lang.loss.pytorch import _derive_temporal_semantics

    program = _loss_ast.load(_TEMPORAL_VCL, target=logic)
    return _derive_temporal_semantics(program)


class TestDerivedSemantics:
    """Verify that _derive_temporal_semantics produces correct semantics from each DL.

    The auto-derivation path compiles the VCL ``temporalConjunction`` /
    ``temporalDisjunction`` lambdas from the logic record, lifts them to
    ``ReductionOp`` callables via ``lift_to_reduction``, and constructs a
    ``vehicle_stl.Semantics``.  These tests confirm that the resulting
    semantics are numerically equivalent to the expected min/max operations,
    using ``_sliding_window`` as the independent reference — not another
    vehicle-stl call that could mask an upstream regression.

    STLLoss:
      - ``temporalConjunction = \\x y -> min x y``  →  Globally = sliding-window min
      - ``temporalDisjunction = \\x y -> max x y``  →  Finally  = sliding-window max

    DL2Loss:
      - ``temporalConjunction = \\x y -> max x y``  →  Globally = sliding-window max
      - ``temporalDisjunction = \\x y -> min x y``  →  Finally  = sliding-window min
    """

    @pytest.fixture(scope="class")
    def stl_semantics(self) -> Any:
        from vehicle_lang.typing import DifferentiableLogic

        return _derive_semantics_from(DifferentiableLogic.STL)

    @pytest.fixture(scope="class")
    def dl2_semantics(self) -> Any:
        from vehicle_lang.typing import DifferentiableLogic

        return _derive_semantics_from(DifferentiableLogic.DL2)

    # -- STLLoss: derived semantics must equal sliding-window min/max --

    @pytest.mark.parametrize(
        "signal_list, start, end",
        [
            pytest.param([0.5, -0.3, 0.2, 0.4, 0.1], 0, 1, id="mixed_sign"),
            pytest.param([0.5, -0.3, 0.2, 0.4, 0.1], 1, 2, id="non_zero_start"),
            pytest.param([0.5, -0.3, 0.2, 0.4, 0.1], 0, 2, id="wider_window"),
        ],
    )
    def test_stl_globally_matches_exact(
        self, stl_semantics: Any, signal_list: list[float], start: int, end: int
    ) -> None:
        """STLLoss derives min-based conjunction: Globally must match sliding-window min.

        Uses ``_sliding_window`` as the reference so that a vehicle-stl regression
        in ``Always`` cannot mask a failure here.
        """
        signal = torch.tensor(signal_list, dtype=torch.float32)
        derived = PyTorchBuiltins(temporal_semantics=stl_semantics)
        r_derived = derived.Globally(start, end, signal)
        for t, exp in enumerate(_sliding_window(signal, start, end, torch.min)):
            if exp is not None:
                assert r_derived[t].item() == pytest.approx(exp, abs=1e-5), (
                    f"STL-derived Globally[{start},{end}] at t={t} must equal "
                    f"sliding-window min {exp:.5f}"
                )

    @pytest.mark.parametrize(
        "signal_list, start, end",
        [
            pytest.param([0.5, -0.3, 0.2, 0.4, 0.1], 0, 1, id="mixed_sign"),
            pytest.param([0.5, -0.3, 0.2, 0.4, 0.1], 1, 2, id="non_zero_start"),
            pytest.param([0.5, -0.3, 0.2, 0.4, 0.1], 0, 2, id="wider_window"),
        ],
    )
    def test_stl_finally_matches_exact(
        self, stl_semantics: Any, signal_list: list[float], start: int, end: int
    ) -> None:
        """STLLoss derives max-based disjunction: Finally must match sliding-window max.

        Uses ``_sliding_window`` as the reference so that a vehicle-stl regression
        in ``Eventually`` cannot mask a failure here.
        """
        signal = torch.tensor(signal_list, dtype=torch.float32)
        derived = PyTorchBuiltins(temporal_semantics=stl_semantics)
        r_derived = derived.Finally(start, end, signal)
        for t, exp in enumerate(_sliding_window(signal, start, end, torch.max)):
            if exp is not None:
                assert r_derived[t].item() == pytest.approx(exp, abs=1e-5), (
                    f"STL-derived Finally[{start},{end}] at t={t} must equal "
                    f"sliding-window max {exp:.5f}"
                )

    # -- DL2Loss: derived semantics must use max/min (opposite of STL) --

    def test_dl2_globally_uses_max(self, dl2_semantics: Any) -> None:
        """DL2Loss encodes max as temporal conjunction: Globally = worst-case penalty.

        In DL2, loss values are non-negative and larger = worse violation.
        The worst-case element in a time window dominates, so temporal
        conjunction reduces to max (not min as in STL).
        """
        # signal[0:3] = [0.1, 0.5, 0.2]; min=0.1, max=0.5
        signal = torch.tensor([0.1, 0.5, 0.2, 0.9], dtype=torch.float32)
        dl2 = PyTorchBuiltins(temporal_semantics=dl2_semantics)

        r_dl2 = dl2.Globally(0, 2, signal)

        assert r_dl2[0].item() == pytest.approx(
            0.5, abs=1e-5
        ), "DL2 Globally[0,2] at t=0 should be max([0.1,0.5,0.2])=0.5"

    def test_dl2_finally_uses_min(self, dl2_semantics: Any) -> None:
        """DL2Loss encodes min as temporal disjunction: Finally = best-case satisfaction.

        In DL2, loss=0 means fully satisfied; Finally looks for the timestep
        with the smallest penalty, so temporal disjunction reduces to min (not max).
        """
        # signal[0:3] = [0.5, 0.1, 0.3]; min=0.1, max=0.5
        signal = torch.tensor([0.5, 0.1, 0.3, 0.8], dtype=torch.float32)
        dl2 = PyTorchBuiltins(temporal_semantics=dl2_semantics)

        r_dl2 = dl2.Finally(0, 2, signal)

        assert r_dl2[0].item() == pytest.approx(
            0.1, abs=1e-5
        ), "DL2 Finally[0,2] at t=0 should be min([0.5,0.1,0.3])=0.1"
