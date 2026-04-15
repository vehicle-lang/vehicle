"""Semantic conformance tests for the temporal operator builtins (Globally, Finally, Until)."""

# mypy: disable-error-code=untyped-decorator

from __future__ import annotations

import pytest

torch = pytest.importorskip("torch", reason="PyTorch is required for conformance tests")
vehicle_stl = pytest.importorskip(
    "vehicle_stl", reason="vehicle-stl is required for conformance tests"
)

from typing import Any, Callable

from vehicle_lang.loss._pytorch._builtins import (
    PyTorchBuiltins,  # pyright: ignore[reportMissingImports]
)

# ---------------------------------------------------------------------------
# Semantics parametrization — used for invariant tests (shape, point interval)
# ---------------------------------------------------------------------------

_ALL_SEMANTICS = [
    pytest.param(None, id="exact"),
    pytest.param(vehicle_stl.softmax(1.0), id="softmax"),
    pytest.param(vehicle_stl.logsumexp(1.0), id="logsumexp"),
]


# ---------------------------------------------------------------------------
# Reference implementation (used only for EXACT conformance tests)
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

    @pytest.mark.parametrize("semantics", _ALL_SEMANTICS)
    def test_shape_preserved(self, semantics: Any) -> None:
        signal = torch.tensor([0.2, 0.4, -0.1, 0.3, 0.5], dtype=torch.float32)
        result = PyTorchBuiltins(temporal_semantics=semantics).Globally(0, 2, signal)
        assert result.shape == signal.shape


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

    @pytest.mark.parametrize("semantics", _ALL_SEMANTICS)
    def test_shape_preserved(self, semantics: Any) -> None:
        signal = torch.tensor([0.2, 0.4, -0.1, 0.3, 0.5], dtype=torch.float32)
        result = PyTorchBuiltins(temporal_semantics=semantics).Finally(0, 2, signal)
        assert result.shape == signal.shape


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

    @pytest.mark.parametrize("semantics", _ALL_SEMANTICS)
    def test_shape_preserved(self, semantics: Any) -> None:
        phi = torch.tensor([0.6, 0.2, -0.3, 0.4], dtype=torch.float32)
        psi = torch.tensor([-0.2, 0.9, 0.7, -0.1], dtype=torch.float32)
        result = PyTorchBuiltins(temporal_semantics=semantics).Until(0, 2, phi, psi)
        assert result.shape == phi.shape

    def test_sign_consistent_across_semantics(self) -> None:
        """When EXACT Until is strongly positive, all smooth semantics agree.

        phi holds everywhere; psi becomes +5 at j+2, so the switch at k=2
        yields min(5,5,5,5)=5. All smooth max approximations of {-5,-5,5}
        are positive since 5 dominates at temperature 1.0.
        """
        phi = torch.tensor([5.0, 5.0, 5.0, 5.0], dtype=torch.float32)
        psi = torch.tensor([-5.0, -5.0, 5.0, -5.0], dtype=torch.float32)
        for sem_param in _ALL_SEMANTICS:
            semantics = sem_param.values[0]
            result = PyTorchBuiltins(temporal_semantics=semantics).Until(0, 2, phi, psi)
            assert (
                result[0].item() > 0
            ), f"Expected positive for semantics={sem_param.id!r}"
