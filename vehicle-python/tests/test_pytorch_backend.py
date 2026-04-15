"""Tests for PyTorch backend compilation."""

import pytest

torch = pytest.importorskip(
    "torch", reason="PyTorch extra is required for backend tests"
)
vehicle_stl = pytest.importorskip(
    "vehicle_stl", reason="vehicle-stl is required for temporal PyTorch backend tests"
)


def test_pytorch_builtins_basic_operations() -> None:
    """Test that PyTorchBuiltins implements basic tensor operations correctly."""
    from vehicle_lang.loss._pytorch._builtins import PyTorchBuiltins

    builtins = PyTorchBuiltins()

    # Test basic tensor creation
    x = torch.tensor([1.0, 2.0, 3.0])
    y = torch.tensor([4.0, 5.0, 6.0])

    # Test arithmetic operations
    add_result = builtins.AddRatTensor(x, y)
    expected = torch.tensor([5.0, 7.0, 9.0])
    assert torch.allclose(
        add_result, expected
    ), f"Expected {expected}, got {add_result}"


def test_pytorch_builtins_advanced_operations() -> None:
    """Test advanced PyTorch builtin operations."""
    from vehicle_lang.loss._pytorch._builtins import PyTorchBuiltins

    builtins = PyTorchBuiltins()

    # Test reductions
    x = torch.tensor([[1.0, 2.0], [3.0, 4.0]])
    sum_result = builtins.ReduceAddRatTensor(0, x)
    expected = torch.tensor(10.0)
    assert torch.allclose(
        sum_result, expected
    ), f"Expected {expected}, got {sum_result}"

    # Test tensor creation functions
    const_tensor = builtins.ConstTensor(5.0, (2, 3))
    expected_shape = (2, 3)
    assert (
        const_tensor.shape == expected_shape
    ), f"Expected shape {expected_shape}, got {const_tensor.shape}"
    assert torch.all(const_tensor == 5.0), "All elements should equal 5.0"


def test_pytorch_translation_basic() -> None:
    """Test that PyTorchTranslation can be instantiated."""
    from vehicle_lang.loss._pytorch._translation import PyTorchTranslation

    translation = PyTorchTranslation()
    assert translation is not None
    assert hasattr(translation, "builtins")
    assert hasattr(translation.builtins, "AddRatTensor")


def test_pytorch_temporal_builtins_globally_and_finally() -> None:
    """Test temporal unary operators against direct vehicle-stl formulas."""
    from vehicle_lang.loss._pytorch._builtins import PyTorchBuiltins

    builtins = PyTorchBuiltins()
    signal = torch.tensor([0.2, -0.4, 0.8, 0.1], dtype=torch.float32)
    start, end = 1, 2

    expected_globally = vehicle_stl.Always(interval=[start, end])(signal)
    expected_finally = vehicle_stl.Eventually(interval=[start, end])(signal)

    actual_globally = builtins.Globally(start, end, signal)
    actual_finally = builtins.Finally(start, end, signal)

    assert torch.allclose(actual_globally, expected_globally)
    assert torch.allclose(actual_finally, expected_finally)


def test_pytorch_temporal_builtin_until() -> None:
    """Test temporal Until operator against direct vehicle-stl formula."""
    from vehicle_lang.loss._pytorch._builtins import PyTorchBuiltins

    builtins = PyTorchBuiltins()
    signal_x = torch.tensor([0.6, 0.2, -0.3, 0.4], dtype=torch.float32)
    signal_y = torch.tensor([-0.2, 0.9, 0.7, -0.1], dtype=torch.float32)
    start, end = 0, 2

    expected = vehicle_stl.Until(interval=[start, end])((signal_x, signal_y))

    actual = builtins.Until(start, end, signal_x, signal_y)
    assert torch.allclose(actual, expected)


def test_pytorch_temporal_until_shape_mismatch() -> None:
    """Until should raise when the two traces have different shapes."""
    from vehicle_lang.loss._pytorch._builtins import PyTorchBuiltins
    from vehicle_lang.loss.error import VehicleInternalError

    builtins = PyTorchBuiltins()
    x = torch.tensor([0.5, 0.3, -0.1], dtype=torch.float32)
    y = torch.tensor([0.1, 0.9], dtype=torch.float32)  # different length

    with pytest.raises(VehicleInternalError):
        builtins.Until(0, 1, x, y)


def test_pytorch_temporal_interval_validation() -> None:
    """Temporal operators should reject invalid intervals explicitly."""
    from vehicle_lang.loss._pytorch._builtins import PyTorchBuiltins
    from vehicle_lang.loss.error import VehicleInternalError

    builtins = PyTorchBuiltins()
    signal = torch.tensor([0.2, 0.3], dtype=torch.float32)

    with pytest.raises(VehicleInternalError):
        builtins.Globally(-1, 1, signal)

    with pytest.raises(VehicleInternalError):
        builtins.Finally(2, 1, signal)


@pytest.mark.skipif(not torch.cuda.is_available(), reason="CUDA not available")  # type: ignore[untyped-decorator]
def test_pytorch_cuda_compatibility() -> None:
    """Test that PyTorch backend works with CUDA if available."""
    from vehicle_lang.loss._pytorch._builtins import PyTorchBuiltins

    builtins = PyTorchBuiltins()

    # Create tensors on GPU
    x = torch.tensor([1.0, 2.0, 3.0], device="cuda")
    y = torch.tensor([4.0, 5.0, 6.0], device="cuda")

    # Test operations work on GPU
    result = builtins.AddRatTensor(x, y)
    assert result.device.type == "cuda", "Result should be on CUDA device"

    expected = torch.tensor([5.0, 7.0, 9.0], device="cuda")
    assert torch.allclose(
        result, expected
    ), "CUDA computation should match expected result"


if __name__ == "__main__":
    # Run basic tests
    test_pytorch_builtins_basic_operations()
    test_pytorch_builtins_advanced_operations()
    test_pytorch_translation_basic()

    print("✅ All PyTorch backend tests passed!")
