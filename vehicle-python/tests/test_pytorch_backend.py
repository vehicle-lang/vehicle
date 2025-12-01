"""Tests for PyTorch backend compilation."""

import pytest
import torch


def test_pytorch_builtins_basic_operations() -> None:
    """Test that PyTorchBuiltins implements basic tensor operations correctly."""
    from vehicle_lang.compile.pytorch._builtins import PyTorchBuiltins

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
    from vehicle_lang.compile.pytorch._builtins import PyTorchBuiltins

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
    from vehicle_lang.compile.pytorch._translation import PyTorchTranslation

    translation = PyTorchTranslation()
    assert translation is not None
    assert hasattr(translation, "builtins")
    assert hasattr(translation.builtins, "AddRatTensor")


@pytest.mark.skipif(not torch.cuda.is_available(), reason="CUDA not available")  # type: ignore[misc]
def test_pytorch_cuda_compatibility() -> None:
    """Test that PyTorch backend works with CUDA if available."""
    from vehicle_lang.compile.pytorch._builtins import PyTorchBuiltins

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
