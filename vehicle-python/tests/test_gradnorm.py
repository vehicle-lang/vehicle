from __future__ import annotations

from typing import Any, Tuple

import pytest


def require_pytorch() -> Tuple[Any, Any]:
    torch_module = pytest.importorskip("torch")
    gradnorm_module = pytest.importorskip("vehicle_lang.loss.pytorch")
    return torch_module, gradnorm_module


def require_tensorflow() -> Tuple[Any, Any]:
    tf_module = pytest.importorskip("tensorflow")
    gradnorm_module = pytest.importorskip("vehicle_lang.loss.tensorflow")
    return tf_module, gradnorm_module


def test_pytorch_renormalises_to_T() -> None:
    torch, loss_pt = require_pytorch()
    trunk = torch.nn.Linear(4, 4)
    target_a = torch.randn(4)
    target_b = torch.randn(4)
    x = torch.randn(4)

    def loss_a() -> Any:
        return ((trunk(x) - target_a) ** 2).sum()

    def loss_b() -> Any:
        return 100.0 * ((trunk(x) - target_b) ** 2).sum()

    balancer = loss_pt.GradNormBalancer(
        {"a": loss_a, "b": loss_b},
        alpha=1.5,
        model=trunk,
    )

    for _ in range(20):
        balancer.step()
        s = sum(w.item() for w in balancer.weights.values())
        assert abs(s - 2.0) < 1e-4


def test_pytorch_suppresses_dominant_task() -> None:
    torch, loss_pt = require_pytorch()
    torch.manual_seed(0)
    trunk = torch.nn.Linear(8, 8)
    optim = torch.optim.SGD(trunk.parameters(), lr=1e-3)
    target_a = torch.randn(8)
    target_b = torch.randn(8)
    x = torch.randn(8)

    def loss_a() -> Any:
        return ((trunk(x) - target_a) ** 2).sum()

    def loss_b() -> Any:
        return 100.0 * ((trunk(x) - target_b) ** 2).sum()

    balancer = loss_pt.GradNormBalancer(
        {"a": loss_a, "b": loss_b},
        alpha=1.5,
        model=trunk,
        weight_lr=0.025,
    )

    for _ in range(200):
        total, _ = balancer.step()
        optim.zero_grad()
        total.backward()
        optim.step()

    w = balancer.weights
    assert w["a"].item() > w["b"].item()


def test_pytorch_explicit_shared_params() -> None:
    torch, loss_pt = require_pytorch()
    a = torch.nn.Linear(2, 2)
    b = torch.nn.Linear(2, 2)
    x = torch.randn(2)
    t = torch.randn(2)

    def loss_a() -> Any:
        return ((a(x) - t) ** 2).sum()

    def loss_b() -> Any:
        return ((b(a(x)) - t) ** 2).sum()

    balancer = loss_pt.GradNormBalancer(
        {"a": loss_a, "b": loss_b},
        shared_params=a.parameters(),
    )
    total, per_task = balancer.step()
    assert total.requires_grad
    assert set(per_task.keys()) == {"a", "b"}


def test_pytorch_rejects_empty() -> None:
    _, loss_pt = require_pytorch()
    with pytest.raises(ValueError):
        loss_pt.GradNormBalancer({}, model=None)


def test_pytorch_requires_model_or_params() -> None:
    _, loss_pt = require_pytorch()
    with pytest.raises(TypeError):
        loss_pt.GradNormBalancer({"a": lambda: None})


def test_tensorflow_renormalises_to_T() -> None:
    tf, loss_tf = require_tensorflow()
    tf.random.set_seed(0)
    trunk = tf.keras.Sequential(
        [tf.keras.layers.Input(shape=(4,)), tf.keras.layers.Dense(4)]
    )
    x = tf.random.normal((1, 4))
    target_a = tf.random.normal((1, 4))
    target_b = tf.random.normal((1, 4))

    def loss_a() -> Any:
        return tf.reduce_sum((trunk(x) - target_a) ** 2)

    def loss_b() -> Any:
        return 100.0 * tf.reduce_sum((trunk(x) - target_b) ** 2)

    balancer = loss_tf.GradNormBalancer(
        {"a": loss_a, "b": loss_b},
        alpha=1.5,
        model=trunk,
    )
    for _ in range(20):
        balancer.step()
        s = float(sum(float(w.numpy()) for w in balancer.weights.values()))
        assert abs(s - 2.0) < 1e-4


def test_tensorflow_suppresses_dominant_task() -> None:
    tf, loss_tf = require_tensorflow()
    tf.random.set_seed(0)
    trunk = tf.keras.Sequential(
        [tf.keras.layers.Input(shape=(8,)), tf.keras.layers.Dense(8)]
    )
    optim = tf.keras.optimizers.SGD(learning_rate=1e-4)
    x = tf.random.normal((1, 8))
    target_a = tf.random.normal((1, 8))
    target_b = tf.random.normal((1, 8))

    def loss_a() -> Any:
        return tf.reduce_sum((trunk(x) - target_a) ** 2)

    def loss_b() -> Any:
        return 10.0 * tf.reduce_sum((trunk(x) - target_b) ** 2)

    balancer = loss_tf.GradNormBalancer(
        {"a": loss_a, "b": loss_b},
        alpha=1.5,
        model=trunk,
        weight_lr=0.025,
    )

    for _ in range(200):
        with tf.GradientTape() as outer_tape:
            total, _ = balancer.step()
        grads = outer_tape.gradient(total, trunk.trainable_variables)
        optim.apply_gradients(zip(grads, trunk.trainable_variables))

    w = balancer.weights
    assert float(w["a"].numpy()) > float(w["b"].numpy())


def test_tensorflow_explicit_shared_params() -> None:
    tf, loss_tf = require_tensorflow()
    a = tf.keras.layers.Dense(2)
    b = tf.keras.layers.Dense(2)
    x = tf.random.normal((1, 2))
    t = tf.random.normal((1, 2))
    a(x)
    b(a(x))

    def loss_a() -> Any:
        return tf.reduce_sum((a(x) - t) ** 2)

    def loss_b() -> Any:
        return tf.reduce_sum((b(a(x)) - t) ** 2)

    balancer = loss_tf.GradNormBalancer(
        {"a": loss_a, "b": loss_b},
        shared_params=a.trainable_variables,
    )
    total, per_task = balancer.step()
    assert set(per_task.keys()) == {"a", "b"}
    assert total.shape == ()
