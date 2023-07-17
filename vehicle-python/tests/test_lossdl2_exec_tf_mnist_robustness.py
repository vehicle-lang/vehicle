from pathlib import Path
from typing import Any, Callable, Dict, Iterator, Sequence, Union

import numpy as np
import pytest

from vehicle_lang.compile import Target, to_python

MNIST_ROBUSTNESS = (
    Path("vendor")
    / "vehicle"
    / "tests"
    / "golden"
    / "compile"
    / "mnist-robustness"
    / "spec.vcl"
)


def test_lossdl2_exec_tf_mnist_robustness() -> None:
    def sampler_for_pertubation(context: Dict[str, Any]) -> Iterator[Any]:
        yield np.zeros(shape=(28, 28))

    def classifier(image: Any) -> Any:
        return (0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

    # def robust(n, classifier, epsilon, trainingImages, trainingLabels):
    trainingImages = [np.zeros(784)]
    trainingLabels = [(0, 0, 0, 1, 0, 0, 0, 0, 0, 0)]
    robust = to_python(
        MNIST_ROBUSTNESS,
        target=Target.LOSS_DL2,
        samplers={"pertubation": sampler_for_pertubation},
    )["robust"]
    robust_loss = robust(
        n=784,
        classifier=classifier,
        epsilon=0.001,
        trainingImages=trainingImages,
        trainingLabels=trainingLabels,
    )
    print(robust_loss)
