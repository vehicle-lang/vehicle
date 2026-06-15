################
## Tensorflow ##
################

import pytest

tf = pytest.importorskip(
    "tensorflow",
    reason="TensorFlow extra is required for this test",
)

###################
## Other imports ##
###################

from typing import Any, cast

from typing_extensions import TypeAlias

import vehicle_lang.loss.tensorflow as vcl_tf
from vehicle_lang import DifferentiableLogic
from vehicle_lang.loss._tensorflow.samplers import ConstantTensorFlowSampler

from ..config import HASKELL_GOLDEN_TESTS_PATH

############
## Config ##
############


MNIST_ROBUSTNESS = (
    HASKELL_GOLDEN_TESTS_PATH / "specifications" / "mnist-robustness" / "spec.vcl"
)


LabelDistribution: TypeAlias = tuple[
    float, float, float, float, float, float, float, float, float, float
]


def one_hot(label: int) -> LabelDistribution:
    return cast(
        LabelDistribution,
        tuple([1.0 if index == label else 0.0 for index in range(0, 10)]),
    )


##########
## Test ##
##########


@pytest.mark.skip(reason="Indices in spec not yet supported")  # type: ignore[untyped-decorator]
def test_lossdl2_exec_tf_mnist_robustness() -> None:
    def classifier(image: Any) -> LabelDistribution:
        return one_hot(0)

    robust_loss = vcl_tf.load_specification(
        path=MNIST_ROBUSTNESS,
        logic=DifferentiableLogic.DL2,
        samplers={"perturbation": ConstantTensorFlowSampler(tf.zeros((28, 28)))},
    )["robust"]

    test_image = tf.convert_to_tensor([[0.0] * 28] * 28)

    loss = robust_loss(
        n=1,
        classifier=classifier,
        epsilon=0.001,
        trainingImages=(test_image,),
        trainingLabels=(0,),
    )
    print(loss)
