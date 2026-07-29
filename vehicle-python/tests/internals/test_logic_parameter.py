################
## Tensorflow ##
################

import pytest
from vehicle_lang.typing import CustomDifferentiableLogic

tf = pytest.importorskip(
    "tensorflow",
    reason="TensorFlow extra is required for this test",
)

###################
## Other imports ##
###################

from typing import Any, cast

import vehicle_lang.loss.tensorflow as vcl_tf
from vehicle_lang import DL2DifferentiableLogic
from vehicle_lang.loss._tensorflow.samplers import ConstantTensorFlowSampler

from ..config import PYTHON_TEST_SPECS_PATH

##########
## Test ##
##########


def test_loss_parameter() -> None:
    loss_fn = vcl_tf.load_specification(
        path=PYTHON_TEST_SPECS_PATH / "test_logic_parameters.vcl",
        logic=CustomDifferentiableLogic("capucciAdditive"),
    )["property"]

    loss = loss_fn(
        p=1,
        f=lambda x: x,
    )
    print(loss)
