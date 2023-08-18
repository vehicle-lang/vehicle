from pathlib import Path
from typing import Any, Dict, Tuple, cast

from typing_extensions import TypeAlias

GOLDEN_PATH = (
    Path(__file__).parent.parent / "vendor" / "vehicle" / "tests" / "golden" / "compile"
)
MNIST_ROBUSTNESS = GOLDEN_PATH / "mnist-robustness" / "spec.vcl"

Image: TypeAlias = Tuple[Tuple[float, ...], ...]

Perturbation: TypeAlias = Image

Label: TypeAlias = int

LabelDistribution: TypeAlias = Tuple[
    float, float, float, float, float, float, float, float, float, float
]


def one_hot(label: Label) -> LabelDistribution:
    return cast(
        LabelDistribution,
        tuple([1.0 if index == label else 0.0 for index in range(0, 10)]),
    )


def test_lossdl2_exec_tf_mnist_robustness() -> None:
    try:
        import tensorflow as tf
        import vehicle_lang.tensorflow as vcl2tf

        def domain_for_pertubation(
            ctx: Dict[str, Any]
        ) -> vcl2tf.VariableDomain[tf.Tensor]:
            epsilon = tf.convert_to_tensor(ctx["epsilon"])
            return vcl2tf.BoundedVariableDomain.from_bounds(
                lower_bounds=tf.fill(dims=[28, 28], value=tf.negative(epsilon)),
                upper_bounds=tf.fill(dims=[28, 28], value=epsilon),
                dtype=tf.float32,
            )

        def classifier(image: Image) -> LabelDistribution:
            return one_hot(0)

        robust_loss = vcl2tf.load_loss_function(
            MNIST_ROBUSTNESS,
            property_name="robust",
            target=vcl2tf.DifferentiableLogic.DL2,
            dtype_rat=tf.float32,
            domains={"pertubation": domain_for_pertubation},
        )

        loss = robust_loss(
            n=1,
            classifier=classifier,
            epsilon=0.001,
            trainingImages=(tf.zeros(shape=(28, 28)),),
            trainingLabels=(tf.convert_to_tensor(0),),
        )
        print(loss)

    except ModuleNotFoundError:
        from logging import warning

        warning("test_lossdl2_exec_tf_mnist_robustness requires tensorflow")
