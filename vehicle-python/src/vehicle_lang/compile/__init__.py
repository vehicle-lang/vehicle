from collections import defaultdict
from pathlib import Path
from typing import Any, Iterable, Mapping, Optional

from .. import session as session
from ..error import VehicleError as VehicleError
from ..typing import ITP, DeclarationName, DifferentiableLogic, LossBackend, QueryFormat
from . import _ast, error
from .pytorch import DefaultPyTorchSampler, PyTorchSampler
from .tensorflow import DefaultTensorFlowSampler, TensorFlowSampler


def compile_specification(
    path: str | Path,
    target: DifferentiableLogic | QueryFormat | ITP,
    output_file: str | Path,
    declarations: Optional[Iterable[DeclarationName]] = None,
    networks: dict[DeclarationName, str | Path] = {},
    datasets: dict[DeclarationName, str | Path] = {},
    parameters: dict[DeclarationName, Any] = {},
    module_name: Optional[str] = None,
    cache: Optional[str | Path] = None,
) -> str:
    """
    Compile a Vehicle specification to a target language

    :param specification: The path to the Vehicle specification file to compile.
    :param target: The target language to compile to (e.g. QueryFormat.Marabou).
    :param output_file: Output location for the compiled file(s).
    :param declarations: The names of the declarations to compile, defaults to all declarations.
    :param networks: A map from the network names in the specification to files containing the networks.
    :param datasets: A map from the dataset names in the specification to files containing the datasets.
    :param parameters: A map from the parameter names in the specification to the values to be used in compilation.
    :param module_name: Override the name of the exported module (for ITP targets).
    :param cache: The location of the verification cache for ITP compilation.
    """

    args = [
        "compile",
        target._vehicle_option_name,
        "--specification",
        str(path),
    ]

    # Add declarations if specified
    if declarations is not None:
        for declaration_name in set(declarations):
            args.extend(["--declaration", declaration_name])

    # Add networks, datasets, and parameters
    for network_name, network_path in networks.items():
        args.extend(["--network", f"{network_name}:{network_path}"])

    for dataset_name, dataset_path in datasets.items():
        args.extend(["--dataset", f"{dataset_name}:{dataset_path}"])

    for parameter_name, parameter_value in parameters.items():
        args.extend(["--parameter", f"{parameter_name}:{parameter_value}"])

    # Add output file
    args.extend(["--output", str(output_file)])

    # Add module name if specified
    if module_name is not None:
        args.extend(["--module-name", module_name])

    # Add cache if specified
    if cache is not None:
        args.extend(["--cache", str(cache)])

    # Call Vehicle
    exec, out, err, _ = session.check_output(args)

    if exec != 0:
        raise VehicleError(f"{err}")
    elif not out:
        raise VehicleError("Vehicle produced no output")

    return out


def load_specification(
    path: str | Path,
    *,
    backend: LossBackend,
    logic: DifferentiableLogic = DifferentiableLogic.DL2,
    samplers: Mapping[str, Any] | None = None,
    declarations: Iterable[DeclarationName] = (),
    declaration_context: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """
    Load a loss function from a Vehicle specification.

    If samplers is not provided, a default sampler will be used for each
    quantified variable encountered in the specification.
    """
    if declaration_context is None:
        declaration_context = {}

    # Create a defaultdict with the appropriate default sampler based on backend
    if samplers is None:
        match backend:
            case LossBackend.TensorFlow:
                from .tensorflow.samplers import DefaultTensorFlowSampler

                default_sampler_tf = DefaultTensorFlowSampler()
                samplers = defaultdict(lambda: default_sampler_tf.get_loss)
            case LossBackend.PyTorch:
                from .pytorch.samplers import DefaultPyTorchSampler

                default_sampler_pt = DefaultPyTorchSampler()
                samplers = defaultdict(lambda: default_sampler_pt.get_loss)
            case _:
                raise NotImplementedError(f"Backend {backend} not supported.")

    program = _ast.load(
        path,
        target=logic,
        declarations=declarations,
    )

    match backend:
        case LossBackend.TensorFlow:
            from .tensorflow._translation import TensorFlowTranslation

            result = TensorFlowTranslation().compile(
                program=program,
                path=path,
                declaration_context=declaration_context,
                samplers=samplers,
            )
        case LossBackend.PyTorch:
            from .pytorch._translation import PyTorchTranslation

            result = PyTorchTranslation().compile(
                program=program,
                path=path,
                declaration_context=declaration_context,
                samplers=samplers,
            )
        case _:
            raise NotImplementedError(f"Backend {backend} not supported.")
    return result


def call_vehicle(args: list[str]) -> str:
    # Call Vehicle
    exec, out, err, _ = session.check_output(args)

    if exec != 0:
        raise VehicleError(f"{err}")
    elif not out:
        raise VehicleError("Vehicle produced no output")

    return out


__all__ = [
    "compile_specification",
    "load_specification",
    "call_vehicle",
    "PyTorchSampler",
    "DefaultPyTorchSampler",
    "TensorFlowSampler",
    "DefaultTensorFlowSampler",
    "error",
]
