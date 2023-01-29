from typing import Any, Callable, Dict, List, Optional

import numpy as np
import tensorflow as tf

from command_line import call_vehicle_to_generate_loss_json
from utils import internal_error_msg


def generate_loss_function(
    specification: str,
    function_name: str,
    networks: Dict[str, Any],
    datasets: Optional[Dict[str, Any]] = None,
    parameters: Optional[Dict[str, Any]] = None,
    quantifier_sampling: Optional[Dict[str, Callable[..., Any]]] = None,
) -> Callable[..., Any]:
    """
    specification: path to the vehicle spec .vcl file
    function_name: name of the function for which we want to create the loss function
    networks: dictionary mapping from the name of the networks declared in the spec to the python implementation
    datasets: dictionary mapping from the name of the datasets declared in the spec to the python implementation
    parameters: dictionary mapping from the name of the parameters declared in the spec to the python value
    quantifier_sampling: dictionary with the name and function for sampling
    """
    if datasets is None:
        datasets = {}
    if parameters is None:
        parameters = {}
    if quantifier_sampling is None:
        quantifier_sampling = {}

    json_dict = call_vehicle_to_generate_loss_json(
        specification, networks, datasets, parameters, function_name
    )
    empty_context: List = []
    loss_metadata = LossMetadata(networks, datasets, parameters, quantifier_sampling)
    loss = LossFunctionTranslation().to_loss_function(loss_metadata, json_dict)

    return loss(empty_context)


class LossMetadata:
    def __init__(
        self,
        networks: Dict[str, Any],
        datasets: Dict[str, Any],
        parameters: Dict[str, Any],
        quantifier_sampling: Dict[str, Callable],
    ) -> None:
        self.networks = networks
        self.datasets = datasets
        self.parameters = parameters
        self.quantifier_sampling = quantifier_sampling


class LossFunctionTranslation:
    def to_loss_function(
        self, metadata: LossMetadata, json_dict: Dict[Any, Any]
    ) -> Callable[..., Any]:
        declaration_context: Dict[Any, Any] = {}
        # For now we only translate one function, but we'll need to handle a list of functions:
        # [
        #   [name, {json}],
        #   ...,
        #   [name, {json}]
        # ]
        # for _ in json_dict:
        # declaration_loss = self._translate_expression(resources, json_dict)
        json_dict = json_dict[0]
        if json_dict[1]["tag"] == "Lambda":
            declaration_loss = self._translate_expression(metadata, json_dict[1])
        else:
            # If the expression that we are translating is not a function in vehicle specification, then translaiting
            # naively will result in a constant being returned.
            # This is a problem because, implicitly, the expression is a function in terms of the networks
            # passed in via the metadata and therefore should still change if the network changes.
            # To fix this we therefore wrap it in a lambda with no arguments to delay evaluation.
            declaration_loss = lambda context: lambda: self._translate_expression(
                metadata, json_dict[1]
            )(context)
        return declaration_loss

    def _translate_expression(
        self, metadata: LossMetadata, json_dict: Dict[Any, Any]
    ) -> Callable[..., Any]:
        tag = json_dict["tag"]
        contents = json_dict["contents"]

        # These strings are taken from the constructor names in src/hs/Vehicle/Backend/LossFunction/Compile.hs
        if tag == "Constant":
            return self._translate_constant(contents)
        elif tag == "Variable":
            return self._translate_variable(contents)
        elif tag == "TensorLiteral":
            return self._translate_tensor(contents, metadata)
        elif tag == "Negation":
            return self._translate_negation(contents, metadata)
        elif tag == "Min":
            return self._translate_minimum(contents, metadata)
        elif tag == "Max":
            return self._translate_maximum(contents, metadata)
        elif tag == "Addition":
            return self._translate_addition(contents, metadata)
        elif tag == "Subtraction":
            return self._translate_subtraction(contents, metadata)
        elif tag == "Multiplication":
            return self._translate_multiplication(contents, metadata)
        elif tag == "Division":
            return self._translate_division(contents, metadata)
        elif tag == "Power":
            return self._translate_power(contents, metadata)
        elif tag == "IndicatorFunction":
            return self._translate_indicator(contents, metadata)
        elif tag == "At":
            return self._translate_at(contents, metadata)
        elif tag == "NetworkApplication":
            return self._translate_network(contents, metadata)
        elif tag == "Quantifier":
            return self._translate_quantifier(contents, metadata)
        elif tag == "Lambda":
            return self._translate_lambda(contents, metadata)
        elif tag == "Let":
            return self._translate_let(contents, metadata)
        elif tag == "Domain":
            return self._translate_domain(contents, metadata)
        else:
            raise TypeError(f'Unknown tag "{tag}"')

    def _translate_constant(self, contents: Dict[Any, Any]) -> Callable[..., Any]:
        def result_func(context: Any) -> Any:
            return contents

        return result_func

    def _translate_variable(self, contents: Dict[Any, Any]) -> Callable[..., Any]:
        def result_func(context: Any) -> Any:
            return context[contents]

        return result_func

    def _translate_tensor(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        func_losses = [self._translate_expression(metadata, c) for c in contents]

        def result_func(context: Any) -> Any:
            values_tensor = [l(context) for l in func_losses]
            return tf.convert_to_tensor(values_tensor)

        return result_func

    def _translate_negation(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        loss = self._translate_expression(metadata, contents)

        def result_func(context: Any) -> Any:
            return -loss(context)

        return result_func

    def _translate_minimum(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context: Any) -> Any:
            return min(loss_1(context), loss_2(context))

        return result_func

    def _translate_maximum(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context: Any) -> Any:
            return max(loss_1(context), loss_2(context))

        return result_func

    def _translate_addition(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context: Any) -> Any:
            return loss_1(context) + loss_2(context)

        return result_func

    def _translate_subtraction(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context: Any) -> Any:
            return loss_1(context) - loss_2(context)

        return result_func

    def _translate_multiplication(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context: Any) -> Any:
            return loss_1(context) * loss_2(context)

        return result_func

    def _translate_division(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context: Any) -> Any:
            return loss_1(context) / loss_2(context)

        return result_func

    def _translate_power(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context: Any) -> Any:
            return loss_1(context) ^ loss_2(context)

        return result_func

    def _translate_indicator(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context: Any) -> Any:
            return 1 if loss_1(context) == loss_2(context) else 0

        return result_func

    def _translate_at(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        loss_list = contents[0]
        index = contents[1]
        loss_index = self._translate_expression(metadata, index)
        loss_tensor = self._translate_expression(metadata, loss_list)

        def result_func(context: Any) -> Any:
            return loss_tensor(context)[loss_index(context)]

        return result_func

    def _translate_network(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        model = metadata.networks[contents[0]]
        input_losses = [self._translate_expression(metadata, c) for c in contents[1]]

        def result_func(context: Any) -> Any:
            inputs = [l(context) for l in input_losses]
            output = model(inputs, training=True)
            return output

        return result_func

    def _translate_quantifier(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        quantifier = contents[0]
        variable_name = contents[1]
        domain = contents[2]
        body = contents[3]

        body_loss = self._translate_expression(metadata, body)

        if variable_name in metadata.quantifier_sampling:
            generate_sample = metadata.quantifier_sampling[variable_name]
        else:
            raise Exception(
                "No sampling method provided for variable " + variable_name + "."
            )

        def result_func(context: Any) -> Any:
            max_loss = np.NINF
            min_loss = np.Inf
            # We generate 10 samples, have to change it in the future
            for _ in range(10):
                sample = generate_sample()
                context.insert(0, sample)
                if contents[0] == "All":
                    max_loss = max(max_loss, body_loss(context))
                elif contents[0] == "Any":
                    min_loss = min(min_loss, body_loss(context))
                context.pop(0)
            if contents[0] == "All":
                return max_loss
            elif contents[0] == "Any":
                return min_loss
            else:
                internal_error_msg(
                    "Found a quantifier in the generated json that is not All nor Any."
                )

        return result_func

    def _translate_lambda(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        body = contents[1]
        body_loss = self._translate_expression(metadata, body)

        def result_func(context, v):
            context.insert(0, v)
            return body_loss(context)

        return lambda context: lambda v: result_func(context, v)

    def _translate_let(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        raise NotImplementedError(f"Let bindings are unsupported")

    def _translate_domain(
        self, contents: Dict[Any, Any], metadata: LossMetadata
    ) -> Callable[..., Any]:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context: Any) -> Any:
            return loss_1(context), loss_2(context)

        return result_func
