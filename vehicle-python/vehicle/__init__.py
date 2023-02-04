import typing
from typing import Any, Callable, Dict, Optional, Set, Tuple

import tensorflow as tf

from .command_line import call_vehicle_to_generate_loss_json
from .utils import internal_error_msg


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
    translation = LossFunctionTranslation(
        networks, datasets, parameters, quantifier_sampling
    )
    loss = translation.to_loss_function(function_name, json_dict)
    return loss


class LossFunctionTranslation:
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
        self.quantifier_sampling: Dict[str, Callable] = quantifier_sampling
        self.current_decl = None

        self.debug: bool = False
        self.debug_vars: Set[Tuple[str, str]] = set()  # {('boundedByEpsilon', 'x')}

    def to_loss_function(
        self, function_name: str, json_dict: Dict[Any, Any]
    ) -> Callable[..., Any]:

        decl_ctx: Dict[str, Callable[..., Any]] = {}
        for [ident, decl] in json_dict:
            self.current_decl = ident

            decl_loss_str = self._translate_expression(decl)
            if decl["tag"] != "Lambda":
                # If the expression that we are translating is not a function in vehicle specification, then translating
                # naively will result in a constant being returned.
                # This is a problem because, implicitly, the expression is a function in terms of the networks
                # passed in via the metadata and therefore should still change if the network changes.
                # To fix this we therefore wrap it in a lambda with no arguments to delay evaluation.
                decl_loss_str = "lambda: " + decl_loss_str
            decl_loss_str = "loss_fn = " + decl_loss_str

            decl_loss_bytecode = compile(decl_loss_str, f"<{ident}>", "exec")

            network_ctx = self.networks
            result_ctx = {"loss_fn": None}
            sample_ctx = {
                "sample_" + var_name: sample
                for (var_name, sample) in self.quantifier_sampling.items()
            }
            tensorflow_ctx = {"to_tensor": tf.convert_to_tensor}
            global_scope = {**decl_ctx, **network_ctx, **sample_ctx, **tensorflow_ctx}
            local_scope = {**result_ctx}

            if self.debug:
                print("")
                print(f"Declaration: {ident}")
                print(f"Compiled expression: {decl_loss_str}")
                print(f"Local scope: {list(local_scope.keys())}")
                print(f"Global scope: {list(global_scope.keys())}")

            exec(decl_loss_bytecode, global_scope, local_scope)
            loss_fn = typing.cast(Callable[..., Any], local_scope["loss_fn"])

            decl_ctx[ident] = loss_fn

        return decl_ctx[function_name]

    def _translate_expression(self, json_dict: Dict[Any, Any]) -> str:
        tag = json_dict["tag"]
        contents = json_dict["contents"]

        # These strings are taken from the constructor names in src/hs/Vehicle/Backend/LossFunction/Compile.hs
        if tag == "Constant":
            return self._translate_constant(contents)
        elif tag == "Variable":
            return self._translate_variable(contents)
        elif tag == "TensorLiteral":
            return self._translate_tensor(contents)
        elif tag == "Negation":
            return self._translate_negation(contents)
        elif tag == "Min":
            return self._translate_minimum(contents)
        elif tag == "Max":
            return self._translate_maximum(contents)
        elif tag == "Addition":
            return self._translate_addition(contents)
        elif tag == "Subtraction":
            return self._translate_subtraction(contents)
        elif tag == "Multiplication":
            return self._translate_multiplication(contents)
        elif tag == "Division":
            return self._translate_division(contents)
        elif tag == "Power":
            return self._translate_power(contents)
        elif tag == "IndicatorFunction":
            return self._translate_indicator(contents)
        elif tag == "At":
            return self._translate_at(contents)
        elif tag == "NetworkApplication":
            return self._translate_network(contents)
        elif tag == "FreeVariable":
            return self._translate_free_variable(contents)
        elif tag == "Quantifier":
            return self._translate_quantifier(contents)
        elif tag == "Lambda":
            return self._translate_lambda(contents)
        elif tag == "Let":
            return self._translate_let(contents)
        else:
            raise TypeError(f'Unknown tag "{tag}"')

    def _translate_constant(self, contents: Dict[Any, Any]) -> str:
        return str(contents)

    def _translate_variable(self, contents: Dict[Any, Any]) -> str:
        return str(contents)

    def _translate_tensor(self, contents: Dict[Any, Any]) -> str:
        elements = ", ".join([self._translate_expression(c) for c in contents])
        return f"to_tensor([{elements}])"

    def _translate_negation(self, contents: Dict[Any, Any]) -> str:
        loss = self._translate_expression(contents)
        return f"-{loss}"

    def _translate_minimum(self, contents: Dict[Any, Any]) -> str:
        loss_1 = self._translate_expression(contents[0])
        loss_2 = self._translate_expression(contents[1])
        return f"min({loss_1}, {loss_2})"

    def _translate_maximum(self, contents: Dict[Any, Any]) -> str:
        loss_1 = self._translate_expression(contents[0])
        loss_2 = self._translate_expression(contents[1])
        return f"max({loss_1}, {loss_2})"

    def _translate_addition(self, contents: Dict[Any, Any]) -> str:
        loss_1 = self._translate_expression(contents[0])
        loss_2 = self._translate_expression(contents[1])
        return f"{loss_1} + {loss_2}"

    def _translate_subtraction(self, contents: Dict[Any, Any]) -> str:
        loss_1 = self._translate_expression(contents[0])
        loss_2 = self._translate_expression(contents[1])
        return f"{loss_1} - {loss_2}"

    def _translate_multiplication(self, contents: Dict[Any, Any]) -> str:
        loss_1 = self._translate_expression(contents[0])
        loss_2 = self._translate_expression(contents[1])
        return f"{loss_1} * {loss_2}"

    def _translate_division(self, contents: Dict[Any, Any]) -> str:
        loss_1 = self._translate_expression(contents[0])
        loss_2 = self._translate_expression(contents[1])
        return f"{loss_1} / {loss_2}"

    def _translate_power(self, contents: Dict[Any, Any]) -> str:
        loss_1 = self._translate_expression(contents[0])
        loss_2 = self._translate_expression(contents[1])
        return f"{loss_1} ** {loss_2}"

    def _translate_indicator(self, contents: Dict[Any, Any]) -> str:
        loss_1 = self._translate_expression(contents[0])
        loss_2 = self._translate_expression(contents[1])
        return f"1 if {loss_1} == {loss_2} else 0"

    def _translate_at(self, contents: Dict[Any, Any]) -> str:
        vector = self._translate_expression(contents[0])
        index = self._translate_expression(contents[1])
        return f"{vector}[{index}]"

    def _translate_network(self, contents: Dict[Any, Any]) -> str:
        model = contents[0]
        input_losses = ", ".join([self._translate_expression(c) for c in contents[1]])
        return f"{model}({input_losses}, training=True)"

    def _translate_free_variable(self, contents: Dict[Any, Any]) -> str:
        free_var = contents[0]
        input_losses = "".join(
            ["(" + self._translate_expression(c) + ")" for c in contents[1]]
        )
        return f"{free_var}{input_losses}"

    def _translate_quantifier(self, contents: Dict[Any, Any]) -> str:
        quantifier = contents[0]
        variable_name = contents[1]
        domain = contents[2]
        body = self._translate_expression(contents[3])

        if variable_name in self.quantifier_sampling:
            generate_sample = self.quantifier_sampling[variable_name]
        else:
            raise Exception(
                "No sampling method provided for variable " + variable_name + "."
            )

        # max_loss = np.NINF
        # min_loss = np.Inf
        # We generate 10 samples, have to change it in the future
        if quantifier == "All":
            op = "max"
        elif quantifier == "Any":
            op = "min"
        else:
            internal_error_msg(
                "Found a quantifier in the generated json that is not All nor Any."
            )

        return "{0}([(lambda {1}: {2})(sample_{1}()) for _ in range(10)])".format(
            op, variable_name, body
        )

    def _translate_lambda(self, contents: Dict[Any, Any]) -> str:
        var_name = contents[0]
        body = self._translate_expression(contents[1])
        if (self.current_decl, var_name) not in self.debug_vars:
            return f"lambda {var_name}: {body}"
        else:
            return "lambda {0}: (print('{0} = '+ str({0})) or ({1}))".format(
                var_name, body
            )

    def _translate_let(self, contents: Dict[Any, Any]) -> str:
        raise NotImplementedError(f"Let bindings are unsupported")
