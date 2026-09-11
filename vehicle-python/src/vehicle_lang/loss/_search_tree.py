from dataclasses import dataclass
from typing import Any, Callable, List, Sequence, cast

import torch
from vehicle_lang._ast._nodes import (
    BooleanExpression,
    BooleanTree,
    Conjunct,
    Disjunct,
    NonTrivialQuery,
    TrivialQuery,
)
from vehicle_lang.loss._common import BoundVarData
from vehicle_lang.loss._pytorch.samplers import DefaultPyTorchSampler


@dataclass
class Sample:
    inputs: dict[str, torch.Tensor]
    loss_history: List[float]


def search_tree(
    boolean_tree: BooleanTree,
    declarations: dict[str, Any],
    bound_vars: dict[str, Any],
    samplers: dict[str, Any] | None = None,
) -> tuple[bool, Sequence[Sample]]:

    def traverse_boolean_expr(node: BooleanExpression) -> tuple[bool, Sequence[Sample]]:

        if isinstance(node, NonTrivialQuery):
            for query_disjunct in node.disjunct_all:
                # print(f"Searching {query_disjunct}")
                loss_fn = declarations[query_disjunct]
                bool_fn = declarations[f"{query_disjunct}_bool"]
                bound_var_data = bound_vars[query_disjunct]

                sample = search_witness(bound_var_data, loss_fn, samplers)

                # Check if sample found by PGD is an actual witness
                is_witness = bool_fn(**sample.inputs).item()
                # print(is_witness)

                # If we have found a witness to a query disjunct (i.e. a witness to the entire query), and the query is negated
                # the boolean result propagated up from here is False
                if is_witness is True:
                    if node.negated:
                        return (False, [sample])
                    # If we have found a witness to a query disjunct, but the query is not negated
                    # the boolean value propagated up from here is True
                    else:
                        return (True, [sample])
                # If we did not manage to find a witness to the current query disjunct,
                # keep searching other disjuncts.

            # If we did not manage to find a witness to any query disjunct, and the query is negated
            # the boolean result propagated up from here is True
            if node.negated:
                return (True, [])
            # If we did not manage to find a witness to any query disjunct, and the query is not negated
            # the boolean result propagated up from here is False
            else:
                return (False, [])

        elif isinstance(node, Conjunct):
            new_samples_after_conjunct: list[Sample] = []

            for conjunct in node.conjunct_all:
                boolean_result, samples = traverse_boolean_expr(conjunct)

                # After traversing into the current conjunct, if the boolean result propagated up is False,
                # there is no need to traverse into other conjuncts
                if boolean_result is False:
                    return (False, samples)
                # If the boolean result propagated up is True, we need to keep traversing into other conjuncts and
                # form a new set of samples with the sample propagated up
                else:
                    new_samples_after_conjunct += samples
            return (True, new_samples_after_conjunct)

        elif isinstance(node, Disjunct):
            new_samples_after_disjunct: list[Sample] = []
            for disjunct in node.disjunct_all:
                boolean_result, samples = traverse_boolean_expr(disjunct)

                # After traversing into the current disjunct, if the boolean result propagated up is True,
                # there is no need to traverse into other disjuncts
                if boolean_result is True:
                    return (True, samples)
                # If the boolean result propagated up is False, we need to keep traversing into other disjuncts and
                # form a new set of samples with the sample propagated up
                else:
                    new_samples_after_disjunct += samples
            return (False, new_samples_after_disjunct)

        else:
            node = cast(TrivialQuery, node)
            return (node.boolean_value, [])

    result = traverse_boolean_expr(boolean_tree.boolean_expression)
    return result


def search_witness(
    bound_var_data: Sequence[BoundVarData],
    loss_fn: Callable[..., torch.Tensor],
    samplers: dict[str, Any] | None = None,
) -> Sample:
    """
    Uses gradient descent to search for a single witness. A round-robin approach is used to find
    an optimal input for each bound variable in turn.

    NOTE - This only works using default PyTorch samplers at the moment.

    Args:
        bound_vars: Contains the name, dims, lower bound and upper bound of each bound
            variable to optimise
        loss_fn: A callable representing the loss function to minimise
        samplers: Sampler objects for each bound variable to optimise
    Returns:
    A Sample object containing the input for each bound variable, and the losses
    """

    # Helper function to update current inputs after optimising a bound variable
    def update_current_inputs(
        current_inputs: dict[str, Any], bound_var: BoundVarData, value: torch.Tensor
    ) -> dict[str, Any]:
        new_inputs = current_inputs.copy()
        new_inputs[bound_var.name] = value
        return new_inputs

    # Set starting points for all bound variables
    current_inputs = {}
    for bound_var in bound_var_data:
        dims = bound_var.dims
        upper_bound = bound_var.upper_bound
        lower_bound = bound_var.lower_bound
        range_size = upper_bound - lower_bound
        initial_point = (
            lower_bound + torch.rand(dims, dtype=lower_bound.dtype) * range_size
        )
        current_inputs[bound_var.name] = initial_point

    # At the moment, use default PyTorch samplers if no samplers are provided
    if samplers is None:
        samplers = {}
        for bound_var in bound_var_data:
            # We only optimise each bound variable for one trajectory with 10 steps
            samplers[bound_var.name] = DefaultPyTorchSampler(
                num_samples=1, num_steps=10
            )

    loss_history = []
    # Find an optimal input for each bound variable one at a time while keeping all other
    # inputs constant
    for bound_var in bound_var_data:
        sampler: DefaultPyTorchSampler = samplers[bound_var.name]
        loss, new_input = sampler.get_loss_and_input(
            dims=bound_var.dims,
            lower_bound=bound_var.lower_bound,
            upper_bound=bound_var.upper_bound,
            search_lambda=lambda value: loss_fn(
                **update_current_inputs(current_inputs, bound_var, value)
            ),
            search=True,
        )
        loss_history += loss.tolist()
        current_inputs = update_current_inputs(current_inputs, bound_var, new_input)

    return Sample(inputs=current_inputs, loss_history=loss_history)
