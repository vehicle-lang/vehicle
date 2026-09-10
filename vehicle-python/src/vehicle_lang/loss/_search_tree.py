from dataclasses import dataclass
from typing import Any, Sequence, cast

from vehicle_lang._ast._nodes import (
    BooleanExpression,
    BooleanTree,
    Conjunct,
    Disjunct,
    NonTrivialQuery,
    TrivialQuery,
)
from vehicle_lang.loss._pytorch.samplers import DefaultPyTorchSampler, Sample


def search_tree(
    boolean_tree: BooleanTree,
    declarations: dict[str, Any],
    bound_vars: dict[str, Any],
    sampler: DefaultPyTorchSampler,  # samplers dictionary containing ABCSamplers (see training backend)
) -> tuple[bool, Sequence[Sample]]:

    boolean_result = False

    def traverse_boolean_expr(node: BooleanExpression) -> Sequence[Sample]:

        if isinstance(node, NonTrivialQuery):
            for query_disjunct in node.disjunct_all:
                print(f"Searching {query_disjunct}")
                loss_fn = declarations[query_disjunct]
                bool_fn = declarations[f"{query_disjunct}_bool"]
                bound_var_data = bound_vars[query_disjunct]

                sample = sampler.pgd(bound_var_data, loss_fn)

                # Check if sample found by PGD is an actual witness
                is_witness = bool_fn(**sample.inputs).item()
                print(is_witness)

                # If we have found a witness to a query disjunct (i.e. a witness to the entire query), and the query is negated
                # the boolean result propagated up from here is False
                if is_witness is True:
                    if node.negated:
                        boolean_result = False
                    # If we have found a witness to a query disjunct, but the query is not negated
                    # the boolean value propagated up from here is True
                    else:
                        boolean_result = True
                    return [sample]
                # If we did not manage to find a witness to the current query disjunct,
                # keep searching other disjuncts.

            # If we did not manage to find a witness to any query disjunct, and the query is negated
            # the boolean result propagated up from here is True
            if node.negated:
                boolean_result = True
            # If we did not manage to find a witness to any query disjunct, and the query is not negated
            # the boolean result propagated up from here is False
            else:
                boolean_result = False
            return []

        elif isinstance(node, Conjunct):
            new_samples_after_conjunct: list[Sample] = []

            for conjunct in node.conjunct_all:
                samples = traverse_boolean_expr(conjunct)

                # After traversing into the current conjunct, if the boolean result propagated up is False,
                # there is no need to traverse into other conjuncts
                if boolean_result is False:
                    return samples
                # If the boolean result propagated up is True, we need to keep traversing into other conjuncts and
                # form a new set of samples with the sample propagated up
                else:
                    new_samples_after_conjunct += samples
            return new_samples_after_conjunct

        elif isinstance(node, Disjunct):
            new_samples_after_disjunct: list[Sample] = []
            for disjunct in node.disjunct_all:
                samples = traverse_boolean_expr(disjunct)

                # After traversing into the current disjunct, if the boolean result propagated up is True,
                # there is no need to traverse into other disjuncts
                if boolean_result is True:
                    return samples
                # If the boolean result propagated up is False, we need to keep traversing into other disjuncts and
                # form a new set of samples with the sample propagated up
                else:
                    new_samples_after_disjunct += samples
            return new_samples_after_disjunct

        else:
            node = cast(TrivialQuery, node)
            boolean_result = node.boolean_value
            return []

    samples = traverse_boolean_expr(boolean_tree.boolean_expression)
    return (boolean_result, samples)
