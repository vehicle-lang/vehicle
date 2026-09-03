from typing import Any

from vehicle_lang._ast._nodes import (
    BooleanExpression,
    BooleanTree,
    Conjunct,
    Disjunct,
    Query,
)
from vehicle_lang.loss._pytorch.samplers import DefaultPyTorchSampler


def search_tree(
    boolean_tree: BooleanTree,
    declarations: dict[str, Any],
    bound_vars: dict[str, Any],
    sampler: DefaultPyTorchSampler,
) -> Any | None:

    boolean_result = False
    # Keep track of all the potential counter-examples found
    potential_counterexamples = []

    def traverse_boolean_expr(node: BooleanExpression) -> None:
        if isinstance(node, Query):
            for query_disjunct in node.disjunct_all:
                print(f"Searching {query_disjunct} \n")
                loss_fn = declarations[query_disjunct]
                bound_var_data = bound_vars[query_disjunct]

                sample = sampler.pgd(bound_var_data, loss_fn)

                # Need to check if sample is an actual witness
                witness = sample

                # If we have found a witness to a query disjunct, and the query is negated
                # the boolean result propagated up from this query is False
                if witness is not None:
                    if node.negated:
                        boolean_result = False
                        potential_counterexamples.append(witness)
                        return
                    # If we have found a witness to a query disjunct, but the query is not negated
                    # the boolean value propagated up from this query is True
                    else:
                        boolean_result = True
                # If we did not manage to find a witness to the current query disjunct,
                # keep searching other disjuncts.

        if isinstance(node, Conjunct):
            for conjunct in node.conjunct_all:
                traverse_boolean_expr(conjunct)

                # After traversing into the current conjunct, if the boolean result propagated up is False,
                # there is no need to traverse into other conjuncts
                if boolean_result is False:
                    return

        if isinstance(node, Disjunct):
            for disjunct in node.disjunct_all:
                traverse_boolean_expr(disjunct)

                # After traversing into the current disjunct, if the boolean result propagated up is True,
                # there is no need to traverse into other disjuncts
                if boolean_result is True:
                    return

    traverse_boolean_expr(boolean_tree.boolean_expression)

    if boolean_result is False:
        # Should evaluate the counter-example with the neural network to see what the output is
        return potential_counterexamples[-1]
    return None
