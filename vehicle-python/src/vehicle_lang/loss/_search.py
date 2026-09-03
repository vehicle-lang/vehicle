from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, List, MutableMapping

from vehicle_lang.loss._pytorch.samplers import DefaultPyTorchSampler

from .._ast._nodes import (
    Binder,
    BooleanExpression,
    BooleanTree,
    Conjunct,
    DefAbstract,
    DefFunction,
    Disjunct,
    Expression,
    Lam,
    Main,
    Query,
    SearchRatTensor,
)
from ..typing import DeclarationName

Quantifiers = List[SearchRatTensor]
Binders = List[Binder]


def restructure_search_loss(
    program: Main,
    declaration_context: MutableMapping[str, Any],
    networks: dict[DeclarationName, Any],
    datasets: dict[DeclarationName, Any],
    parameters: dict[DeclarationName, Any],
) -> tuple[dict[str, Quantifiers], Main]:
    """
    Restructures loss ASTs for search to allow them to be evaluated at inputs.
    Adds implementations of networks, datasets and parameters to the declaration context.
    """
    quantifiers_dict = {}
    new_decls = []

    for decl in program.declarations:
        if isinstance(decl, DefAbstract):
            name = decl.name
            sort = decl.sort

            if sort == "Network":
                declaration_context[name] = networks[name]
            if sort == "Dataset":
                declaration_context[name] = datasets[name]
            if sort == "Parameter":
                declaration_context[name] = parameters[name]

        elif isinstance(decl, DefFunction):
            quantifiers, binders, body = separate_quantifiers_binders(decl)
            quantifiers_dict[decl.name] = quantifiers
            reformed_body = reform_lambdas(binders, body)
            new_decl = DefFunction(decl.provenance, decl.name, decl.type, reformed_body)
            new_decls.append(new_decl)

    return quantifiers_dict, Main(new_decls)


def separate_quantifiers_binders(
    decl: DefFunction,
) -> tuple[Quantifiers, Binders, Expression]:
    """
    Traverses a loss AST and separates its quantifiers, binders, and body (underneath all quantifiers).
    """
    all_nodes_traversed = []
    quantifiers = []
    binders = []

    def traverse_loss(node: Expression) -> None:
        all_nodes_traversed.append(node)
        if isinstance(node, SearchRatTensor):
            quantifiers.append(node)
            binders.append(node.search_lambda.binder)
            traverse_loss(node.search_lambda.body)
        else:
            return

    traverse_loss(decl.body)

    if len(all_nodes_traversed) == 0:
        raise TypeError("There should be at least one node in the loss AST.")

    return quantifiers, binders, all_nodes_traversed[-1]


def reform_lambdas(binders: Binders, body: Expression) -> Expression:
    """
    Reforms lambdas around the body of a loss AST.
    """
    # Reverse the order of binders as the lambdas are reformed from inside out
    binders_copy = binders.copy()
    binders_copy.reverse()
    new_body = body

    for binder in binders_copy:
        new_body = Lam(binder, body=new_body)

    return new_body


def search_property(
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
