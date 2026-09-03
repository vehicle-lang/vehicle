from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, List, MutableMapping

from .._ast._nodes import (
    Binder,
    DefAbstract,
    DefFunction,
    Expression,
    Lam,
    Main,
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
            new_decl = DefFunction(
                provenance=decl.provenance,
                name=decl.name,
                isProperty=True,
                type=decl.type,
                body=reformed_body,
            )
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
