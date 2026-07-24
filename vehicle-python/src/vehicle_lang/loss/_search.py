from dataclasses import dataclass
from typing import Any, List, MutableMapping
from ..typing import DeclarationName
from .._ast._nodes import Main, Expression, Lam, Binder, SearchRatTensor, DefAbstract, DefFunction

Quantifiers = List[SearchRatTensor]
Binders = List[Binder]


def restructure_search_loss(
    program: Main,
    declaration_context: MutableMapping[str, Any],
    networks: dict[DeclarationName, Any],
    datasets: dict[DeclarationName, Any],
    parameters: dict[DeclarationName, Any]
) -> tuple[dict[str, Quantifiers], Main]:
    """
    Restructures loss functions for search to allow them to be evaluated at inputs.
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


def separate_quantifiers_binders(property: DefFunction) -> tuple[Quantifiers, Binders, Expression]:
    """
    Traverses a loss AST and separates its quantifiers, binders, and body (underneath all quantifiers).
    """
    all_nodes = []
    quantifiers = []
    binders = []

    def traverse(node):
        all_nodes.append(node)
        if isinstance(node, SearchRatTensor):
            quantifiers.append(node)
            binders.append(node.search_lambda.binder)
            traverse(node.search_lambda.body)
        else:
            return
    
    # If the property contains a neural network, the top node will always be a Lam, not a SearchRatTensor
    if isinstance(property.body, Lam):
        traverse(property.body.body)
    else:
        traverse(property.body)

    if len(all_nodes) == 0:
        raise TypeError("There should be at least one node in the property.")
    
    return quantifiers, binders, all_nodes[-1]


def reform_lambdas(
    binders: Binders, 
    body: Expression
) -> Expression:
    """
    Reforms lambdas around the body of a loss function.
    """
    # Reverse the order of binders as the lambdas are reformed from inside out
    binders_copy = binders.copy()
    binders_copy.reverse()
    new_expression = body

    for binder in binders_copy:
        new_expression = Lam(binder, body=new_expression)
    
    return new_expression