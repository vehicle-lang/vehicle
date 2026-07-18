from dataclasses import fields
from typing import Any, List
from .._ast._nodes import Main, Lam, SearchRatTensor, DefFunction


Example = tuple[dict, float]


def extract_quantifiers(program: Main) -> dict[str, List[SearchRatTensor]]:
    properties = [decl for decl in program.declarations if isinstance(decl, DefFunction)]
    properties_bound_vars = {}
    for property in properties:
        bound_vars = extract_quantifiers_property(property)
        properties_bound_vars[property.name] = bound_vars
    return properties_bound_vars


def extract_quantifiers_property(property: DefFunction) -> List[SearchRatTensor]:
    quantifiers = []

    def traverse(node):
        if isinstance(node, SearchRatTensor):
            quantifiers.append(node)
            traverse(node.search_lambda.body)
        else:
            return
    
    # If the property contains a neural network, the top node will always be a Lam, not a SearchRatTensor
    if isinstance(property.body, Lam):
        traverse(property.body.body)
    else:
        traverse(property.body)
    
    return quantifiers


def search_witnesses(
    quantifiers,
    neural_network,
    alpha,
    iterations
) -> List[Example]:
    """
    Per property
    """
    pass


def search_counterexamples(
    quantifiers,
    neural_network,
    alpha,
    iterations
) -> List[Example]:
    """
    Per property
    """
    pass


def pgd(
    loss_fn,
    neural_network,
    lower_bound,
    upper_bound,
    alpha,
    iterations,
) -> List[Example]:
    """
    Needs a starting input 
        - depends on the input type of the neural network? 
        - can be random, use seed
        - can also start with lower or upper bound?
    Returns a list of pairs, first element in pair is example (array), second element is loss value
    The only downside of performing the same optimisation algorithm for witnesses and counter-examples is
    that producing a graph of the loss values for counter-examples would show a decreasing trend
    """

    examples = []

    for i in range(iterations):
        # Need to evaluate loss function for each input - don't know how this will work
        # How to project it back into acceptable input region -- see clamp() in samplers.py
        pass
    
    return