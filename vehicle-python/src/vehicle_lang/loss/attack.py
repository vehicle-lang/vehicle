from dataclasses import fields
from typing import List
from .._ast._nodes import Main, Lam, SearchRatTensor, DefFunction

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
