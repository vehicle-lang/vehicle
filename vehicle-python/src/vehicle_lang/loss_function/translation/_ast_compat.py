import ast
import sys
from typing import List, Optional, Union

__all__: List[str] = [
    "arguments",
    "unparse",
    "dump",
]

if sys.version_info >= (3, 8):
    from ast import arguments

else:

    def arguments(
        posonlyargs: List[ast.arg],
        args: List[ast.arg],
        vararg: Optional[ast.arg],
        kwonlyargs: List[ast.arg],
        kw_defaults: List[ast.expr],
        kwarg: Optional[ast.arg],
        defaults: List[ast.expr],
    ) -> ast.arguments:
        return ast.arguments(args, vararg, kwonlyargs, kw_defaults, kwarg, defaults)


# NOTE: unparse was added in Python 3.9
if sys.version_info >= (3, 9):
    from ast import unparse
else:
    from astunparse import unparse

# NOTE: indent was added in Python 3.9
if sys.version_info >= (3, 9):
    from ast import dump
else:
    from ast import dump as dump_no_indent

    def dump(
        node: ast.AST,
        annotate_fields: bool = True,
        include_attributes: bool = False,
        *,
        indent: Union[int, str, None] = None,
    ) -> str:
        return dump_no_indent(node, annotate_fields, include_attributes)
