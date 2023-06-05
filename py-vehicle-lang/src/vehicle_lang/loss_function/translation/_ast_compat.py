import ast
import sys
from typing import List, Optional

__all__: List[str] = [
    "arguments",
    "unparse",
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


if sys.version_info >= (3, 9):
    from ast import unparse
else:
    from astunparse import unparse
