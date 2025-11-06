#!/usr/bin/env python3

import json
import sys
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Sequence, Tuple, Union, TypeVar
from typing_extensions import Self
from pathlib import Path
from fractions import Fraction

# Minimal AST classes for testing
@dataclass(frozen=True)
class AST:
    @classmethod  
    def from_dict(cls, value: Any) -> Self:
        # Simple decoding logic for testing
        if isinstance(value, dict) and "tag" in value:
            tag = value["tag"]
            contents = value.get("contents", [])
            
            # Map tag names to classes
            class_map = {
                "Main": Main,
                "DefFunction": DefFunction,
                "AddRatTensor": AddRatTensor,
                "SubRatTensor": SubRatTensor,
                "MulRatTensor": MulRatTensor,
                "Var": Var,
                "Provenance": Provenance,
                "Pi": Pi,
                "RatType": RatType,
                "TensorType": TensorType,
                "Lam": Lam,
                "Binder": Binder,
                "RatTensor": RatTensor,
            }
            
            if tag in class_map:
                cls_type = class_map[tag]
                if tag == "Main":
                    return cls_type([AST.from_dict(d) for d in contents])
                elif tag in ["AddRatTensor", "SubRatTensor", "MulRatTensor"]:
                    return cls_type(tuple(AST.from_dict(d) for d in contents))
                elif tag == "Var":
                    return cls_type((contents[0], [AST.from_dict(d) for d in contents[1]]))
                elif tag == "DefFunction":
                    return cls_type(contents)
                else:
                    return cls_type(contents)
            else:
                print(f"Unknown tag: {tag}")
                return UnknownTag(tag, contents)
        else:
            return value

@dataclass(frozen=True)
class Program(AST):
    pass

@dataclass(frozen=True)  
class Main(Program):
    declarations: Sequence[Any]

@dataclass(frozen=True)
class DefFunction(AST):
    contents: Any

@dataclass(frozen=True)
class AddRatTensor(AST):
    contents: Tuple[Any, Any]

@dataclass(frozen=True)
class SubRatTensor(AST):
    contents: Tuple[Any, Any]

@dataclass(frozen=True)
class MulRatTensor(AST):  
    contents: Tuple[Any, Any]

@dataclass(frozen=True)
class Var(AST):
    contents: Tuple[str, Sequence[Any]]

@dataclass(frozen=True)
class Provenance(AST):
    contents: Any

@dataclass(frozen=True)
class Pi(AST):
    contents: Any

@dataclass(frozen=True)
class RatType(AST):
    contents: Any

@dataclass(frozen=True)
class TensorType(AST):
    contents: Any

@dataclass(frozen=True)
class Lam(AST):
    contents: Any

@dataclass(frozen=True)
class Binder(AST):
    contents: Any

@dataclass(frozen=True)
class RatTensor(AST):
    contents: Any

@dataclass(frozen=True)
class UnknownTag(AST):
    tag: str
    contents: Any

# Test
if __name__ == "__main__":
    print('Testing JSON parsing with complex example...')
    try:
        with open('test_complex.json') as f:
            data = json.load(f)
        
        print('Parsing JSON into AST...')
        prog = Program.from_dict(data)
        print('✅ JSON parsing successful!')
        
        print(f'Program type: {type(prog)}')
        if hasattr(prog, 'declarations'):
            print(f'Number of declarations: {len(prog.declarations)}')
            for i, decl in enumerate(prog.declarations):
                print(f'  Declaration {i}: {type(decl).__name__}')
                if hasattr(decl, 'contents'):
                    print(f'    Contents: {len(decl.contents) if isinstance(decl.contents, (list, tuple)) else type(decl.contents)}')
        
    except Exception as e:
        print(f'❌ Error: {e}')
        import traceback
        traceback.print_exc()
