import sys
from typing import List

################################################################################
# Export List
################################################################################

__all__: List[str] = [
    "singledispatchmethod",
]

################################################################################
# Python 3.7: itertools.accumulate
################################################################################

if sys.version_info >= (3, 8):
    from functools import singledispatchmethod as singledispatchmethod

else:
    from singledispatchmethod import singledispatchmethod as singledispatchmethod
