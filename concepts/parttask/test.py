from __future__ import annotations
from typing import *


def test(one: str, two: List[str] | Dict[int], three: any) -> int:
    print(one)
    print(two)
    return one + two

print(test(1, 2, None))

def tse(x: Optional[int | List]) -> tuple[int, int]:
    print(x)
    return (5,4)

tse(None)