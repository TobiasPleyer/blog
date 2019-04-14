from enum import IntEnum
from typing import List

class DoLine:
    class Type(IntEnum):
        LET = 1
        SEQ = 2
        BIND = 3

    @classmethod
    def type2Str(cls, typ):
        s = ""
        if typ == cls.Type.LET:
            s = "LET"
        elif typ == cls.Type.SEQ:
            s = "SEQ"
        elif typ == cls.Type.BIND:
            s = "BIND"
        return s

    def __init__(self, typ, val: str, var:str =""):
        self.typ = typ
        self.val = val.strip()
        self.var = var.strip()

    def __str__(self):
        return (self.type2Str(self.typ) + " " + self.var + ": " + self.val)

    def __repr__(self):
        return self.__str__()

    def __eq__(self, other):
        return ((self.typ == other.typ) and
                (self.val == self.val) and
                (self.var == self.var))

    def rewrite(self):
        val = f"({self.val})"
        typ = self.typ
        var = self.var
        if typ == self.Type.LET:
            return f"(stackReturn {val}) | (lambda {var}: "
        elif typ == self.Type.SEQ:
            return f"{val} >> ("
        elif typ == self.Type.BIND:
            return f"{val} | (lambda {var}: "
