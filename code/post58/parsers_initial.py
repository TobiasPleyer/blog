from enum import IntEnum
from functools import partial, reduce
from typing import List
import unittest
import pyparsing as P


def literal(s): return P.Literal(s)
def literal_(s): return P.Suppress(P.Literal(s))
def word(s): return P.Word(s)
def word_(s): return P.Suppress(P.Word(s))


# These have to be implemented
parseLet = None
parseBind = None
parseSeq = None
parseDoLine = None


class ParserTest(unittest.TestCase):

    def test_parseLet(self):
        self.assertEqual(
                parseLet.parseString("let x = 42")[0],
                DoLine(DoLine.Type.LET, "42", "x")
                )

    def test_parseDoLine_with_let(self):
        self.assertEqual(
                parseDoLine.parseString("let x = 42")[0],
                DoLine(DoLine.Type.LET, "42", "x")
                )

    def test_parseSeq(self):
        self.assertEqual(
                parseSeq.parseString("func(arg1, arg2)")[0],
                DoLine(DoLine.Type.SEQ, "func(arg1, arg2)")
                )

    def test_parseDoLine_with_seq(self):
        self.assertEqual(
                parseDoLine.parseString("func(arg1, arg2)")[0],
                DoLine(DoLine.Type.SEQ, "func(arg1, arg2)")
                )

    def test_parseBind(self):
        self.assertEqual(
                parseBind.parseString("res <- func(arg1, arg2)")[0],
                DoLine(DoLine.Type.BIND, "func(arg1, arg2)", "res")
                )

    def test_parseDoLine_with_bind(self):
        self.assertEqual(
                parseDoLine.parseString("res <- func(arg1, arg2)")[0],
                DoLine(DoLine.Type.BIND, "func(arg1, arg2)", "res")
                )


if __name__ == '__main__':
    unittest.main()
