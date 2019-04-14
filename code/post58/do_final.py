from enum import IntEnum
from functools import partial, reduce
from typing import List
import unittest
import pyparsing as P


def literal(s): return P.Literal(s)
def literal_(s): return P.Suppress(P.Literal(s))
def word(s): return P.Word(s)
def word_(s): return P.Suppress(P.Word(s))


def compose(*flist):
    def helper(*args, **kwargs):
        f0 = None
        if len(flist) == 0:
            pass
        elif len(flist) == 1:
            f0 = flist[0](*args, **kwargs)
        else:
            f0 = flist[0](*args, **kwargs)
            for fn in flist[1:]:
                f0 = fn(f0)
        return f0
    return helper


def sequence(*flist):
    if len(flist) == 0:
        return None
    else:
        f0 = flist[0]
        try:
            f0 = f0()
        except:
            pass
        return compose(*flist[1:])(f0)


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


def createDoLine(typ, parseResult):
    context = parseResult.asDict()
    return DoLine(typ, **context)

createLet = partial(createDoLine, DoLine.Type.LET)
createSeq = partial(createDoLine, DoLine.Type.SEQ)
createBind = partial(createDoLine, DoLine.Type.BIND)


parseLet = ( literal_("let")
           + word(P.alphanums).setResultsName("var")
           + literal_("=")
           + P.restOfLine.setResultsName("val")
           ).setParseAction(createLet)


parseBind = ( word(P.alphanums).setResultsName("var")
            + literal_("<-")
            + P.restOfLine.setResultsName("val")
            ).setParseAction(createBind)


parseSeq = P.restOfLine.setResultsName("val").setParseAction(createSeq)


parseDoLine = ( parseLet
              | parseBind
              | parseSeq
              )


def isComment(s: str):
    s = s.strip()
    return s and (not s.startswith("#"))


def toDoLine(s: str):
    return parseDoLine.parseString(s)[0]


def assertSeqAtEnd(ds):
    dl = list(ds)
    if len(dl) > 0 and dl[-1].typ != DoLine.Type.SEQ:
        raise ValueError("The last line of a do must be an expression")
    return dl


def deSugar(ds):
    dl = list(ds)
    if len(dl) > 0:
        return ([d.rewrite() for d in dl[:-1]] + ['(' + dl[-1].val + ')'])
    else:
        return []


def matchParentheses(s: str):
    openedPars = s.count('(')
    closedPars = s.count(')')
    return s + (openedPars - closedPars) * ')'


def do(s: str) -> str:
    """The do-function.
    Takes a string containing the computation written in Haskell style
    do-notation and returns a string with the computation rewritten using the
    bind (>>=) and sequence (>>) functions of the monad type class.

    This function will render the whole computation in one line without
    indentation.

    The returned string should be valid to be passed to Python's `eval`.
    """
    return sequence( s
                   , lambda s: s.split('\n')
                   , partial(filter, isComment)
                   , partial(map, toDoLine)
                   , assertSeqAtEnd
                   , deSugar
                   , lambda ss: ''.join(ss)
                   , matchParentheses
                   )


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


class DoTest(unittest.TestCase):

    def test_empty(self):
        self.assertEqual(do(""), "")

    def test_oneline_seq_is_ok(self):
        test_input = 'putStrLn "Test"'
        expected = '(putStrLn "Test")'
        self.assertEqual(do(test_input), expected)

    def test_oneline_bind_fails(self):
        test_input = 'line <- getLine'
        with self.assertRaises(ValueError):
            do(test_input)

    def test_oneline_let_fails(self):
        test_input = 'let x = 42'
        with self.assertRaises(ValueError):
            do(test_input)

    def test_sequence_only(self):
        test_input = """
        putStrLn "Test"
        putStrLn "Test2"
        stackReturn ()
        """
        expected = (
                '(putStrLn "Test")' + ' >> (' +
                '(putStrLn "Test2")' + ' >> (' +
                '(stackReturn ())' +
                '))'
                )
        self.assertEqual(do(test_input), expected)

    def test_one_bind(self):
        test_input = """
        line <- getLine
        putStrLn line
        """
        expected = (
                '(getLine)' + ' | (lambda line: ' +
                '(putStrLn line)' +
                ')'
                )

    def test_two_binds(self):
        test_input = """
        line <- getLine
        line2 <- getLine
        putStrLn line
        putStrLn line2
        """
        expected = (
                '(getLine)' + ' | (lambda line: ' +
                '(getLine)' + ' | (lambda line2: ' +
                '(putStrLn line)' + ' >> (' +
                '(putStrLn line2)' +
                ')))'
                )
        self.assertEqual(do(test_input), expected)

    def test_simple_let(self):
        test_input = """
        let x = 42
        putStrLn (show x)
        """
        expected = (
                '(stackReturn (42))' + ' | (lambda x: ' +
                '(putStrLn (show x))' +
                ')'
                )
        self.assertEqual(do(test_input), expected)

    def test_ignore_empty_line(self):
        test_input = """
        putStrLn "foo"

        putStrLn "bar"
        """
        expected = (
                '(putStrLn "foo")' + ' >> (' +
                '(putStrLn "bar")' +
                ')'
                )
        self.assertEqual(do(test_input), expected)

    def test_ignore_comments(self):
        test_input = """
        # first comment
        res <- complicatedComputation(arg1, arg2)
        # second comment
        shell("more stuff", res)
        """
        expected = (
                '(complicatedComputation(arg1, arg2))' + ' | (lambda res: ' +
                '(shell("more stuff", res))' +
                ')'
                )
        self.assertEqual(do(test_input), expected)

    def test_example(self):
        test_input = """
        putStr "How many gos do you want? "
        line <- getLine
        let cnt = read line :: Int
        sequence_ $ replicate goCnt (putStr "go")
        putStrLn "!"
        """
        expected = (
                '(putStr "How many gos do you want? ")' + ' >> (' +
                '(getLine)' + ' | (lambda line: ' +
                '(stackReturn (read line :: Int))' + ' | (lambda cnt: ' +
                '(sequence_ $ replicate goCnt (putStr "go"))' + ' >> (' +
                '(putStrLn "!")' +
                '))))'
                )
        self.assertEqual(do(test_input), expected)


if __name__ == '__main__':
    unittest.main()
