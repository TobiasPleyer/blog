import unittest


def do(s: str) -> str:
    """The do-function.
    Takes a string containing the computation written in Haskell style
    do-notation and returns a string with the computation rewritten using the
    bind (>>=) and sequence (>>) functions of the monad type class.

    This function will render the whole computation in one line without
    indentation.

    The returned string should be valid to be passed to Python's `eval`.
    """
    pass


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
