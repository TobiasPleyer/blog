from functools import reduce
from subprocess import run, PIPE, STDOUT

class Either():
    Left = 0
    Right = 1
    def __init__(self, kind, value=None):
        self._kind = kind
        self._value = value

    def unwrap(self):
        return self._value

    @staticmethod
    def mreturn(value):
        return Either(Either.Right, value)

    def get_return(self):
        return Either.mreturn

    def __or__(self, k):
        if isLeft(self):
            return self
        else:
            return k(self.unwrap())

    def __rshift__(self, k):
        return self.__or__(lambda _: k)

    def __str__(self):
        if self._kind == Either.Left:
            return f"Left {repr(self._value)}"
        else:
            return f"Right {repr(self._value)}"

    def __repr__(self):
        return self.__str__()


def isLeft(e):
    return e._kind == Either.Left


def isRight(e):
    return e._kind == Either.Right


def left(a):
    return Either(Either.Left, a)


def right(a):
    return Either(Either.Right, a)


class IO():
    def __init__(self, value):
        self._value = value

    @staticmethod
    def mreturn(value):
        return IO(lambda: value)

    def get_return(self):
        return IO.mreturn

    def unwrap(self):
        return self._value

    def run(self):
        return self._value()

    def __or__(self, k):
        return IO(lambda: k(self.run()).run())

    def __rshift__(self, k):
        return self.__or__(lambda _: k)

    def __str__(self):
        return "IO {repr(self._value)}"

    def __repr__(self):
        return self.__str__()


class WriterT():
    def __init__(self, value):
        self._value = value

    def get_return(self):
        inner_return = self.unwrap().get_return()
        return lambda value: WriterT(inner_return((value,[])))

    def unwrap(self):
        return self._value

    def __or__(self, k):
        inner_ret = self.unwrap().mreturn
        return WriterT(
            self.unwrap() |(
            lambda x1: k(x1[0]).unwrap() |(
            lambda x2: inner_ret((x2[0], x1[1] + x2[1])))))

    def __rshift__(self, k):
        return self.__or__(lambda _: k)

    def __str__(self):
        return f"WriterT {repr(self._value)} "

    def __repr__(self):
        return self.__str__()


class EitherT():
    def __init__(self, value):
        self._value = value

    def get_return(self):
        inner_return = self.unwrap().get_return()
        return lambda value: EitherT(inner_return(right(value)))

    def unwrap(self):
        return self._value

    def __or__(self, k):
        inner_ret = self.unwrap().get_return()
        return EitherT(
            self.unwrap() |(
            lambda a: inner_ret(a) if isLeft(a) else k(a.unwrap()).unwrap()))

    def __rshift__(self, k):
        return self.__or__(lambda _: k)

    def __str__(self):
        return f"EitherT {repr(self._value)} "

    def __repr__(self):
        return self.__str__()


# The low level function handling the execution of the external program.
# In Haskell it would have the signature
# do_shell :: String -> IO (Either () (), [String])
def do_shell(cmd):
    process = run(cmd, stdout=PIPE, stderr=PIPE, shell=True)
    stdout = process.stdout.decode('utf-8')
    stderr = process.stderr.decode('utf-8')
    info = [f"Command run: {cmd}"]
    if stdout:
        info.append(stdout)
    if stderr:
        info.append(stderr)
    if process.returncode > 0:
        return (left(stderr), info)
    return (right(stdout), info)

# Helper function to construct the underlying data type
def shell(cmd):
    # This is the big boy: A monadic stack, stacking WriterT and EitherT on
    # top of the IO monad First the IO action, it must already follow the
    # conventions of the stack, i.e. return a tuple of (Either a b,[String]).
    # The first entry is used by the stacked EitherT monad and the second
    # one is the monoid instance for the stacked WriterT monad
    return EitherT(WriterT(IO(lambda: do_shell(cmd))))


def runAction(action):
    return action.unwrap().unwrap().run()


if __name__ == '__main__':

    # EXAMPLES
    cmd1 = shell("echo 'OK'; exit 0")
    cmd2 = shell("echo 'Also OK'; exit 0")
    cmd3 = shell("echo 'Even better'; exit 0")
    full_action = cmd1 >> cmd2 >> cmd3
    res, info = runAction(full_action)
    print(f"Final result: {res}")
    print("== INFO ==")
    print(reduce(lambda x, y: x + '\n' + y, info))
    cmd4 = shell("echo 'Command failed' >&2; exit 1")
    full_action = cmd1 >> cmd4 >> cmd3
    res, info = runAction(full_action)
    print(f"Final result: {res}")
    print("== INFO ==")
    print(reduce(lambda x, y: x + '\n' + y, info))
