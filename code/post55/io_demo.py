class IO():
    def __init__(self, value):
        self._value = value

    @staticmethod
    def mreturn(value):
        return IO(lambda: value)

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

io1 = IO.mreturn(1)
def io2(x):
    print(f"Doubling {x} -> {2*x}")
    return IO.mreturn(None)

composite_io = io1 | io2
composite_io.run()
