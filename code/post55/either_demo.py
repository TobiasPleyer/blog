class Either():
    Left = 0
    Right = 1
    def __init__(self, kind, value=None):
        self._kind = kind
        self._value = value

    def __or__(self, k):
        if isLeft(self):
            return self
        else:
            return k(self.unwrap())

    def __rshift__(self, k):
        return self.__or__(lambda _: k)

    def unwrap(self):
        return self._value

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

# Now we can write code like in the example above
print(right(1) >> right(2) >> right(3) >> right(4))
print(right(1) >> right(2) >> left("Something bad happened") >> right(4))
print(right(1) | (lambda x: right(2*x)))
print(left("nok") | (lambda x: right(2*x)))
