from typing import Any, Callable, Tuple


class OverloadException(Exception):
    pass


class OverloadedFunction:
    def __init__(self, name):
        self._name = name
        self._overloads = {}

    def add_overload(self, signature: Tuple[Any, ...], func: Callable[[Any], Any]):
        if signature in self._overloads:
            raise OverloadException(f"Overloaded function '{self._name}' has already been overloaded with signature {signature}")
        self._overloads[signature] = func

    def __call__(self, *args):
        signature = tuple(map(type, args))
        func = self._overloads.get(signature, None)
        if func is None:
            raise OverloadException(f"Overloaded function '{self._name}' does not provide an overload for the signature {signature}")
        else:
            return func(*args)


def overload(overloaded_funcname: str):
    def decorator(func: Callable[[Any], Any])-> Callable[[Any], Any]:
        if overloaded_funcname not in globals():
            globals()[overloaded_funcname] = OverloadedFunction(overloaded_funcname)
        overloaded_func = globals()[overloaded_funcname]
        if not isinstance(overloaded_func, OverloadedFunction):
            raise OverloadException("Given function name does not correspond to an overloaded function")
        signature = func.__annotations__
        if 'return' in signature:
            signature.pop('return')
        signature = tuple(signature.values())
        overloaded_func.add_overload(signature, func)
        return func
    return decorator


if __name__ == '__main__':

    @overload('test')
    def test1(x: int):
        return x + 42

    @overload('test')
    def test2(x: float):
        return x * 0.5

    @overload('test')
    def test3(x: str):
        return x + "!!!"

    @overload('test')
    def test4(n: int, x: str):
        return x + "!"*n

    print(test(2))
    print(test(2.0))
    print(test("Hi"))
    print(test(5, "Hi"))

    customer_hash = { "John Doe": {"first": "John", "last": "Doe", "age": 42} }
    customer_array = 123456*[{}] + [{"first": "John", "last": "Doe", "age": 42}]

    @overload('lookup')
    def lookupByString(s: str):
        return customer_hash[s]

    @overload('lookup')
    def lookupByIndex(i: int):
        return customer_array[i]

    print(lookup("John Doe"))
    print(lookup(123456))
