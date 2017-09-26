# asyncio_hello_world_naive.py
import asyncio

async def slow_operation(future):
    await asyncio.sleep(1)
    future.set_result('Hello World!')

future = asyncio.Future()
coro = slow_operation(future)
ret = coro.send(None)
print("Return from coro.send: {}".format(ret))
try:
    coro.send(None)
except StopIteration as e:
    pass
print(future.result())
