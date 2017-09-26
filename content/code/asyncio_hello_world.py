# asyncio_hello_world.py
import asyncio

async def slow_operation(future):
    await asyncio.sleep(1)
    future.set_result('Hello World!')

loop = asyncio.get_event_loop()
future = asyncio.Future()
loop.run_until_complete(slow_operation(future))
print(future.result())
loop.close()
