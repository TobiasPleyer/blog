import logging
import time

logger = logging.getLogger(__name__)
LOGLEVEL = logging.ERROR
logger.setLevel(LOGLEVEL)
# These lines are to prevent 1M messages to be logged.
# Instead they will be logged to the void...
handler = logging.NullHandler()
logger.propagate = False
logger.addHandler(handler)

def f(n):
    s = 0
    for i in range(n):
        val = i*i
        if LOGLEVEL <= logging.DEBUG:
            logger.debug(f"Current sum: {s}, adding {val}")
        s += val
    return s

start = time.perf_counter()
f(1_000_000)
stop = time.perf_counter()
print(stop-start)
