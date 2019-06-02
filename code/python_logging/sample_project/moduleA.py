
import logging
from util import setup_logger

logger, log_level = setup_logger(__name__)    # (a)

def functionA(i):
    ret = i * 2
    if log_level <= logging.DEBUG:            # (b)
        logger.debug(f"In functionA: argument {i} return value {ret}")
    return ret
