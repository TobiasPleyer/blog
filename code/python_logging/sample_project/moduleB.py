
import logging
from util import setup_logger

logger, log_level = setup_logger(__name__)

def functionB(i):
    ret = i - 4
    if log_level <= logging.DEBUG:
        logger.debug(f"In functionB: argument {i} return value {ret}")
    return ret
