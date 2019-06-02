import logging                         # (a)

LOG_LEVEL = logging.ERROR              # (b)

logging.basicConfig(level=LOG_LEVEL)   # (c)

LOG_HANDLER = logging.NullHandler()    # (d)

PER_MODULE_LOG_LEVELS = {              # (e)
    'moduleA': logging.ERROR,
    'moduleB': logging.ERROR,
}
