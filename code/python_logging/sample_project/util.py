import logging
import logging.handlers
import log_config

def setup_logger(name):
    logger = logging.getLogger(name)                        # (a)
    logger.propagate = False                                # (b)
    log_level = log_config.LOG_LEVEL                        # (c)

    if name in log_config.PER_MODULE_LOG_LEVELS:            # (d)
        log_level = log_config.PER_MODULE_LOG_LEVELS[name]
        logger.setLevel(log_level)

    fh = logging.handlers.MemoryHandler(2048, target=log_config.LOG_HANDLER)
    fh.setLevel(log_level)
    logger.addHandler(fh)
    return (logger, log_level)
