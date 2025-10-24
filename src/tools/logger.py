import logging

def get_logger(name: str) -> logging.Logger:
    logger = logging.getLogger(name)
    if not logger.hasHandlers():
        logging.basicConfig(level=logging.DEBUG, format='[%(levelname)s] %(message)s')
    return logger
