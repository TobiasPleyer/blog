from datetime import datetime
from pyparsing import *


filename = "yesterday.log"
def literal_(s): return Suppress(Literal(s))


class IP:
    def __init__(self, ip1, ip2, ip3, ip4):
        self.ip = [ip1, ip2, ip3, ip4]

    def __str__(self):
        return ".".join(self.ip)

    def __repr__(self):
        return "IP(" + str(self) + ")"


class Product:
    P_NONE = 0
    P_MOUSE = 1
    P_KEYBOARD = 2
    P_MONITOR = 3
    P_SPEAKERS = 4

    def __init__(self, product):
        self.product = product
        if product == "mouse":
            self.snr = self.P_MOUSE
        elif product == "mouse":
            self.snr = self.P_MOUSE
        elif product == "mouse":
            self.snr = self.P_MOUSE
        elif product == "mouse":
            self.snr = self.P_MOUSE
        else:
            self.snr = self.P_NONE

    def __str__(self):
        return self.product

    def __repr__(self):
        return "Product(" + str(self) + ")"


def toIP(parseResult):
    ip = parseResult.asDict()["IP"]
    return IP(*ip)


def toProduct(parseResult):
    product = parseResult.asDict()["Product"]
    return Product(product)


def toDateTime(parseResult):
    dt = map(int, parseResult.asDict()["DateTime"])
    return datetime(*dt)


parseIP = ( Word(nums)
          + literal_(".")
          + Word(nums)
          + literal_(".")
          + Word(nums)
          + literal_(".")
          + Word(nums)
          ).setResultsName("IP").setParseAction(toIP)


parseTime = ( Word(nums, exact=4)
            + literal_("-")
            + Word(nums, exact=2)
            + literal_("-")
            + Word(nums, exact=2)
            + Word(nums, exact=2)
            + literal_(":")
            + Word(nums, exact=2)
            + literal_(":")
            + Word(nums, exact=2)
            ).setResultsName("DateTime").setParseAction(toDateTime)


parseProduct = ( Literal("mouse")
               | Literal("keyboard")
               | Literal("monitor")
               | Literal("speakers")
               ).setResultsName("Product").setParseAction(toProduct)


parseLogEntry = parseTime + parseIP + parseProduct


parseLog = ZeroOrMore(Group(parseLogEntry))


if __name__ == '__main__':
    parse_results = parseLog.parseFile(filename)
    for parse_result in parse_results:
        print(parse_result)
