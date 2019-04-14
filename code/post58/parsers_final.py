import pyparsing as P


def literal(s): return P.Literal(s)
def literal_(s): return P.Suppress(P.Literal(s))
def word(s): return P.Word(s)
def word_(s): return P.Suppress(P.Word(s))

def createDoLine(typ, parseResult):
    context = parseResult.asDict()
    return DoLine(typ, **context)

createLet = partial(createDoLine, DoLine.Type.LET)
createSeq = partial(createDoLine, DoLine.Type.SEQ)
createBind = partial(createDoLine, DoLine.Type.BIND)


parseLet = ( literal_("let")
           + word(P.alphanums).setResultsName("var")
           + literal_("=")
           + P.restOfLine.setResultsName("val")
           ).setParseAction(createLet)


parseBind = ( word(P.alphanums).setResultsName("var")
            + literal_("<-")
            + P.restOfLine.setResultsName("val")
            ).setParseAction(createBind)


parseSeq = P.restOfLine.setResultsName("val").setParseAction(createSeq)


parseDoLine = ( parseLet
              | parseBind
              | parseSeq
              )
