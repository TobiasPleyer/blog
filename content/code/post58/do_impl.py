from functools import partial, reduce


def compose(*flist):
    def helper(*args, **kwargs):
        f0 = None
        if len(flist) == 0:
            pass
        elif len(flist) == 1:
            f0 = flist[0](*args, **kwargs)
        else:
            f0 = flist[0](*args, **kwargs)
            for fn in flist[1:]:
                f0 = fn(f0)
        return f0
    return helper

def sequence(*flist):
    if len(flist) == 0:
        return None
    else:
        f0 = flist[0]
        try:
            f0 = f0()
        except:
            pass
        return compose(*flist[1:])(f0)

def isComment(s: str):
    s = s.strip()
    return s and (not s.startswith("#"))

def toDoLine(s: str):
    return parseDoLine.parseString(s)[0]

def assertSeqAtEnd(ds):
    dl = list(ds)
    if len(dl) > 0 and dl[-1].typ != DoLine.Type.SEQ:
        raise ValueError("The last line of a do must be an expression")
    return dl

def deSugar(ds):
    dl = list(ds)
    if len(dl) > 0:
        return ([d.rewrite() for d in dl[:-1]] + ['(' + dl[-1].val + ')'])
    else:
        return []

def matchParentheses(s: str):
    openedPars = s.count('(')
    closedPars = s.count(')')
    return s + (openedPars - closedPars) * ')'


def do(s: str) -> str:
    """The do-function.
    Takes a string containing the computation written in Haskell style
    do-notation and returns a string with the computation rewritten using the
    bind (>>=) and sequence (>>) functions of the monad type class.

    This function will render the whole computation in one line without
    indentation.

    The returned string should be valid to be passed to Python's `eval`.
    """
    return sequence( s
                   , lambda s: s.split('\n')
                   , partial(filter, isComment)
                   , partial(map, toDoLine)
                   , assertSeqAtEnd
                   , deSugar
                   , lambda ss: ''.join(ss)
                   , matchParentheses
                   )
