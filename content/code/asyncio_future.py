class Future:
    """This class is *almost* compatible with concurrent.futures.Future.

    Differences:

    - result() and exception() do not take a timeout argument and
      raise an exception when the future isn't done yet.

    - Callbacks registered with add_done_callback() are always called
      via the event loop's call_soon_threadsafe().

    - This class is not compatible with the wait() and as_completed()
      methods in the concurrent.futures package.

    (In Python 3.4 or later we may be able to unify the implementations.)
    """
    # ...
    # stuff left out for readability
    # ...
    def __iter__(self):
        if not self.done():
            self._asyncio_future_blocking = True
            yield self  # This tells Task to wait for completion.
        assert self.done(), "yield from wasn't used with future"
        return self.result()  # May raise too.

    if compat.PY35:
        __await__ = __iter__ # make compatible with 'await' expression
