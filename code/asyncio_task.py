class Task(futures.Future):
    # ...
    # left out
    # ...
    def _step(self, exc=None):
        assert not self.done(), \
            '_step(): already done: {!r}, {!r}'.format(self, exc)
        if self._must_cancel:
            if not isinstance(exc, futures.CancelledError):
                exc = futures.CancelledError()
            self._must_cancel = False
        coro = self._coro
        self._fut_waiter = None

        self.__class__._current_tasks[self._loop] = self
        # Call either coro.throw(exc) or coro.send(None).
        try:
            if exc is None:
                # We use the `send` method directly, because coroutines
                # don't have `__iter__` and `__next__` methods.
                result = coro.send(None)
            else:
                result = coro.throw(exc)
        except StopIteration as exc:
            self.set_result(exc.value)
        except futures.CancelledError:
            super().cancel()  # I.e., Future.cancel(self).
        except Exception as exc:
            self.set_exception(exc)
        except BaseException as exc:
            self.set_exception(exc)
            raise
        else:
            blocking = getattr(result, '_asyncio_future_blocking', None)
            if blocking is not None:
                # Yielded Future must come from Future.__iter__().
                if result._loop is not self._loop:
                    self._loop.call_soon(
                        self._step,
                        RuntimeError(
                            'Task {!r} got Future {!r} attached to a '
                            'different loop'.format(self, result)))
                elif blocking:
                    if result is self:
                        self._loop.call_soon(
                            self._step,
                            RuntimeError(
                                'Task cannot await on itself: {!r}'.format(
                                    self)))
                    else:
                        result._asyncio_future_blocking = False
                        result.add_done_callback(self._wakeup)
                        self._fut_waiter = result
                        if self._must_cancel:
                            if self._fut_waiter.cancel():
                                self._must_cancel = False
                else:
                    self._loop.call_soon(
                        self._step,
                        RuntimeError(
                            'yield was used instead of yield from '
                            'in task {!r} with {!r}'.format(self, result)))
            elif result is None:
                # Bare yield relinquishes control for one event loop iteration.
                self._loop.call_soon(self._step)
            elif inspect.isgenerator(result):
                # Yielding a generator is just wrong.
                self._loop.call_soon(
                    self._step,
                    RuntimeError(
                        'yield was used instead of yield from for '
                        'generator in task {!r} with {}'.format(
                            self, result)))
            else:
                # Yielding something else is an error.
                self._loop.call_soon(
                    self._step,
                    RuntimeError(
                        'Task got bad yield: {!r}'.format(result)))
        finally:
            self.__class__._current_tasks.pop(self._loop)
            self = None  # Needed to break cycles when an exception occurs.

    def _wakeup(self, future):
        try:
            future.result()
        except Exception as exc:
            # This may also be a cancellation.
            self._step(exc)
        else:
            # Don't pass the value of `future.result()` explicitly,
            # as `Future.__iter__` and `Future.__await__` don't need it.
            # If we call `_step(value, None)` instead of `_step()`,
            # Python eval loop would use `.send(value)` method call,
            # instead of `__next__()`, which is slower for futures
            # that return non-generator iterators from their `__iter__`.
            self._step()
        self = None  # Needed to break cycles when an exception occurs.
