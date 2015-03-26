SafeConcurrentState
===================

The monad known as `SafeConcurrentState s` expresses impure computation which
is to run concurrently where possible, and to share state of type `s`, such
that if an exception is thrown, the latest image of the state is returned.

```Haskell
-- Throws an exception after i microseconds.
blowup :: Int -> SafeConcurrentState s ()
blowup i = embedIO (threadDelay i >> error "Kaboom!")

-- Countdown the state until 0.
countdown :: Int -> SafeConcurrentState Int ()
countdown n = case n of
  0 -> return ()
  n -> set (n-1) >> countdown (n-1)

-- We use the applicative combinator to gain concurrency; blowup i and
-- countdown n will compute in parallel.
-- If n is large enough that the computation is expected to last longer than
-- i microseconds, then we can observe the concurrency: the final state may lie
-- anywhere in [0, n] and cannot be predicted!
example i n = blowup i *> countdown n
```

## Why bother?

The motivating use case is a computation which will allocate an arbitrary and
statically unknown number of resources on-demand, exploit them concurrently,
hold them open until all computation is finished, and clean up afterwards
depending upon whether an exception was raised. In case of exception, the
opened resources should be rolled back; in case of no exception, they should
be committed; and in any case, they must all be closed.

This can be expressed rather well with `SafeConcurrentState` and
`DependentMap`. A resource can be acquired freely through `embedIO`, and
atomically placed in the map with `modify`, assuming a suitable key into
the map (perhaps a filename or url). This is the scheme adopted by
[Manifest](https://github.com/avieth/Manifest) to give worry-free read/write
access to heterogeneous resources.
