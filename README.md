# `ghc-stack-profiler`

A lightweight profiler that doesn't need to compile your program with profiling information (i.e. `-prof`).

The main idea is to periodically sample the Haskell callstack and use IPE and [stack annotation](https://www.well-typed.com/blog/2025/09/better-haskell-stack-traces/) information in order to understand the source locations which
correspond to the stack frames.

The profiling samples can be exported to [`speedscope.app`](https://www.speedscope.app/) for rendering:

![GHC stack trace obtained with `ghc-stack-profiler` and viewed in speedscope.app](images/ghc-stack-profile-left-heavy.png)

> 🚧 **Under construction** 🚧
>
> This project is currently being developed. The API may change at any moment and is unstable.
>
> **This project can only be used with the [WIP GHC branch `wip/fendor/ghc-stack-profiler`](https://gitlab.haskell.org/ghc/ghc/-/tree/wip/fendor/ghc-stack-profiler).**

## Usage

To profile a program it needs to be compiled and instrumented with the `ghc-stack-profiler` package via:

```haskell
import GHC.Stack.Profiler.Sampler

main :: IO ()
main = withSampleProfilerForMyThread (SampleIntervalMs 10) $ do
    ...
```

This will spawn a profiling thread that will periodically take a snapshot of the current RTS callstack of your program and serialises it to the eventlog.

To emit the eventlog messages by the profiler, you need to run your program with the `-l` RTS flag, for example via:

```bash
./<program> ... +RTS -l -RTS
```

This will write out an eventlog to `<program>.eventlog` which can be transformed for [`speedscope.app`](https://www.speedscope.app/) via the script `ghc-stack-profiler-speedscope`:

```bash
ghc-stack-profiler-speedscope <program>.eventlog
```

The resulting profile `<program>.eventlog.json` can be viewed and further analysed in [`speedscope.app`](https://www.speedscope.app/).

Note, the results are affected by optimisation compilation options, such as `-fno-omit-yields`.

### Example `simple`

The `simple` project is a typical fibonacci implementation. Run it via cabal, assuming a supported GHC version on `$PATH`:

```bash
cabal run exe:simple -- +RTS -l
ghc-stack-profiler-speedscope simple.eventlog
```

Uploading the resulting `simple.eventlog.json` to [`speedscope.app`](https://www.speedscope.app/) shows something similar to:

![Profile of the `simple` program](images/simple-stack-profiler-example1.png)

## Performance

Our initial testing has been instrumenting GHC.

We observe an overhead of around 5% - 10% for compiling `Cabal-syntax`.
The sampling interval was 10 milliseconds.
The overhead depends on the callstack depth, number of alive threads and sampling interval.

More sophistacted benchmarks are expected soon.
