![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/well-typed/ghc-stack-profiler/ci.yml?style=for-the-badge) ![Hackage Version](https://img.shields.io/hackage/v/ghc-stack-profiler?style=for-the-badge) ![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue?style=for-the-badge) ![Stability: Experimental](https://img.shields.io/badge/stability-experimental-yellow?style=for-the-badge)

_A light-weight call-stack profiler for GHC!_

# GHC Stack Profiler

> ⚠️ **Warning:** This package is experimental. It is versioned according to the PVP. Breaking changes should be expected and no effort will be made to avoid major version bumps until at least version 1.0.0.0.

> ⚠️ **Warning:** Due to a bug in GHC, copying the call-stack may cause a segfault at runtime in applications built with GHC 9.14.1 and older. If you use GHC Stack Profiler in production, you should build your application with GHC 9.14.2 or newer.

GHC Stack Profiler periodically samples the GHC runtime call-stack and writes these samples to the eventlog.
These eventlogs can be used in two ways:

- [`ghc-stack-profiler-speedscope`](https://hackage.haskell.org/package/ghc-stack-profiler-speedscope) can be used to export call-stack profiles to [speedscope](https://www.speedscope.app/).
- [`eventlog-live-otlp`](https://github.com/well-typed/eventlog-live#readme) can stream call-stack profiles, in real-time, to any observability platform that supports the [OpenTelemetry](https://opentelemetry.io/) protocol, such as [Grafana Cloud](https://grafana.com/).

Unlike GHC's built-in cost-centre stack profiler, GHC Stack Profiler does _not_ require you to rebuild your program with profiling support and has virtually no overhead when it's not running. (See [Benchmarks](#benchmarks).)

The following is a screenshot of [speedscope](https://www.speedscope.app/) that shows a call-stack profile of Agda version 2.8.0.1 checking the standard library created using GHC Stack Profiler:

![A call-stack profile of Agda 2.8.0.1 checking the standard library.](assets/agda-with-ghc-stack-profiler-2026-09-11.png)

## Table of Contents

- [Getting Started](#getting-started)
  - [Instrument your application with GHC Stack Profiler](#instrument-your-application-with-ghc-stack-profiler)
  - [GHC Stack Profiler with Speedscope](#ghc-stack-profiler-with-speedscope)
  - [GHC Stack Profiler with Eventlog Live – Real-Time Call-Stack Profiles](#ghc-stack-profiler-with-eventlog-live-real-time-call-stack-profiles)
  - [GHC Stack Profiler with Eventlog Socket – Dynamic Control](#ghc-stack-profiler-with-eventlog-socket--dynamic-control)
- [Benchmarks](#benchmarks)
  - [Benchmark: Agda 2.8.0.1 checking the standard library](#benchmark-agda-2801-checking-the-standard-library)
  - [Benchmark: GHC 10.1 loading `Cabal-syntax`](#benchmark-ghc-101-loading-cabal-syntax)

## Getting Started

Let's get GHC Stack Profiler working with your application, which we'll conveniently call `your-application`.

In the first two sections, we'll instrument your application with GHC Stack Profiler and visualise the call-stack profile of a completed run using [speedscope](https://www.speedscope.app/). In the last two sections, we'll add [Eventlog Live](https://github.com/well-typed/eventlog-live) and [Eventlog Socket](https://github.com/well-typed/eventlog-socket) to visualise your application's call-stack profiles in real-time and control GHC Stack Profiler from your observability dashboard.

### Instrument your application with GHC Stack Profiler

To instrument your application with GHC Stack Profiler, you need to make four changes:

1.  Add `ghc-stack-profiler` to the `build-depends` for your application:

    ```diff
      executable your-application
        ...

        build-depends:
          ...
    +     , ghc-stack-profiler ==0.5.0.0
    ```

    > ⚠️ **Warning:** If you're using `ghc-stack-profiler-speedscope`, `eventlog-live-otlp`, or any other program that processes the eventlog produced by `ghc-stack-profiler`, it is important that both are built with the same version of `ghc-stack-profiler-core`.

2.  Build your application with support for RTS options and the threaded runtime.

    Add the following to the `executable` section of your application:

    ```diff
      executable your-application
        ...

    +   ghc-options: -rtsopts
    +   ghc-options: -threaded
    ```

    The [`-rtsopts`](https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#ghc-flag-rtsopts-none-some-all-ignore-ignoreAll) flag enables the RTS options for your application. This allows us to enable the eventlog at runtime and enable various kinds of profiling. Setting this option may pose a security risk. If this is a concern, you can set all the required RTS options at compile time using [`-with-rtsopts`](https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#ghc-flag-with-rtsopts-opts).

    The [`-threaded`](https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#ghc-flag-threaded) flag builds your application with the threaded RTS.

3.  Instrument your main function:

    ```diff
      module Main where
      ...

    + import GHC.Stack.Profiler (withProfilerFromEnv)

      main :: IO ()
      main =
    +   withProfilerFromEnv $
          ...
    ```

    > ℹ️ **Tip:**
    > If you prefer not to configure your program from the environment, the [GHC.Stack.Profiler](https://hackage.haskell.org/package/ghc-stack-profiler/docs/GHC-Stack-Profiler.html) exposes a variety of function that instrument your program.

    > ℹ️ **Tip:**
    > You can use the [`annotateStackIO`](https://hackage-content.haskell.org/package/ghc-stack-annotations/docs/GHC-Stack-Annotation.html#v:annotateStackIO) functions from [`ghc-stack-annotations`](https://hackage-content.haskell.org/package/ghc-stack-annotations) to push annotation frames onto the call-stack at runtime.
    > These annotation frames are visible in call-stack profiles captured by GHC Stack Profiler.
    > See [Better Haskell stack traces via user annotations](https://www.well-typed.com/blog/2025/09/better-haskell-stack-traces/).

4.  Build your application and its dependencies with info table maps.

    Let's do this in two steps:
    1.  To build your application and its dependencies with info table maps, you must ensure that they are built with the `-finfo-table-map` and `-fdistinct-constructor-tables` GHC options.

        The easiest way to do this is to add the following to your `cabal.project` file:

        ```
        package *
          ghc-options:
            -finfo-table-map
            -fdistinct-constructor-tables
        ```

        There is currently no easy way to pass GHC options to all packages when using `cabal install`.
        As a workaround, you can add a `cabal.project` file to a source distribution and install from there.

        If you run GHC Stack Profiler with your application built this way, you will get detailed information for all the symbols defined in your application and most symbols defined in your dependencies.
        However, you will see some unresolved info tables, which will show as numbers, e.g., `0x100000000`.
        These are symbols that are either built into GHC or defined in the [_boot libraries_](https://gitlab.haskell.org/ghc/ghc/-/wikis/working-conventions/boot-libraries) that came with GHC, such as `base`.
        The boot packages are _never_ rebuilt by Cabal and are unaffected by the `package *` stanza.

    2.  To build the GHC and the boot libaries with info table maps, you must build GHC with the `+ipe` flavour.

        The easiest way to do this is using `ghcup`. Some variant of the following command may work for you:

        ```sh
        ghcup compile ghc -j0 -b 9.10.3 -v 9.10.3 -f perf+ipe -o '%v-ipe' --
        ```

        You may need to pass the appropriate configure flags for your platform.
        See [Building and Porting GHC](https://gitlab.haskell.org/ghc/ghc/-/wikis/building#building-and-porting-ghc).

    Once you have a version of GHC built with the `+ipe` flavour and rebuilt application, you should no longer see unresolved info tables.

### GHC Stack Profiler with Speedscope

If you have instrumented your application, you can run it with GHC Stack Profiler and export a call-stack profile to the [speedscope](https://www.speedscope.app/) format:

```sh
# Configure GHC Stack Profiler
export GHC_STACK_PROFILER="ON" # or any other non-empty value
export GHC_STACK_PROFILER_SAMPLE_INTERVAL="10" # milliseconds

# Start your application
./your-application               \
    +RTS                         \
    -l                           \
    -olyour-application.eventlog \
    -RTS

# Export the eventlog to speedscope
ghc-stack-profiler-speedscope \
    your-application.eventlog \
    your-application.json
```

To view your call-stack profile, open [speedscope](https://www.speedscope.app/) and load `your-application.json`.

The `ghc-stack-profiler-speedscope` program has several options that control the speedscope profile:

- You can restrict your profile to the section between start and end markers (using `--start`/`--end`), which you can emit from your application using [`traceMarkerIO`](https://hackage-content.haskell.org/package/base/docs/Debug-Trace.html#v:traceMarkerIO).

  Let's say your application has to do some setup and cleanup, but you're only interested in profiling The Big Chore. If you instrument your application as follows and call `ghc-stack-profiler-speedscope` with `--start=START` and `--end=END`, your profile will only include samples from The Big Chore:

  ```hs
  main = do
    doSomeSetup           -- Not included in profile.
    traceMarkerIO "START" -- Start marker.
    doTheBigChore         -- Included in profile.
    traceMarkerIO "END"   -- End marker.
    doSomeCleanup         -- Not included in profile.
  ```

  > ℹ️ **Tip:** This applies a post-hoc filter, which means that GHC Stack Profiling will still be sampling during the setup and cleanup. If you want to sample _only_ during The Big Chore, you can use either [`startProfiling`](https://hackage-content.haskell.org/package/ghc-stack-profiler/docs/GHC-Stack-Profiler.html#v:startProfiling)/[`stopProfiling`](https://hackage-content.haskell.org/package/ghc-stack-profiler/docs/GHC-Stack-Profiler.html#v:stopProfiling) or the [Eventlog Socket control commands](#eventlog-socket--sockets-and-dynamic-control).

- You can aggregate your application's profiles by thread or capability:
  - `--per-thread`: Group the profiles by thread. (Default.)
  - `--per-capability`: Group the profiles by capability.
  - `--no-aggregation`: Do not aggregate the profiles.

### GHC Stack Profiler with Eventlog Live – Real-Time Call-Stack Profiles

If you have instrumented your application, you can run it with GHC Stack Profiler and Eventlog Live and stream call-stack profiles, in real-time, to any observability platform that supports the [OpenTelemetry](https://opentelemetry.io/) protocol, such as [Grafana Cloud](https://grafana.com/). For detailed instructions, see the section [Eventlog Live with GHC Stack Profiler](https://github.com/well-typed/eventlog-live#eventlog-live-with-ghc-stack-profiler) in the README for Eventlog Live.

The following shows real-time call-stack profiles visualised in Grafana:

![A screen recording of the Grafana Call-Stack Profiles dashboard for the jumpy-jump example program.](assets/jumpy-jump-with-ghc-stack-profiler-2026-07-31.gif)

### GHC Stack Profiler with Eventlog Socket – Dynamic Control

When compiled with the `+control` feature flag, GHC Stack Profiler has built-in support for Eventlog Socket's control commands. This lets you dynamically start and stop profiling by writing the command to the eventlog socket. For a detailed explanation of control commands, see the section [Control Commands](https://github.com/well-typed/eventlog-socket#control-commands) in the README for Eventlog Socket.

If you are using Eventlog Live, you can use its control server to send the GHC Stack Profiler control commands via HTTP. This lets you control profiling from your observability dashboard, e.g., using the Start/Stop buttons at the bottom of the Grafana dashboard in [the previous section](#eventlog-live-real-time-call-stack-profiles). For detailed instructions, see the section [Eventlog Live with Eventlog Socket](https://github.com/well-typed/eventlog-live/tree/main/eventlog-live#eventlog-live-with-eventlog-socket) in the README for Eventlog Live.

## Benchmarks

This section discusses our benchmarks that measure the overhead of instrumenting and profiling your application with GHC Stack Profiler and GHC's built-in cost-centre profiler. Our conclusions:

- Instrumenting your application with GHC Stack Profiler has no measurable overhead.

  Running GHC Stack Profiler has about 2% overhead with no significant difference between the measured sample intervals.

- Instrumenting your application with the cost-centre profiler has around 50% overhead with no cost centres and around 100% overhead with late cost centres.

  Running the cost-centre profiler has an additional 2% overhead with no significant difference between the measured sample intervals.

### Benchmark: Agda 2.8.0.1 checking the standard library

The benchmark measures Agda 2.8.0.1 checking the standard library:

```sh
# from within std-lib/ in the Agda repository
agda --build-library +RTS -N1
```

There are three classes of benchmarks:

- The `baseline` benchmark uses Agda with no modifications.

- The `ghc-stack-profiler` benchmarks use Agda instrumented with `ghc-stack-profiler`.

  (For details, see [Instrument your application with GHC Stack Profiler](#instrument-your-application-with-ghc-stack-profiler).)

- The `profiling` benchmarks use Agda instrumented with cost-centre profiling, using the following `cabal.project`, where the value of `profiling-detail` taken from the benchmark name:

  ```hs
  profiling: True

  package *
    profiling-detail: none -- or late
  ```

  (The `-p` RTS option was used to the profiler and the `-V` RTS option was used to set the sample interval.)

The results are normalised as a percentage of the `baseline` benchmark which took, on average, 3 minutes and 55 seconds on an otherwise idle machine. The timings are the result of, on average, 10 runs excluding warmup.

![A bar chart that shows the relative timing of the various benchmarks compared to the baseline. For GHC Stack Profiler, the "instrumented only" benchmark has no measurable overhead, and both benchmarks that sample the call-stack have about 2% overhead. For cost-centre profiling, the "instrumented only" benchmark that introduces no cost centres has 54% overhead, the "instrumented only" benchmark that introduces late cost centres has 96% overhead, and both benchmarks that sample the cost-centre stacks have another 2% overhead on top of that.](assets/benchmark-agda-2.8.0.1-checking-agda-stdlib.png)

### Benchmark: GHC 10.1 loading `Cabal-syntax`

The benchmark measures GHC 10.1 (9a442c9383) loading `Cabal-syntax` in interactive mode, using the GHC command obtained from `hie-bios`:

```sh
# from within libraries/Cabal/ in the GHC repository
hie-bios -v debug Cabal-syntax/src/Distribution/CabalSpecVersion.hs
```

There two classes of benchmarks:

- The `ghc-stack-profiler` benchmarks use GHC instrumented with `ghc-stack-profiler`.

  (For details, see [Instrument your application with GHC Stack Profiler](#instrument-your-application-with-ghc-stack-profiler).)

- The `profiling` benchmarks use GHC instrumented with cost-centre profiling, using the `default` build flavour with the `profiled_ghc` [flavour transformer](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/flavours.md), which builds GHC and its dependencies with profiling and adds late cost centres.

  (The `-pj` RTS option was used to enable the profiler and the `-V` RTS option was used to set the sample interval.)

The results are normalised as a percentage of the `ghc-stack-profiler (instrumented only)` benchmark which took, on average, 7 seconds. The timings are the result of, on average, 3 runs without warmup on a noisy machine. The measurement that shows that sampling at a 10ms interval is slower than a 1ms interval is likely due to this noise. We did not included a `baseline` benchmark with an uninstrumented GHC, as there was no measurable overhead in the previous benchmark. We also did not include a `profiling (instrumented only, profiling-detail: none)` benchmark, as that would have required adding a new flavour transformer to GHC's build system.

![A bar chart that shows the relative timing of the various benchmarks compared to the "instrumented only" benchmark for GHC Stack Profiler. For GHC Stack Profiler, both benchmarks that sample the call-stack have about 7-8% overhead. For cost-centre profiling, the "instrumented only" benchmark has about 128% overhead, and both benchmarks that sample the cost-centre stacks have another 3-8% overhead.](assets/benchmark-ghc-10.1-9a442c9383-loading-Cabal-syntax.png)
