![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/well-typed/ghc-stack-profiler/ci.yml?style=for-the-badge) ![Hackage Version](https://img.shields.io/hackage/v/ghc-stack-profiler?style=for-the-badge) ![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue?style=for-the-badge) ![Stability: Experimental](https://img.shields.io/badge/stability-experimental-yellow?style=for-the-badge)

_A light-weight call-stack profiler for GHC!_

# GHC Stack Profiler

> ⚠️ Warning: This package is experimental. It is versioned according to the PVP. Breaking changes should be expected and no effort will be made to avoid major version bumps until at least version 1.0.0.0.

> ⚠️ Warning: Due to a bug in GHC, copying the call-stack may cause a segfault at runtime in applications built with GHC 9.14.1 and older. If you use GHC Stack Profiler in production, you should build your application with GHC 9.14.2 or newer.

GHC Stack Profiler periodically samples the GHC runtime call-stack and writes these samples to the eventlog.
These eventlogs can be used in two ways:

- [`ghc-stack-profiler-speedscope`](https://hackage.haskell.org/package/ghc-stack-profiler-speedscope) can be used to export the profiles to [speedscope](https://www.speedscope.app/).
- [`eventlog-live-otlp`](https://github.com/well-typed/eventlog-live#readme) can stream the profiles, in real-time, to any observability platform that supports the [OpenTelemetry](https://opentelemetry.io/) protocol, such as [Grafana Cloud](https://grafana.com/).

Unlike GHC's built-in cost-centre stack profiler, GHC Stack Profiler does _not_ require you to rebuild your program with profiling support and has virtually no overhead when it's not running.

## Instrument your application with GHC Stack Profiler

To instrument your application with GHC Stack Profiler, you need to make four changes:

1.  Add `ghc-stack-profiler` to the `build-depends` for your application:

    ```diff
      executable your-application
        ...

        build-depends:
          ...
    +     , ghc-stack-profiler ==0.5.0.0
    ```

    > ⚠️ Warning: If you're using `ghc-stack-profiler-speedscope`, `eventlog-live-otlp`, or any other program that processes the eventlog produced by `ghc-stack-profiler`, it is important that both are built with the same version of `ghc-stack-profiler-core`.

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

    + import GHC.Stack.Profiler (startProfilerFromEnv)

      main :: IO ()
      main = do
    +   startProfilerFromEnv
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

## GHC Stack Profiler and Speedscope

If you have instrumented you application, you can run it with GHC Stack Profiler and export a call-stack profile to the [speedscope](https://www.speedscope.app/) format:

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

## Eventlog Live – Real-Time Call-Stack Profiles

## Eventlog Socket – Sockets and Dynamic Control
