# Revision history for ghc-stack-profiler-speedscope

## 0.4.0.0 -- 2026-07-14

- Switch to `ipedb` version 0.2.0.0 and the new database format.
- Remove `--index` flag from command-line interface; use `ipedb`.
- Add `--table-format` flag to command-line interface to support different database formats.

## 0.3.0.0 -- 2026-06-23

- Support GHC 10.1 [#29](https://github.com/well-typed/ghc-stack-profiler/pull/29)
  - Adds supports for optional source locations in stack annotations

## 0.2.0.0 -- 2026-04-10

- Support ipedb to store InfoProvs [#8](https://github.com/well-typed/ghc-stack-profiler/pull/8)

## 0.1.0.0 -- 2025-12-09

- Export an `.eventlog` file to speedscope.
- First version. Released on an unsuspecting world.
