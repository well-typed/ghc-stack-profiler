# Revision history for ghc-stack-profiler-core

## 0.4.0.0 -- 2026-07-14

Major version number changed to match `ghc-stack-profiler-speedscope`.

No changes.

## 0.3.0.0 -- 2026-06-23

- Support GHC 9.4, 9.6 and 9.8 for ghc-stack-profiler-core [#30](https://github.com/well-typed/ghc-stack-profiler/pull/30)
- Support GHC 10.1 [#29](https://github.com/well-typed/ghc-stack-profiler/pull/29)
  - Adds supports for optional source locations in stack annotations
- Fix rendering of the protocol specification with haddock [#28](https://github.com/well-typed/ghc-stack-profiler/pull/28)

## 0.2.0.0 -- 2026-04-10

- Add support for eventlog-socket lifecycle hooks [#19](https://github.com/well-typed/ghc-stack-profiler/pull/19)
- Never crash on decoding errors [#12](https://github.com/well-typed/ghc-stack-profiler/pull/12)

## 0.1.0.0 -- 2025-12-09

- Add types for encoding RTS callstacks to bytes that can be sent to the eventlog.
- First version. Released on an unsuspecting world.
