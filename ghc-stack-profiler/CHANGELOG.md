# Revision history for ghc-stack-profiler

## 0.2.0.0 -- 2026-04-10

* Backport stack decoding segmentation fault fix from GHC HEAD [#20](https://github.com/well-typed/ghc-stack-profiler/pull/20)
* Add support for eventlog-socket lifecycle hooks [#19](https://github.com/well-typed/ghc-stack-profiler/pull/19)
* Never crash on decoding errors [#12](https://github.com/well-typed/ghc-stack-profiler/pull/12)
* Use aync for proper thread management [#10](https://github.com/well-typed/ghc-stack-profiler/pull/10)
* Implement support for eventlog-socket and custom commands [#7](https://github.com/well-typed/ghc-stack-profiler/pull/7)

## 0.1.0.0 -- 2025-12-09

* Adds API for sampling the RTS callstack and writing the results to the eventlog.
* First version. Released on an unsuspecting world.
