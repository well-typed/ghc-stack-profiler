# Releasing

* [ ]: Check tests pass and CI is updated
* [ ]: Bump package version numbers compliant with [PVP](https://pvp.haskell.org/)
* [ ]: Update ChangeLogs
* [ ]: Create branch `release/<version>` and open Pr to check CI is green
* [ ]: Upload packages to hackage:
  * Currently, we assume releases of the `ghc-stack-profiler*` family are in lockstep.
  * This release scripts automates the upload 
    * `bash ./docs/upload_releases.sh <version>`
* [ ]: `git tag <version> && git push --tags`
