# Overview

## Publishing a release

The packages `ghc-stack-profiler`, `ghc-stack-profiler-core`, and `ghc-stack-profiler-speedscope` are versioned and released in lock-step.

1.  Ensure that the current HEAD is ready to be published:
    - The version number is updated in all relevant places.
      This includes at least:
      - `ghc-stack-profiler/ghc-stack-profiler.cabal`
      - `ghc-stack-profiler/CHANGELOG.md`
      - `ghc-stack-profiler-core/ghc-stack-profiler-core.cabal`
      - `ghc-stack-profiler-core/CHANGELOG.md`
      - `ghc-stack-profiler-speedscope/ghc-stack-profiler-speedscope.cabal`
      - `ghc-stack-profiler-speedscope/CHANGELOG.md`
    - The Haddock documentation builds without warnings and renders without errors.
    - The tests pass on CI.

2.  Create a Git tag of the form `${VERSION}`, e.g., `0.5.0.0`:

    ```sh
    git tag ${VERSION}
    ```

    > ⚠️ **Warning:** Replace `${VERSION}` with the new version.

3.  Publish the Git tag:

    ```sh
    git push --tags
    ```

4.  The README is included in the source distribution for `ghc-stack-profiler`.

    To ensure that the images render on Hackage, replace the links to the local assets with permanent links to the Git tag you created in step (2).

    ```diff
    - ![...](assets/benchmark-agda-2.8.0.1-checking-agda-stdlib.png)
    + ![...](https://github.com/well-typed/ghc-stack-profiler/blob/0.5.0.0/assets/benchmark-agda-2.8.0.1-checking-agda-stdlib.png?raw=true)
    ```

    > ⚠️ **Warning:** Do not forget the `?raw=true` at the end.

    > ⚠️ **Warning:** Do not commit these changes.

5.  Build the source distribution.

    ```sh
    cabal sdist ghc-stack-profiler
    ```

    > ℹ️ **Tip:** This writes the source distributions to `dist-newstyle/sdist/`.

6.  Upload the source distribution to Hackage _as a package candidate_
    - Navigate to <https://hackage.haskell.org/packages/candidates/upload>.
    - Upload the source distribution built in the previous step.

7.  Ensure that the package candidate page has no errors.

8.  Ensure that the `CHANGELOG.md` has no errors.

9.  Publish the candidate package.
    - On the package candidate package, click on _"[Publish]"_ and confirm.
