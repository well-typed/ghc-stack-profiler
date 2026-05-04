#!/bin/sh -e

# Usage: ./scripts/test.sh [CABAL_ARGS] -- [TEST_ARGS]

cabal run ghc-stack-profiler-tests \
	--enable-tests \
	-f+debug \
	"$@"
