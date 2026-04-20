#!/bin/sh -e

# Usage: ./scripts/test.sh [CABAL_ARGS] -- [TEST_ARGS]

cabal build oddball \
	--builddir 'dist-newstyle/test-oddball-+optimise-heavily-+debug' \
	--constraint 'eventlog-socket +optimise-heavily +debug' \
	&& \
	cabal run ghc-stack-profiler-tests \
		--enable-tests \
		-f+debug \
		"$@"
