.PHONY: assets
assets: \
	../assets/benchmark-agda-2.8.0.1-checking-agda-stdlib.png \
	../assets/benchmark-ghc-10.1-9a442c9383-loading-Cabal-syntax.png

../assets/benchmark-agda-2.8.0.1-checking-agda-stdlib.png: \
		plot_benchmark_comparison.py \
		benchmark-agda-2.8.0.1-checking-agda-stdlib.json
	pipx run plot_benchmark_comparison.py \
		--benchmark-names='' \
		--baseline='baseline' \
		--title='Agda 2.8.0.1 checking agda-stdlib' \
		benchmark-agda-2.8.0.1-checking-agda-stdlib.json \
		--output=../assets/benchmark-agda-2.8.0.1-checking-agda-stdlib.png

../assets/benchmark-ghc-10.1-9a442c9383-loading-Cabal-syntax.png: \
		plot_benchmark_comparison.py \
		benchmark-ghc-10.1-9a442c9383-loading-Cabal-syntax.json
	pipx run plot_benchmark_comparison.py \
		--benchmark-names='' \
		--baseline='ghc-stack-profiler (instrumented only)' \
		--title='GHC 10.1 loading Cabal-syntax' \
		benchmark-ghc-10.1-9a442c9383-loading-Cabal-syntax.json \
		--output=../assets/benchmark-ghc-10.1-9a442c9383-loading-Cabal-syntax.png
