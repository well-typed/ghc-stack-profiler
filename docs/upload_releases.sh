#! /usr/bin/env bash

set -euo pipefail

PROG_NAME=$(basename "$0")

error () {
    echo "$1" >&2
}

synopsis () {
    echo "Synopsis: ${PROG_NAME} [--upload] <version-number>"
    echo ""
    echo "Options:"
    echo "  --upload    Actually upload the packages to Hackage (default: dry-run)"
    echo "  -h, --help  Show this help message"
}

DO_UPLOAD=false
VERSION=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --upload)
            DO_UPLOAD=true
            shift
            ;;
        -h|--help|help|h|-\?|\?)
            synopsis
            exit 0
            ;;
        -*)
            error "Unknown option: $1"
            synopsis
            exit 1
            ;;
        *)
            if [[ -n "$VERSION" ]]; then
                error "Unexpected extra argument: $1"
                synopsis
                exit 1
            fi
            VERSION="$1"
            shift
            ;;
    esac
done

if [[ -z "$VERSION" ]]; then
    error "Release script requires a version number"
    synopsis
    exit 1
fi

ROOT_DIR=$(git rev-parse --show-toplevel)
SOURCE_DIST_DIR=${ROOT_DIR}/sdist/${VERSION}

sdist() {
    cabal sdist --output-directory="${SOURCE_DIST_DIR}" "${1}"
}

upload() {
    local files=()
    for target in "$@"; do
        files+=("${SOURCE_DIST_DIR}/${target}-${VERSION}.tar.gz")
    done

    if [[ "$DO_UPLOAD" == true ]]; then
        echo "Publish packages: "
        printf '  %s\n' "${files[@]}"
        cabal upload --publish "${files[@]}"
    else
        echo "Candidate packages: "
        printf '  %s\n' "${files[@]}"
        cabal upload "${files[@]}"
    fi
}

main () {
    sdist ghc-stack-profiler-core
    sdist ghc-stack-profiler
    sdist ghc-stack-profiler-speedscope

    upload ghc-stack-profiler-core ghc-stack-profiler ghc-stack-profiler-speedscope
}

main
