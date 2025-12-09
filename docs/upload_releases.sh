#! /usr/bin/env bash

set -euxo pipefail

error () {
    echo ${$1} >&2
}

synopsis () {
    echo "Synopsis: ./docs/upload_releases.sh <version-number>"
}


if [[ $# != 1 ]]; then
    error "Release script required 1 argument"
    synopsis
    exit 1
fi

if [[ "${1}" == '-h' || "${1}" == 'help' || "${1}" == '--help' || "${1}" == 'h' || "${1}" == '-?'|| "${1}" == '?' ]]; then
    synopsis
    exit 0
fi

VERSION=${1}
CWD=$(pwd)
ROOT_DIR=$(git rev-parse --show-toplevel)
SOURCE_DIST_DIR=${ROOT_DIR}/sdist/${VERSION}

sdist() {
    cabal sdist --output-directory=${SOURCE_DIST_DIR} ${1}
}

upload() {
    cabal upload ${SOURCE_DIST_DIR}/${1}-${VERSION}.tar.gz
}

main () {
    sdist ghc-stack-profiler-core
    sdist ghc-stack-profiler
    sdist ghc-stack-profiler-speedscope

    upload ghc-stack-profiler-core
    upload ghc-stack-profiler
    upload ghc-stack-profiler-speedscope
}

main
