#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc
cabal upload -d $dir/*-docs.tar.gz $@
