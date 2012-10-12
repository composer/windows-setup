#!/bin/sh

# This file must be saved with Unix line endings, or cygwin will choke

DIR=`dirname "$0"`;
DIRECTORY=$(cd "${DIR}" && pwd)

if command -v 'cygpath' >/dev/null 2>&1; then
  DIRECTORY=`cygpath -m $DIRECTORY`;
fi

PHAR="$(echo $DIRECTORY | sed 's/ /\ /g')/composer.phar"
php "${PHAR}" $*
