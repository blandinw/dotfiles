#!/usr/bin/env bash

if test $# -lt 1; then
    echo "usage: "$(basename $0)" executable"
    exit 1
fi

SCRIPT_NAME=$1
shift

set -e
set -x

ln -s "$PWD/$SCRIPT_NAME" "$HOME/dotfiles/bin/$(basename $SCRIPT_NAME)"

set +x
