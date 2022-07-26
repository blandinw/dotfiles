#!/usr/bin/env bash
# Run kmonad once per keyboard, using the same config file

set -euo pipefail

usage() {
  echo "$(basename $0) CONFIG_FILE"
  exit 1
}

if [ $# -lt 1 ]; then
  usage
fi

CONFIG_FILE="$1"
shift
KEYBOARD="$1"
shift

if [ "$(uname -s)" == Darwin ]; then
  IDFUNC=iokit-name
elif [ "$(uname -s)" == Linux ]; then
  IDFUNC=device-file
fi

TMP_FILE="$(mktemp -t $(basename $CONFIG_FILE))"

< "$CONFIG_FILE" > "$TMP_FILE" perl -pe "
  s
  {input\s+\([\" a-zA-Z0-9_-]+\)}
  {input ($IDFUNC \"$KEYBOARD\")}
"

sudo kmonad "$TMP_FILE"
