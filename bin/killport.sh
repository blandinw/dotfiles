#!/bin/bash

set -ex

if [ $# -lt 1 ]; then
    echo "Usage: $(basename $0) <port> <signal>"
    exit
fi

port=$1
shift

if [ ! -z $1 ]; then
    signal=$1
else
    signal="INT"
fi

PLATFORM=$(uname)
if [ $PLATFORM = Darwin ]; then
  HITS=$(lsof -i ":$port" | tail -n+2 | awk '{print $2}')
elif [ $PLATFORM = Linux ]; then
  HITS=$(sudo netstat -nltp | grep ":$port" | awk '{print $7}' | egrep -o '[0-9]+' | uniq)
else
  echo "error: unk platform $PLATFORM"
  exit 1
fi

echo $HITS | xargs kill "-$signal"
