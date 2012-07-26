#!/bin/bash

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

lsof -i ":$port" | tail -n+2 | awk '{print $2}' | xargs kill "-$signal"
