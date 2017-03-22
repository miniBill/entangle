#!/bin/bash

declare -i p
trap 'kill "$p"; sleep 1; kill -9 "$p"' EXIT
while true
do
    stack exec entangle & p=$!
    inotifywait -q -e create ../../.stack-work/install/*/*/*/bin/
    sleep 1
    kill "$p"
    sleep 1
    kill -9 "$p" 2> /dev/null
done