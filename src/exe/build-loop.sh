#!/bin/bash
declare -i p
trap 'kill -9 "$p"' EXIT
while true; do
  stack exec entangle  & p=$!
  inotifywait -q -e create ../../.stack-work/install/*/*/*/bin/
  sleep 1
  kill -9 "$p"
done
