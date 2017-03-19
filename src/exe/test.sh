#!/bin/sh

tmux new-session -d -s test 'exec ./build-loop.sh'
tmux rename-window 'test'
tmux select-window -t test:0
tmux split-window -h 'exec browser-sync start -c bs-config.js'
tmux split-window -v -t 0 'exec stack build --file-watch'
tmux split-window -v -t 2 'exec elm-reactor -a 0.0.0.0'
tmux -2 attach-session -t test