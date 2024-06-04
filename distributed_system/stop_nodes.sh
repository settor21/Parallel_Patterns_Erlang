#!/bin/bash

echo "Stopping all active Erlang nodes..."

# Find all erl processes and terminate them
pgrep -f "erl -name node" | while read -r pid; do
    kill -9 $pid
    echo "Stopped node with PID $pid"
done

echo "All active Erlang nodes have been stopped."
