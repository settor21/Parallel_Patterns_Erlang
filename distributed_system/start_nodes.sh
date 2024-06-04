#!/bin/bash

# Usage: ./start_nodes.sh -n <number_of_nodes> -h <hostname>

while getopts n:h: flag
do
    case "${flag}" in
        n) num_nodes=${OPTARG};;
        h) hostname=${OPTARG};;
    esac
done

if [ -z "$num_nodes" ] || [ -z "$hostname" ]; then
    echo "Usage: ./start_nodes.sh -n <number_of_nodes> -h <hostname>"
    exit 1
fi

echo "Checking if epmd is running..."

# Check if epmd is running
if ! pgrep -x "epmd" > /dev/null
then
    echo "epmd is not running. Starting epmd..."
    epmd -daemon
else
    echo "epmd is already running."
fi

echo "Starting $num_nodes nodes on hostname $hostname..."

for i in $(seq 1 $num_nodes); do
    node_name="node$i@$hostname"
    erl -name $node_name -setcookie mycookie -noshell -detached
    echo "Started $node_name"
done
