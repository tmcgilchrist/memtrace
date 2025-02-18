#!/bin/bash

# Check if at least one argument is provided
if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <name> [t]"
    exit 1
fi

# Set the MEMTRACE variable and run the command
MEMTRACE="$1" dune exec ./examples/testproto.exe

# If the second argument is "t", run the pprof command
if [ "$2" == "t" ]; then
    gzip "$1".pb
    GODEBUG=pprofdebug=1 pprof -http=":" ./_build/default/examples/testproto.exe "$1".pb.gz 
fi
