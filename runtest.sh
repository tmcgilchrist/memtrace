#!/bin/bash

# Function to print usage
usage() {
    echo "Usage: $0 <executable> <tracefile> [t] [--args <args>]"
    exit 1
}

# Check if at least two arguments are provided
if [ "$#" -lt 2 ]; then
    usage
fi

# Parse arguments
EXECUTABLE=$1
TRACEFILE=$2
RUN_VIEWERS=false
EXTRA_ARGS=""

shift 2  # Move past the first two arguments

while [[ "$#" -gt 0 ]]; do
    case $1 in
        t)
            RUN_VIEWERS=true
            ;;
        --args)
            shift
            # Collect all remaining arguments into EXTRA_ARGS
            EXTRA_ARGS=""
            while [[ "$#" -gt 0 && "$1" != "t" ]]; do
                EXTRA_ARGS="$EXTRA_ARGS $1"
                shift
            done
            # If we stopped because we found 't', we need to process it
            if [[ "$#" -gt 0 && "$1" == "t" ]]; then
                RUN_VIEWERS=true
                shift
            fi
            ;;
        *)
            usage
            ;;
    esac
    shift
done

# Set MEMTRACE and run the command with optional arguments
echo "Running: MEMTRACE=\"$TRACEFILE\" dune exec ./examples/\"$EXECUTABLE\".exe -- $EXTRA_ARGS"
MEMTRACE="$TRACEFILE".ctf dune exec ./examples/"$EXECUTABLE".exe -- $EXTRA_ARGS

echo "Coverting to protobuf..."
dune exec bin/convert.exe "$TRACEFILE".ctf "$TRACEFILE".pb

# Run viewers in the current terminal if "t" was passed
if [ "$RUN_VIEWERS" = true ]; then
    echo "Running viewers..."
    gzip -f "$TRACEFILE".pb  # Added -f to force overwrite if needed
    
    # First switch OPAM environment
    echo "Switching OPAM environment..."
    opam switch temp
    eval $(opam env)
    
    # Then run the viewers
    echo "Starting pprof..."
    GODEBUG=pprofdebug=1 pprof -http=":" "$TRACEFILE".pb.gz &
    
    echo "Starting memtrace-viewer..."
    memtrace-viewer "$TRACEFILE".ctf
fi