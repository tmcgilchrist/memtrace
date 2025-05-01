# Protobuf format 

We provide a CLI tool for converting memtrace files into the protobuf format, so they can be visualised using pprof. 

## Protobuf Conversion CLI tool 

To convert a memtrace .ctf file to the protobuf format, you can run:

```shell
# Generate and zip profile data

$ dune exec bin/convert.exe <trace_file> <output_file>
$ gzip <output_file>

# Start web UI for viewing profile data
$ ~/go/bin/pprof -http localhost:8080 <output_file>
```

As an example, consider the output generated from the example [fib_par file](./../examples/fib_par.ml):

![memtrace-viewer](memtrace-viewer-output.png)

The corresponding protobuf file produces the following output:

![Allocations-by-size](pprof-by-size.png)

you can click the "sample" drop-down menu to view allocations by the number of traces they appear in instead of the amount of memory allocated:

![Allocations-by-number-of-traces](pprof-by-samples.png)

## what is missing 

Pprof uses "mappings" to resolve memory addresses to symbol information. For pure OCaml code, mappings are generally not required, since memtrace already provides symbolic information. However, mappings can be useful if the program interacts with native OCaml runtime components or uses FFI libraries. Unfortunately, memtrace doesn’t expose this mapping information directly, and supporting it would require emitting protobuf data in real time. For now we use a dummy mapping, without which pprof cannot find the main binary name and produces an incomplete graph. 

When viewing a flamegraph in pprof, you can right-click on a location and view it's source code: another feature that uses information from the mappings that are missing in our conversion tool. 


