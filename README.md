# TeSSLa Compiler and Interpreter

## Download

You can get the latest version of the TeSSLa compiler and interpreter from the [Gitlab Artifact Browser](https://gitlab.isp.uni-luebeck.de/tessla/tessla/builds/artifacts/TesslaCoreProposal/browse/target/scala-2.12?job=deploy).

## Docker Image

TeSSLa is also available as part of the [TeSSLa docker image](https://gitlab.isp.uni-luebeck.de/tessla/tessla-docker), which is especially useful if you want to use TeSSLa on instrumented C code.

## Usage

```
Usage: tessla OPTION* tessla-file trace-file?
Evaluate the given Tessla specification on the input streams provided by the given trace file.

Options:
  tessla-file       The file containing the Tessla specification
  trace-file        The file containing the trace data used as input for the
                    specification. If this is not provided, input is read from
                    stdin
      --            Treat all subsequent arguments as positional even if they
                    start with a dash
      --list-out-streams
                    Print a list of the output streams defined in the given
                    tessla spec and then exit
      --no-diagnostics
                    Don't print error messages and warnings
      --flatten-input
                    Print the input trace in a flattened form.
      --debug       Print stack traces for runtime errors
      --print-computation-depth
                    Print the length of the longest path a propagation message
                    travels
      --print-core  Print the Tessla Core representation generated from the
                    Tessla specification
      --print-recursion-depth
                    Print the length of the longest recursion
      --verify-only Only check the Tessla spec for errors and don't execute it
      --stop-on     Stop when the output stream with the given name generates
                    its first event
      --list-in-streams
                    Print a list of the input streams defined in the given
                    tessla spec and then exit
      --abort-at    Stop the interpreter after a given amount of events.
  -h, --help        Display this help message and exit
      --timeunit    Use the given unit as the unit for timestamps in the input
      --version     Display version information and exit
```

## Examples

Example tessla and input files can be found in the [tests directory](src/test/resources/de/uni_luebeck/isp/tessla/interpreter/tests) and in the [TeSSLa examples repository](https://gitlab.isp.uni-luebeck.de/tessla/rv-examples).
