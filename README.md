[![pipeline](https://git.tessla.io/tessla/tessla/badges/development/pipeline.svg)](https://git.tessla.io/tessla/tessla/-/jobs)

# TeSSLa Compiler and Backends

This project provides the core functionalities for the TeSSLa language.

# Usage
## Download

You can get the latest version of TeSSLa from the [Gitlab Artifact Browser](https://git.tessla.io/tessla/tessla/builds/artifacts/master/browse/target/scala-2.13?job=deploy).

## Building

The project can be built using [sbt](https://www.scala-sbt.org/), by running `sbt assembly`. The finished assembly will then be located under `target/scala-*/`. This also generates assemblies for all sub-modules, which will be located in their respective target directories.

If you want to build only a specific sub-project, run `sbt <project>/assembly` instead. (e.g. `sbt core/assembly`). The project names for sbt can be found in the [build.sbt](build.sbt).

## Docker Image

TeSSLa is also available as part of the TeSSLa docker image, which is especially useful if you want to use TeSSLa on instrumented C code. See [https://www.tessla.io/rv-tutorial](https://www.tessla.io/rv-tutorial) for more infos.

## Project Structure

This project consists of several submodules:

- [Core](core/README.md):                    Sub-folder `core/`                Common Compiler Frontend, translating TeSSLa to TeSSLa Core
- [Interpreter](interpreter/README.md):      Sub-folder `interpreter/`         Interactive Shell for evaluation of TeSSLa specifications
- [Compiler](tessla-compiler/README.md):     Sub-folder `tessla-compiler/`     Efficient compilation of TeSSLa Core to a Scala/Rust monitor
- [Doc](docs/README.md):                     Sub-folder `docs/`                Generation of documentation from comments in a TeSSLa specification (Tessladoc)
- [Instrumenter](instrumenter/README.md):    Sub-folder `instrumenter/`        Instrumentation of C code to generate traces for a TeSSLa specification (linux-amd64 only)

The code for the common Command line interface is located in `src/`

## Documentation

For more details on the different modules take a look at their respective READMEs.

Additionally, the generated scaladoc for each module can be found on the Artifact browser (see above) or at `target/scala-*/api/` after generating it through `sbt doc`.

## CLI

The CLI of TeSSLa follows a command based structure. The following commands are available:

| Command      | Description                                                 |
|--------------|-------------------------------------------------------------|
| interpreter  | Evaluate a specification with a given trace (file or stdin) |
| compile-core | Compile TeSSLa to TeSSLa Core and print the result          |
| doc          | Generate documentation for TeSSLa code in JSON format       |
| compile      | Generate a Scala/Rust monitor from a specification          |
| instrumenter | Instrument C code based on the provided annotations         |


For detailed usage information, take a look at the following help text.
```
tessla 1.2.3
Usage: tessla [interpreter|compile-core|doc|compile-scala|compile-rust|instrumenter] [options] <args>...

Compile Tessla specifications and evaluate them on provided input streams.
  -t, --base-time <value>  Use the given time constant (including a unit) as the reference time for time literals
  -s, --stdlib <file>      Provide a standard library to use instead of the default one.
  --no-diagnostics         Suppress error messages and warnings
  --debug                  Print stack traces for errors and provide more verbose output

  -h, --help               Prints this help message and exit.
  --version                Print the version and exit.
  -l, --license            Print the legal information for this software and exit.

Command: interpreter [options] [<tessla-file>] [<trace-file>]
Evaluate the given Tessla specification on the input streams provided by a trace file.
  <tessla-file>            The file containing the Tessla specification
  <trace-file>             The file containing the trace data used as input for the specification. If this is not provided, input is read from stdin
  -S, --stop-on <value>    Stop when the output stream with the given name generates its first event
  -r, --reject-undeclared-inputs
                           Throw an error if an undeclared input stream occurs in the trace data
  -a, --abort-at <value>   Stop the interpreter after a given amount of events.
  --ctf                    The trace-file with the input data is in CTF format. With this option you must specify a trace-file. stdin is not supported.
  --csv                    The trace-file or the input stream is in CSV format.

Command: compile-core [options] <tessla-file>
Compile the provided specification to Tessla Core
  <tessla-file>            The file containing the Tessla specification

  -c, --print-core         Print the extended Tessla Core representation generated from the Tessla specification
  --print-core-lanspec     Print the Tessla Core representation conform to the language specification.
  --print-typed            Print the typed Tessla representation generated from the Tessla specification
  --print-locations        Print ASTs with locations
  --print-all-types        Print ASTs with all types
  --list-out-streams       Print a list of the output streams defined in the given Tessla specification and then exit
  --list-in-streams        Print a list of the input streams defined in the given Tessla specification and then exit
  -a, --export-annotations <value>
                           Generate a Json file containing the annotation information of the specification

Command: doc [options] [<files>]
Generate documentation for Tessla code
  --include-stdlib         Include documentation for definitions from the standard library
  -i, --includes           Include documentation from included files
  -o, --outfile <value>    Write the generated docs to the given file instead of stdout
  <files>                  The TeSSLa files for which to generate documentation

Command: compile-scala [options] <tessla-file>
Compile TeSSLa specifications to Scala
  <tessla-file>            The file containing the Tessla specification
  -a, --add-source <value>
                           Additional source file included on top of the generated source
  -o, --out-file <value>   Place the generated Scala source code at this location.
  -j, --jar-file <value>   Compile TeSSLa specification to an executable jar file which is created at the given location.
  -n, --no-io              Replaces I/O Handling in generated source with simple API interface

Command: compile-rust [options] <tessla-file>
Compile TeSSLa specifications to Rust
  <tessla-file>            The file containing the Tessla specification
  -b, --bin-file <value>   Compile a full monitor binary and place it in the given location
  -a, --add-source <value>
                           Additional rust file inserted at the top of the monitor library
  -p, --project-dir <value>
                           Export a Cargo workspace with everything necessary to modify and build the tessla monitor yourself
  -m, --io-interface       Generate the code for an executable I/O interface (src/main.rs) in the exported workspace

Command: instrumenter <tessla-file> <c-file> [<include-path>...]
Instrument C code based on the provided annotations (linux-amd64, windows-x64 only)
  <tessla-file>            The file containing the Tessla specification, with annotations for the instrumentation.
  <c-file>                 Instrument the provided C file according to the specification
  <include-path>...        Include paths for the C compiler

```

## Examples

Example tessla and input files can be found in the [tests directory](resources/de/uni_luebeck/isp/tessla/common) and in the [TeSSLa examples repository](https://git.tessla.io/tessla/examples).
