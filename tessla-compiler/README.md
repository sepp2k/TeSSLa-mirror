#  Tessla-Compiler

TeSSLa Compiler is the sub-module for the translation of TeSSLa specifications to Scala/Rust and JavaScript.
Currently, only the translation to Scala is implemented.

The translation to Scala supports generation of a `.scala` source code file, or generation of a fully ready to use `.jar` file.

## Usage

The TeSSLa compiler can be run in the following way:

```
java -jar tessla.jar compile [options] <tessla-file>
```

Following options are available:

```
-o, --out-file <value>      Place the generated Scala source code at this location.
-j, --jar-file <value>      Compile TeSSLa specification to an executable jar file which is created at the given location.
```

## Stdio monitor generation

With the parameter `-o` TeSSLa compiler generates a file `<name>.scala` which can be compiled with `scalac` and executed as monitor.
This monitor accepts events via stdio (see section below).

Instead of compiling to Scala source one can directly compile to an executeable jar file by using the `-j` parameter followed by the
path of the jar archive that shall be created. The generated jar file can directly be launched via `java -jar ...` and behaves exactly
like the monitor generated from source.

##  Event format

Events can then be passed to the monitor via `stdin` in the following format:

```
timestamp: streamname=value
```
or in case of unit streams

```
timestamp: streamname
```

The timestamps have to arrive in an ordered way. Events for passed timestamps will be printed in the same format, the first time a greater timestamp arrives or when the input ends (Ctrl+D/EOF).
Additional infos on the input/output format can be found in [the I/O documentation](doc/IO.md)

## Architecture

The project is a backend to the [TeSSLa compiler frontend](../core/README.md) project.

It consists of `TranslationPhases` which are run sequentially.
The TeSSLa Core AST received from the frontend is preprocessed, translated to a common intermediate language and then translated to the target language.

Details on the architecture can be found in the [architecture documentation](doc/Architecture.md) or in the Scaladoc contained in the source code.

## ToDo

+ Include Mutability/Immutablity detection in master branch
+ Rust translation
+ Java-Script translation
+ For further bugfixes see issue section in the gitLab
