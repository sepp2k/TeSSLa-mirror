#  tessla-compiler  -  Compiler from TeSSLa to imperative languages

Compiler for TeSSLa specifications written in Scala

##  General
TeSSLa Compiler is a project for the translation of TeSSLa specifications to Scala/Rust and JavaScript.
Only the translation to Scala is implemented, yet.

This repository is based on the [tessla](https://gitlab.isp.uni-luebeck.de/tessla/tessla) project.

##  Build

The source is available as a [sbt](https://www.scala-sbt.org/) project and can be built as follows:

  ```
  git clone git@gitlab.isp.uni-luebeck.de:tessla/tessla2code.git
  cd tessla2code
  sbt assembly
  ```

The compiler's jar archive can be found under `target/scala-2.13/tessla-compiler-assembly-x.x.x-SNAPSHOT.jar`


##  Usage

Once the project is built, the compiler can be used from the command line with the following command:

`java -jar target/scala-2.13/tessla-compiler-assembly-x.x.x-SNAPSHOT.jar input.tessla -o Main.scala`

Further additional options are:

```
  -h, --help             Display this help message and exit
      --debug            Print stack traces for runtime errors
      --no-diagnostics   Don't print error messages and warnings
      --no-optimization  Produce non-optimized output code
  -o, --output-file      Location of the output (including filename)
      --version          Display version information and exit
  -t, --target           Target language: scala (default), javascript, rust or
                         rust-bare
  -v, --verbose          Produce a lot of output

```

After that the Scala code can be compiled and the TeSSLa monitor can be launched:

```
scalac Main.scala
scala Main
```

Events can then be passed to the Monitor via `stdin` in the following format:

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

The project is a backend to the [TeSSLa compiler frontend](https://gitlab.isp.uni-luebeck.de/tessla/tessla) project.

It consists of `TranslationPhases` which are run sequentially.
The TeSSLa Core AST received from the frontend is preprocessed, translated to a common intermediate language and then translated to the target language.

Details on the architecture can be found in the architecture [documentation](doc/Architecture.md) or in the Scaladoc contained in the source code.

## ToDo

+ Include Mutability/Immutablity detection in master branch
+ Rust translation
+ Java-Script translation
+ (Script) performing compilation to bytecode/binary code
+ For further bugfixes see issue section in the gitLab
