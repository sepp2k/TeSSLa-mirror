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

`java -jar target/scala-2.13/tessla-compiler-assembly-x.x.x-SNAPSHOT.jar input.tessla > Main.scala`

Further additional options are:

```
  -h, --help                Display help message and exit
      --debug               Print stack traces for runtime errors
      --no-diagnostics      Don't print error messages and warnings
      --no-mutability       Produce code with exclusively immutable datastructures
      --no-optimization     Produce non-optimized output code
  -o, --output-file         Location of the output (including filename)
      --version             Display version information and exit
  -t, --target              Target language: java (default), javascript, rust or
                            rust-bare
  -v, --verbose             Produce a lot of output

```

After that the Scala code can be compiled and the TeSSLa Monitor can be launched:

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

##  Code  structure

`IntermediateCode.scala` contains definitions for an imperative intermediate language to which TeSSLa is translated prior the translation to a concrete imperative language. `IntermediateCodeUtils.scala` and `IntermediateCodeTypeInference.scala` contain basic operations to deal with this intermediate language, including a DSL to easily create program fragments of this intermediate code. The translation of TeSSLa Core to Intermediate Code is launched in `TeSSLaCoreToIntermediate.scala`, which contains a `TranslationPhase` (inherited from the tessla project) from a `TeSSLaCoreSpecification` to a `SourceListing` (defined in `IntermediateCode.scala`). The main translation logic can be found in `StreamCodeGenerator.scala` (for the translation of Stream variable definitions) and `NonStreamCodeGenerator.scala` (for the further translation of expressions).

In the package `backends` the logic for the translation from intermediate code to the target languages is located. `BackendInterface.scala` contains the central interface all translation backends have to implement. The translation to Scala code is split in the files `ScalaBackend.scala` for translating the basic control structures and `ScalaConstants.scala` for further translation details.

## ToDo

+ Implement Mutability/Immutablity detection
+ Rust translation
+ Java-Script translation
+ Script performing compilation to bytecode/binary code
+ Fix TODOs in code
+ For further bugfixes see issue section in the gitlab
