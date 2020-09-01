#  Tessla-Compiler

TeSSLa Compiler is the sub-module for the translation of TeSSLa specifications to Scala/Rust and JavaScript.
Currently, only the translation to Scala is implemented.

The translation to Scala supports generation of a `.scala` source code file, or generation of a fully ready to use `.jar` file.

##  Event format

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

The project is a backend to the [TeSSLa compiler frontend](../core/README.md) project.

It consists of `TranslationPhases` which are run sequentially.
The TeSSLa Core AST received from the frontend is preprocessed, translated to a common intermediate language and then translated to the target language.

Details on the architecture can be found in the [architecture documentation](doc/Architecture.md) or in the Scaladoc contained in the source code.

## ToDo

+ Include Mutability/Immutablity detection in master branch
+ Rust translation
+ Java-Script translation
+ For further bugfixes see issue section in the gitLab
