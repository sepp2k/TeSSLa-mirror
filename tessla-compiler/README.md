#  Tessla-Compiler

TeSSLa Compiler is the sub-module for the translation of TeSSLa specifications to Scala/Rust and JavaScript.
Currently the translation to Scala and to Rust is implemented.

The translation to *Scala* supports generation of a `.scala` source code file, or generation of a fully ready to use 
`.jar` file. The translation to *Rust* always generates a complete Cargo project.

##  Usage

The TeSSLa compiler can be used to generate a monitor either as a source file or as a binary artifact.
One can further choose whether to include an I/O interface in the monitor to read and write events from and to stdio 
or generate API code accessable from another Scala/Java or Rust application.

For detailed usage information see this [documentation](doc/Usage.md).

## Architecture

The project is a backend to the [TeSSLa compiler frontend](../core/README.md) project.

It consists of `TranslationPhases` which are run sequentially.
The TeSSLa Core AST received from the frontend is preprocessed, translated to a common intermediate language and then 
translated to the target language.

Details on the architecture can be found in the [architecture documentation](doc/Architecture.md) or in the Scaladoc 
contained in the source code.

## ToDo

+ Include Mutability/Immutablity detection in master branch
+ Java-Script translation
+ For further bugfixes see issue section in the gitLab
