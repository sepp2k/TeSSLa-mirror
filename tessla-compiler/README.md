#  Tessla-Compiler

TeSSLa Compiler is the sub-module for the translation of TeSSLa specifications to Scala/Rust and JavaScript.
Currently, only the translation to Scala is implemented.

The translation to Scala supports generation of a `.scala` source code file, or generation of a fully ready to use `.jar` file.

##  Usage

TeSSLa Compiler can be used to create a Scala monitor either as source or jar archive.
One can further choose whether to include an I/O interface in the monitor to read and write events from and to stdio or generate API code accessable from another Scala/Java application.

For detailed usage information see this [documentation](doc/Usage.md).

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
