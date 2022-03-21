#  Tessla compiler Usage

The TeSSLa compiler can be run in the following way:

```
java -jar tessla.jar compile [options] <tessla-file>
```

Following options are available:

```
-a, --add-source <value>         Additional source file included on top of the generated source
-o, --out-file <value>           Place the generated source code at this location.
-b, --bin-file <value>           Compile TeSSLa specification to an executable binary file which is created at the given location.
-n, --no-io                      Replaces I/O Handling in generated source with simple API interface
-g, --target-language <value>    Select the target language to compile to: (scala, rust)
```

## Scala

### Stdio monitor generation

Without further options the TeSSLa compiler generates a file `out.scala` which can be compiled with `scalac` and executed as monitor.
This monitor accepts events via stdio. More detailed information about the input/output format see [here](../../IO.md).
Note: If the monitor receives more than one input for the same stream at the same timestamp, the later ones are ignored.

Instead of compiling to Scala source one can directly compile to an executable jar file by using the `-b` parameter followed by the
path of the jar archive that shall be created. The generated jar file can directly be launched via `java -jar ...` and behaves exactly
like the monitor generated from source.

### API monitor generation

It is also possible to generate a monitor with an API interface and without I/O handling. This may especially be useful if one wants
to access the monitor from another Java or Scala application. The compilation to API code can be achieved with the `-n` flag.
The Api monitor can be generated as Scala code and as (non-executable) jar archive as well. Details about how the API can be accessed
can be found [here](API.md).

### Connecting TeSSLa to external functions

Further it is possible to include Scala (and hence also Java) functions in the monitor. Therfore write your functions in a file
`addSource.scala` and include them by passing `-a addSource.scala`. The code from `addSource.scala` is then included at the very top
of the generated monitor. The corresponding functions and types can then be accessed from a TeSSLa specification via the `extern`
keyword and `native:` put in front of the extern name.

If you for example define a Scala function

```
object foo {
    def bar(x: Long) : Long = x + 5
}
```

in Scala you can import this function as

```
def bar(x:Int) : Int = extern("native:foo.bar")
```

in your TeSSLa specification and use it (Note: Int in TeSSLa is translated to Long in Scala). 
Also a reference to the Java standard library is possible this way:

```
def bas(x:Int) : Int = extern("native:java.lang.Math.abs")
```

A list how TeSSLa types correspond to Scala types can be found [here](Types.md).

For detailed examples of using external Scala functions see this testcases

* externalFunctionUse
    * [externalFunctionUse.tessla](../src/test/resources/de/uni_luebeck/isp/tessla/tesslac/nativeExterns/externalFunctionUse.tessla)
    * [externalFunctionUse.scala](../src/test/resources/de/uni_luebeck/isp/tessla/tesslac/nativeExterns/externalFunctionUse.scala)
    
* externalTypes
    * [externalTypes.tessla](../src/test/resources/de/uni_luebeck/isp/tessla/tesslac/nativeExterns/externalTypes.tessla)
    * [externalTypes.scala](../src/test/resources/de/uni_luebeck/isp/tessla/tesslac/nativeExterns/externalTypes.scala)
    
Note: 

* Use of Scala externs is currently only supported by the compiler backend not by the interpreter
* Expressions using Scala externs cannot be evaluated during compilation time. This may in some cases avoid macro expansion.
  If a macro (function) which is receiving or returning a stream variable cannot be evaluated at compilation time due to 
  a non evaluateable Scala extern compilation will refuse according to the TeSSLa language specification.
* As consequence of the previous point: Extern Scala functions cannot directly receive streams but can be made liftable and 
  applied on streams.

## Rust

### Stdio monitor generation

Without further options the TeSSLa compiler generates a file `out.rs`.
You cannot compile this file directly because the TeSSLa standard library is not included. You can
pass a folder name with the `-b` parameter. Then a complete Cargo project is created and compiled directly into a monitor .
This monitor accepts events via stdio. More detailed information about the input/output format see [here](../../IO.md).
Note: If the monitor receives more than one input for the same stream at the same timestamp, the later ones are ignored.

### API monitor generation

It is also possible to generate a monitor with an API interface and without I/O handling. This may especially be useful if one wants
to access the monitor from another application written in any language that can interface to Rust.
The compilation to API code can be achieved with the `-n` flag.
The API monitor can be generated as Rust code and as (non-executable) library as well. Details about how the API can be accessed
can be found [here](API.md).

### Connecting TeSSLa to external functions

This feature is in an experimental state and is disabled in the current version of the TeSSLa compiler
(see [TesslaCoreToRust.scala](../src/main/scala/de/uni_luebeck/isp/tessla/tessla_compiler/backends/rustBackend/TesslaCoreToRust.scala)`: insertSegments()`).
