Architecture of the Compiler Backend
====================================

The compiler backend is able to translate TeSSLa core to Scala and Rust and can be
extended to other imperative languages.

The translation works as follows:

The TeSSLa frontend produces a TeSSLa Core AST which is received by the backend.
The AST is in the Core format as described in the Language Specification with
one exception:

- Extern Expressions can be used directly in call expression without the detour
of storing them into a separate variable. Nevertheless the compiler does not
require externs to be inlined this way.

Compilation process
-------------------

In the following the compilation process is described step by step.
It consists of `TranslationPhases` which are run sequentially.

### Scala

#### Preprocessing

First the compiler backend preprocesses the received Core AST. Therefore it
produces an `ExtendedSpecification` which is a container class for the CoreAST
and

- Information which variable identifiers are used in the expressions which are
assigned to other variables. This is used for finding an evaluation order but
also for other preprocessing stages. The corresponding analysis of the AST is
done in `preprocessing.UsageAnalysis`

- Information which variables are always inlined and which have to be translated
as lazy assignments. Inlining or lazy variable assignments are important for the
following reason. Consider this TeSSLa Specification snippet:

  ``` def a : Int = 4 / x def b : Int = extern_ite(x != 0, a, 0) ```

  The if expression takes its second and third parameter lazy. However in the
case of `x = 0` the non-lazy assignment to a would already cause the program to
panic eventhough a is never used. That is why we want to translate the
assignments to something like this: (here in Scala)

  ``` lazy val a = 4 / x val b = if (x != 0) a else 0 ```

  Alas lazy vals are not that efficient so instead of the translation to a lazy
assignment we just inline the expression to where it is used, if the variable is
only used once in the program (here in Scala again):

  ``` val b = if (x != 0) 4 / x else 0 ```

  This lazyness analysis is performed in `preprocessing.LazynessAnalysis`

#### Translation into Intermediate Code

A TeSSLa Core specification is not directly translated into Scala but
first into a common abstract imperative language.

This language is defined in `IntermediateCode`. The generated container is
called `SourceListing` and contains several sections. This sections are added at
different places in the final monitor code.

The sections are:

- `stepSource`       Statements executed when a new timestamp arrives -
`tailSource`       Statements executed when a calculation has just been
performed - `tsGenSource`      Statements executed to negotiate the next active
timestamp - `inputProcessing`  Statements executed to process the input -
`staticSource`     Statements executed prior monitor execution

The intermediate code consists of statements, expressions and types. The
translation composes these artifacts to the statement sequences named above.

The translation from the TeSSLa AST to a `SourceListing` is done by the class
`TeSSLaCoreToIntermediateCode` using the classes `StreamCodeGenerator` and
`NonStreamCodeGenerator`. The first one translates expressions of type
Events[...] the second all other expressions.

Non stream code is translated straight forward to equivalent assignments in
`IntermediateCode` and added to `staticSource` since these assignments are
static and can be calculated once at the beginning of the monitoring.

The streams are calculated in the following way: For every stream `x` we create
the variables

- `x_value` carrying the last (or current) value of `x` - `x_ts` carrying the
timestamp of `x`s last (or current) event - `x_init` flag indicating if `x`
already had a value - `x_changed` flag indicating if `x` has an event at the
current timestamp - `x_error` carrying the error if the last value of `x` could
not be determined - `x_unknown` flag indicating whether it is unclear if `x`
currently has an event (due to an error in a lifted function. Equals the cross
in the LanSpec)

A last expression may also refer to the last value excluding the current one so
some of the variables named above also exist with prefix `last`.

Every stream assignment in the specification is then translated to a set of
assignments to the variables listed above according to the current values.

The principle of the generated monitor is the following:

- Inputs from stdin are consumed and stored to string variables `inputStream`,
`value`. They are parsed and the variables of input streams are set accordingly
in the `inputProcessing` section. - Always when a timestamp greater than the
previous one arrives `tsGenSource` section is run and the next timestamp for
which the stream have to be calculated is stored in the variable `currTs` until
`currTs` is set to the timestamp where not all inputs are already received. Then
inputs are read again. Note here that `currTs` may be set to timestamps where no
input stream has an event but delay expressions have. - If `currTs` is set to a
timestamp smaller then the latest inputs the `stepSource` section is executed
where all stream variable values at `currTs` are  calculated. After that
`tailSource` is called where for example outputs can be generated.

The class `IntermediateCodeUtils` contains lots of useful functions for
generating the intermediate code, like TeSSLa to intermediate code type
conversion or a DSL for generating intermediate code.

After the code is generated typecasts are included where necessary. This is done
by class `IntermediateCodeTypeInference`. Also unused variables which may exist
e.g. by generated `_lastvalue` variables without lasts in the specification or
variables which were inlined (`UnusedVarRemove`).

#### Translation to the target Source code

For the translation to real source code the interface
`backends.BackendInterface` exists. It loads a template source file from the
resources and replaces comments in there by the translations of the statement
sequences in the generated `SourceListing`. So it is a `TranslationPhase` from
`SourceListing` to String.

Implementations for the interface must override the two methods

- `generateVariableDeclarations` which produces variable declarations -
`generateCode` which translates sequences of intermediate code assignments to
real-world code.

The Scala translation can be found in `backends.scalaBackend.ScalaBackend`. It
inherits `backends.BackendInterface`.

#### Further translation steps

For Scala there is also the possibility to generate the monitor as fat jar.
This is done in class `backends.scalaBackend.ScalaCompiler`.
Therefore the standard scala compiler is used as resource in the sbt project.
To generate a fat jar, all scala dependencies are extracted from the
`scala-library.jar` which is used by the compiler itself and packed into the
generated monitor.

### Rust

#### Preprocessing

The compiler starts with the `ExtractAndWrapFunctions` phase. In this phase, nested functions are unwrapped and boxed so
that they can call themselves recursively. In the next phase, the `FormatStringMangler`, calls to `String.format()` are 
transformed.
This has the reason that Rust's format! macro must know the format string parameter at compile time. In the third
preprocessing phase `EscapeInvalidIdentifiers`, characters that are not allowed to be part of an identifier in Rust are 
replaced by valid characters. The last phase of _rust specific_ preprocessing `GenerateStructDefinitions` iterates over 
the core AST,
filtering out all tuples and records and generating the corresponding Rust structure definitions.

The two preprocessing phases `UsageAnalysis` and `InliningAnalysis` are described in detail above in the Scala section.

#### Translation into Intermediate Code

The intermediate code is neither generated nor used by the Rust backend.

#### Translation to the target source code

The translation to Rust source code is performed by
[TesslaCoreToRust](../src/main/scala/de/uni_luebeck/isp/tessla/tessla_compiler/backends/rustBackend/TesslaCoreToRust.scala).
It loads a [template source file](../src/main/resources/de/uni_luebeck/isp/tessla/tessla_compiler/RustSkeleton.rs) from 
the
resources and replaces comments in there by the translations of the statement
sequences in the generated `SourceListing`. It is a `TranslationPhase` from
`SourceListing` to String. The generation of the Rust code is handled here by two code generators: 
The 
[RustStreamCodeGenerator](../src/main/scala/de/uni_luebeck/isp/tessla/tessla_compiler/backends/rustBackend/RustStreamCodeGenerator.scala)
generates stream-related function calls such as lifts and I/O functions.
The 
[RustNonStreamCodeGenerator](../src/main/scala/de/uni_luebeck/isp/tessla/tessla_compiler/backends/rustBackend/RustNonStreamCodeGenerator.scala) 
generates code such as for external and integrated function applications and assignments.

#### Further translation steps

For Rust there is also the possibility to generate the monitor as a Cargo project.
This is done by the
[RustCompiler](../src/main/scala/de/uni_luebeck/isp/tessla/tessla_compiler/backends/rustBackend/RustCompiler.scala).
The Cargo project is generated on the fly. The standard library is copied from the resources and included as a sub
project.

Further documentation
---------------------

Further details on the single classes and methods can be found in the Scaladoc
comments in the source.
