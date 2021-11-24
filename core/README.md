# Core

This module contains the core compiler, which contains the entire frontend compilation pipeline for TeSSLa, responsible for parsing, typechecking and transforming the specification into a core representation, which can then be used by other backends.

Entry point for most use cases is [Compiler.scala](src/main/scala/de/uni_luebeck/isp/tessla/core/Compiler.scala)

## Translation Phases (v1.2.0)
The pipeline of translation phases is as follows:

TesslaParser → TesslaSyntaxToTessla → Flattener → TypeChecker → TypedTessla2TesslaASTCore → ConstantEvaluator

### TesslaParser
* Parses given TeSSLa code by using ANTLR4
* Is invoked twice: First to parse the given source code and its `includes`, and afterwards to parse the contents of the standard library.

→ TesslaParser.ParseResult

### TesslaSyntaxToTessla

* Translates the parse tree into a Tessla AST
* Extracts location information from the tokens.

→ Tessla

### Flattener

* Turns nested expressions into a flat, three-address-code-like structure
* Appends a unique numeric value to each identifier to make them unique across the entire specification
* Adds scope objects to the global and macro scopes, which map identifiers to their associated expressions, and produces errors for conflicting definitions in the same scope
* Removes block expressions and hoists them into the surrounding macro scope or the global scope)
* Transforms modules into records
* Resolves imports of modules by replacing usages of imported identifiers by a member-access on that module.

→ FlatTessla

### TypeChecker

* Annotates every expression with its type
* Checks for type errors
* Converts constant value expressions to `default(nil, value)`-streams where needed
* Converts primitive n-ary operator applications on streams `a + b` with an n-ary signal lift `slift(a, b, +)`

→ TypedTessla

### TypedTessla2TesslaASTCore

* Translates from the old TeSSLa 1.0 AST of the previous phase to the newly introduced AST in version 1.2.

→ TesslaAST.Typed

### ConstantEvaluator

* Uses the externs defined in `RuntimeExterns` to evaluate constant expressions.

### AnnotationValidator

* Ensures that annotation parameters are only using constant values.

### ConstantRecycler

* Reduces the core AST by combining assignments of the same expression to different identifiers to a single assignment.

→ TesslaAST.Core

## Further Information

Further details on the translation process can be found in the Scaladoc documentation of the single classes
