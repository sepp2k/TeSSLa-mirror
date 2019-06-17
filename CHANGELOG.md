# Changelog

## Version 1.0.0 (upcoming)

### Breaking Changes

* The following keywords have been added and can no longer be used as variable names: `module`, `__builtin__`, `import`, `imexport`

### API Changes

* In the generated TeSSLa-Core many built-in operators are removed because they're defined in the standard library instead. Backend-defined built-ins are supported via the CustomBuiltIn class.
* Most TeSSLa-Core types have been replaced by BuiltInType(name: String)
* The `Compiler` methods now require additional options to select the used standard library and how to resolve includes

### Additions and Fixes

* A module system has been added. You can define modules using `module ModuleName { definitions }` and then access the module's definitions with `ModuleName.DefinitionName`. Modules can also be passed around as objects.
* Built-in macros and types can now be defined using `def ... = __builtin__(name)` and `type ... = __builtin__(name)` respectively
* There is now a standard library that is included automatically into all TeSSLa specs
* Types can now be used before their definition
* Generic macros can be passed as arguments to other macros and the type arguments will be inferred when possible

## Version 0.7.6

### Additions and Fixes

* Allow more line breaks. For a detailed lists of where line breaks can be used, see syntax.md.

### API Changes

* TesslaCore.Nil now has a member named `typ`

## Version 0.7.5

### Additions and Fixes

* Brings back online monitoring, which was broken since 0.7.0.

### API Changes

* `Trace.fromSource` now no longer takes an ANTLR `CharStream` but a `scala.io.Source` and there is a new `Trace.fromLineIterator` which takes an iterator of the lines of the trace.

## Version 0.7.4

### Additions and Fixes

* Fixed error handling for missing type annotations at input streams
* Fixed error handling for syntax errors which were reported wrongly as runtime errors

### API Changes

* The interpreter is not longer a translation phase. `Interpreter.run` is now just a normal function.

## Version 0.7.3

### Breaking Changes

* `tessla.version` is now `Tessla.version`
* TesslaDoc is now invoked via the new `tessladoc` script rather than `tessla --doc`

### Additions and Fixes
* String literals can now contain dollar signs by escaping them with a backslash: `\$`
* A bug has been fixed that prevented the use of non-empty tuples in input streams
* TeSSLa now supports formatted string literals that start with `f"` instead of just `"` and allow printf-style format-specifiers after string interpolations (e.g. `$myInt%04x`)
* The functions `format(String, Any): String`, `formatInt(String, Int): String` and `formatFloat(String, Float): String` have been added. They apply a format string to a single argument. These mainly exist because formatted string literals are desugared to calls to the functions. You probably don't want to use them directly. Note that calling these functions with an illegal format string will cause a runtime-exception, whereas formatted string literals would detect the error at compile time.
* Change the implementation of TeSSLa's `List` to use Scala's `Vector` instead of Scala's `List`. That way `List_append` and `List_last` are no longer O(n)
* Add built-in function `List_get` to access a list at a specified index
* Add built-in function `lift4`
* If `out expr` is used (rather than `out expr as name`), the inferred name will now be the actual source code of `expr`, not a "pretty-printed" version as before. This way something like `out "$x"` will now produce the output `23: "$x" = "42"` rather than being "pretty-printed" as `23: toString(x) = "42"`.

### API Changes
* The generated `BuildInfo` class has been moved from the `interpreter` package to the `tessla` package
* Methods `Result.combineAll`, `Result.runSequentially` and `Result#combine` have been added to more conventiently work with Result objects

## Version 0.7.2

### Breaking Changes
* It is no longer possible to write if-statements without an else-clause. Use `filter` instead.
  * `if c then a` treats `c` and `a` with signal semantics and produces an event for each event on `c` or `a`. The new `filter(a, c)` treats `c` as a signal and `a` as an event stream. Hence you a get a real filter of the event stream `a` without any additional events, which is in most cases what you want. If you really need the additional events you can reproduce this behaviour with `filter(first(a, c), c)`.
  * `if c then x` for a constant `x`, e.g. `5`, implicitly converts `x` to the constant stream `const(x, ())` and hence produces an event with value `x` for every truthy event on `c`. You can reproduce this behaviour with `filter(const(x, c), c)`.
* `delayedLast` no longer exists. Use `delay` instead.
  * `delayedLast` was a delayed `last` in the sense that the last seen value on `x` was used for events produced when the delay was over. The new `delay` no longer has this build-in `last` and produces unit events instead. `delay(d, r)` considers new events on the delay stream `d` only when they arrive simultaneously with an event on the reset stream `r` or when an old delay is over at that timestamp. An event on the reset stream without a simultaneous event on the delay stream cancels the currently active delay.
  * A non-recursive `delayedLast(v,d)` can be realized as `last(v, delay(d,d))` where the stream `d` is used as delay and as reset stream, which is possible if `d` is not recusively defined.
  * The recursive definition
    ```ruby
    def s: Events[Int] := default(delayedLast(s, s), f)
    ```
    which produces an event every `f` timeunits starting with `t=0` can now be realized as
    ```ruby
    def s: Events[Int] := const(f, default(delay(s, ()), ()))
    ```
    with the only reset event at `t=0`.
  * In the most general case where new delays are started by old delays _and_ external events one has to use every external event as reset, e.g. the specification
    ```ruby
    def s: Events[Int] := merge(delayedLast(s,s), f)
    ```
    can now be realized as
    ```ruby
    def s: Events[Int] := merge(first(f, delay(s,f)), f)
    ```

### Additions
* Some TesslaCore-to-TesslaCore optimizations have been added
* `lift1`, `min`, `max`, `minimum`, `maximum` and `sum` have been added as built-in functions
* The float operations `sin`, `cos`, `tan` and `atan` have been added as built-in functions
* Aggregating functions in the standard library now take into account events at t=0

### API Changes

* `Compiler.applyPasses` is now `Compiler.compile`
* Instead of `Interpreter.run*` taking Strings/streams there is now a single `Interpreter.run` that takes the TesslaCore.Specification as returned by `compile` and a `Trace`
* `TesslaCore.Lift`'s function is now represented as a `TesslaCore.Function`, not `TesslaCore.ValueArg`
* Scope and body have been renamed to body and result
* Translation phases now consist of a `TranslationPhase.Translator` that does the actual work and a `TranslationPhase` that wraps the `Translator` and can be composed with other translation phases or functions

## Version 0.7.1

### Additions

* `filter` now exists as a built-in function

## Version 0.7.0

### Breaking Changes

* After being deprecated for some time, the `define` key no longer exists and `def` should be used instead
* Line breaks are now significant and can no longer be used in arbitrary places. For a detailed lists of where line breaks can be used, see syntax.md (that is the syntax.md of version 0.6.3 as that's the earliest it has been updated). Line breaks can also be escaped with a backslash
* Object members can now only have the form `ID = expression` (or equivalently `ID: expression`) or `ID`. The other forms were barely documented and never used and are now gone.
* Backslashes and dollar signs can no longer be used unescaped inside string literals (see string interpolation and escape sequences below)
* There no longer is sub-typing for tuples and normal object types. That is, if you have, say, a 3-element tuple or an object with members `x` and `y`, you can now no longer pass them to functions expecting 2-element tuples or objects with only an `x` member. If you want subtyping for objects, use open object types instead (see below)
* Some of the syntactic variations to specify time ranges in the extended input format have been removed as they've been undocumented, untested and often buggy. This is a breaking change in the sense that if there had been any code using these features, it would now no longer work. However, it is an utterly irrelevant change in the sense that no one ever used these features or even knew they existed or how they were supposed to work.

### Deprecations

* Using `${` for object literals is deprecated, you can now use `{` as the beginning of an object literal
* Using `return` is deprecated. Since newlines terminate definitions now, you can simply write blocks without `return` and there will be no ambiguities with prefix operators or time units
* Using `fun` is deprecated, you can now simply write lambda expressions without it

### Additions

* Strings now support the following escape sequences:
  * `\\`: a backslash
  * `\"`: a double quote
  * `\r`: a carriage return
  * `\n`: a newline
  * `\t`: a tabulator
  * `\a`: the system bell (Unicode code point 0007)
* Strings now support string interpolation by writing `$identifier` or `${expression}` inside a string literal. This will call `toString` on the value of the given identifier or expression and insert it into the string at the given point.
* You can now use `print` instead of `out` to print a given value (applying `toString` if the value isn't already a string) without adding the standard formatting (i.e. `timestamp: name = value`) to it. That is, you can now print information using arbitrary formatting by building a custom string using string interpolation and then `print` to print it
* Identifiers can now contain Unicode characters
* Integers can now be written using Unicode digits and/or hexadecimal notation
* We now support double-precision floating point numbers with a decimal literal notation, the type `Float` and the following built-in operations: `+.`, `-.`, `*.`, `/.`, `<.`, `>.`, `>=.`, `<=.`, `intToFloat`, `floatToInt`, `pow`, `log(n, base)`
* We now support the modulo operator for integers (`%`)
* We now support open object types for function parameters. These are object types that have `..` as their last member (e.g. `{x: Int, ..}`). If a function takes such a type as a parameter, you can pass any object as an argument as long as the object has at least the members specified (that is, it is allowed to have additional members). Functions taking non-open object types will only accept objects that have exactly the members specified and no others. Tuples are non-open object types.
* The unit value, the empty tuple and the empty object are now all the same thing. The type `Unit` is now simply an alias for the type `()` (which can also be written as `{}`).
* TesslaDoc comments (comments that start with `---` or `##`) can now be extracted using the `--doc` or `--doc-all` flags (the latter includes tessladocs from included files, the former only from the main file)
* The input parser is now a lot faster (up to a factor of 10)

### API Changes

* The TesslaSource class no longer exists. ANTLR's CharStream class should be used instead.
* The TesslaCore.Specification class now contains proper case classes to describe the contained streams instead of using tuples. Type information for streams is included again.
* TesslaCore.Unit no longer exists - you'll need to check for / use empty objects instead

## Version 0.6.5

### Additions

* Add the option `--print-node-count` to print the number of nodes in the TeSSLaCore graph

## Version 0.6.4

### Additions

* Static-if-then-else can now be used inside lifted functions. This makes it possible to lift recursive functions
* Add built-in functions `toString[T](x: T): String` and `String_concat(x: String, y: String): String`

## Version 0.6.3

### Additions

* Add the ability to execute a TeSSLa specification through the Java API

## Version 0.6.2

### Additions

* A built-in function `count` has been added

## Version 0.6.1

### Additions

* Objects, set, maps and options can now be used in input streams
* Add delay operator as described in the TeSSLa paper
* Add built-in function `Set_fold`
* The type `List[T]` has been added with the functions `List_empty`, `List_size`, `List_append`, `List_prepend`, `List_head`, `List_tail`, `List_init`, `List_last`, `List_fold`


## Version 0.6.0

### Additions

* Lambda functions are now supported with the syntax `fun (paramList) => body` (the need for the keyword `fun` will be dropped in version 0.7.0)
* Add option types (type `Option[T]` with constructor functions `Some[T](x: T)` and `None[T]` and the functions `isNone` and `getSome`)
* Add built-in function `lift` that takes two streams and a function taking two values and then lifts that function to the streams as per the lift semantics from the TeSSLa paper. Also add `lift3` that does the same thing for three streams.
* Objects are now value types if all their members have value types. It is now possible to have streams of such objects.
* There now is a Java API

### API changes

* The old `TesslaCore.Lift` is now called `TesslaCore.SignalLift`, `TesslaCore.Lift` now refers to the new lift operator

## Version 0.5.3

### Additions

* Explicitly set stream encoding

## Version 0.5.2

### Additions

* `tessla.version` has been added to return the version of the current TeSSLa implementation
* Objects whose members are assigned to local variables of the same name can now be defined using the shorthand `${x, y}`
* The syntaxes `expression "where" { definition* }` and `{ definition* "return" expression }` have been added as a replacement for regular block syntax, which suffered from ambiguities. Note that this will change again in version 0.7.0 and regular block syntax, now unambiguous, will be preferred again.

## Version 0.5.1 (Changes Since 0.5.0)

### Additions

* CTF support
* It is now possible to use multiple unary operators in a row
* Add support for objects and tuples
* The syntax of function calls now allows non-identifier expressions as functions (that is, first-class functions can now be called without storing the function in a variable first)
* Type aliases can now be defined using the `type` keyword
