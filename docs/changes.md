# Changelog

## Version 0.7.0 (Changes Since 0.6.5)

### Breaking Changes

* After being deprecated for some time, the `define` key no longer exists and `def` should be used instead
* Line breaks are now significant and can no longer be used in arbitrary places. For a detailed lists of where line breaks can be used, see syntax.md (that is the syntax.md of version 0.6.3 as that's the earliest it has been updated). Line breaks can also be escaped with a backslash
* Object members can now only have the form `ID = expression` (or equivalently `ID: expression`) or `ID`. The other forms were barely documented and never used and are now gone.
* Backslashes and dollar signs can no longer be used unescaped inside string literals (see string interpolation and escape sequences below)
* There no longer is sub-typing for tuples and normal object types. That is, if you have, say, a 3-element tuple or an object with members `x` and `y`, you can now no longer pass them to functions expecting 2-elment tuples or objects with only an `x` member. If you want subtyping for objects, use open object types instead (see below)
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
  * `\a`: the system bell (unicode code point 0007)
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

## Version 0.7.1

### Additions

* `filter` now exists as a built-in function

## Version 0.7.2

### Breaking Changes
* It is no longer possible to write if-statements without an else-clause. Use `filter` instead.
* `delayedLast` no longer exists. Use `delay` instead. Examples:
** A non-recursive `delayedLast(x,y)` becomes `last(x, delay(y,y))`
** `def s: Events[Int] := default(delayedLast(s, s), delay)` becomes `def s: Events[Int] := default(const(amount, delay(s, ())), amount)`
** TODO Malte: expand this

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

## Version 0.7.3 (upcoming)

### Additions and Fixes
* String literals can now contain dollar signs by escaping them with a backslash: `\$`
* A bug has been fixed that prevented the use of non-emtpy tuples in input streams

### API Changes
* The generated `BuildInfo` class has been moved from the `interpreter` package to the `tessla` package
