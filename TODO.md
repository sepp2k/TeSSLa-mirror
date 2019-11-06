* [Prio 10] Allow non-fatal errors that cause the translation phase to still produce a result (containing error nodes); the result could then be recovered to be put into the next phase if desired (so we could still run the type checker on a program containing scoping errors). This means the error node should be distinct from other nodes (as opposed to just being an identifier node named "error" as it is now), so later phases don't report another (possibly internal) error while handling a node that's already caused an error previously. We could also print the tree containing the error nodes in --debug mode to give additional info on why an error occurred.
* Get rid of the Definitions data structure in Flat/TypedTessla. Definitions is a hierarchical data structure which can look up stuff in itself including the parent. In most usages one wants to look up and modify stuff explicitly without automatic consideration of the parent.

* Improve handling of generics in type checking and the inference of type arguments, separate type lambdas from value lambdas. Whenever a type lambda appears outside of a type-application, try to instantiate it to the required type or produce an error
** To make use of the better inference for `default(nil, x)`, consider changing the argument order of `default` or add a special hack to make `x` be typechecked before `nil`, so that the type of `x` can be used to determine the type of `nil`.

* Pattern matching
* ADTs, pattern matching

* Remove flattening phase and go directly from Tessla -> TypedTessla where TypedTessla is not in ANF. ANF won't enter until TesslaCore, which won't actually change much for the ConstantEvaluator. Also get rid of the Scope-datastructure while we're at it.
                                               ANF
** TesslaSyntax -> SimpleTessla -> TypedTessla ----> TesslaCore
** The type checker can then propagate expected types down the tree within a single expression, so that we can get more type inference

* Make InterpreterTests work with print statement
* Binary literals
* Global ID counter instead of ID factory?

* Create tests for syntax errors
* Allow using types from modules
* import
* Allow using the module name from within the module (use qid-resolving instead of object access to access members of a known module)
* Add infoUrl: Option[String] to diagnostics
* Fix named arguments - handle all named arguments in the same place
* Type checking for annotations
* Make annotations part of the type
* Annotations for out streams

* error built-in
* trace back for runtime errors

* Fix string-serialization of recursive functions -> give the function names instead of using lambda as value directly

* Locations spinnen manchmal total, also gerade die zur Laufzeit