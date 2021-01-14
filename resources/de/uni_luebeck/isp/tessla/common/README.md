TeSSLa test cases and examples
==============================

This folder contains several test cases and examples in the following folders:
* `datastructures` contains tests operating with complex data structures like sets, maps etc.
* `errors` contains test cases related to errors and warnings thrown by the compiler or runtime errors thrown by the interpreter.
* `functions` contains tests which make use of functions which are defined inside the TeSSLa specification.
* `lift` contains testcases related to lifting operations on streams.
* `misc` contains test cases testing miscellaneous features of the software back-end and the compiler, which might not be available or relevant on the hardware.
* `objects` contains tests using record and tuple data structures.
* `operators` contains testcases for the basic built-in stream functions and operators.
* `scenarios` contains general TeSSLa examples which are not specific to the software back-end. These examples must not throw errors or warnings.
* `stdlib` contains tests for complex functions which are contained in the stdlib.

The tests in this folder are executed and checked on every backend.
The single backends also contain dedicated test folders with tests that are only executed on these backends. It is also possible that a backend overrides a test from this common test folder in its private test folder.
