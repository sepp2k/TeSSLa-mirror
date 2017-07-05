#!/bin/sh

# Run the test cases through the JAR
# This runs the same test cases as "sbt test", but displays the output and diagnostics instead of
# verifying them and just displaying a check mark
# Any options given to this script will be passed along to the interpreter, so you can use, for
# example, --debug or --print-core to run all test cases with that option.

sbt "set test in assembly := {}" clean assembly
for testcase in src/test/resources/de/uni_luebeck/isp/tessla/interpreter/tests/*.tessla
do
    echo $(basename $testcase)
    inputfile="$(dirname $testcase)/$(basename $testcase .tessla).input"
    java -jar target/scala-2.12/tessla.interpreter-assembly-*.jar "$@" "$testcase" "$inputfile"
    echo
done
