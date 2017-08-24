#!/bin/bash

# Run the test cases through the JAR
# This runs the same test cases as "sbt test", but displays the output and diagnostics instead of
# verifying them and just displaying a check mark
# Any options given to this script will be passed along to the interpreter, so you can use, for
# example, --debug or --print-core to run all test cases with that option.

export testPath=src/test/resources/de/uni_luebeck/isp/tessla/interpreter/tests/
function runTest {
  testcase=$1
  shift
  testcaseName=${testcase#$testPath}
  echo $testcaseName
  inputfile="$(dirname $testcase)/$(basename $testcase .tessla).input"
  java -jar target/scala-2.12/tessla.interpreter-assembly-*.jar "$@" "$testcase" "$inputfile"
  echo
}
export -f runTest

sbt "set test in assembly := {}" clean assembly
command='runTest "{}" '"$@"
find $testPath -name "*.tessla" -exec bash -c "$command" \;
