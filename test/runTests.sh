#!/bin/zsh

interpreter=../target/scala-2.13/tessla-compiler-assembly-0.1.0-SNAPSHOT.jar

bold=$(tput bold)
red=$(tput setaf 1)
green=$(tput setaf 2)
normal=$(tput sgr0)

#set -e

rm -rf errors
mkdir -p output
mkdir -p errors

while read tcName spec trace expected expectedRet
do

    java -jar $interpreter $spec > output/Main.scala 2> /dev/null
    cd output
    scalac Main.scala &> /dev/null

    scala Main < ../$trace &> out.trace
    retVal=$?
    cat out.trace | ../sort.py | sed -E 's/  */ /' > out.trace.sorted
    
    cp ../$expected expected.trace
    cat expected.trace | ../sort.py | sed -E 's/  */ /' > expected.trace.sorted

    if [[ $expectedRet -ne $retVal ]]; then
        echo $retVal > ../errors/${tcName}.unexprectedErrCode
    fi
    
    cmp --silent out.trace.sorted expected.trace.sorted
    cmpResult=$?
    
    if [[ $cmpResult -ne 0 ]]; then
        diff out.trace.sorted expected.trace.sorted > diff
        cp out.trace.sorted ../errors/${tcName}.trace
        cp expected.trace.sorted ../errors/${tcName}.expected
        mv diff ../errors/${tcName}.diff
    fi
    
    if [[ $cmpResult -ne 0 ]] || [[ $expectedRet -ne $retVal ]]; then
      echo -e $bold${red}Testcase ${tcName} ${spec} failed.$normal
    else
      echo -e $bold${green}Testcase ${tcName} ${spec} successfull.$normal
    fi
    
    rm -r *

    cd ..
    

done < Testcases.lst

 rm -r output
