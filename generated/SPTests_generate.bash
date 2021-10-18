#!/bin/bash

cd "$(dirname "$0")"/..

IN=`find sp/* |grep _Test.sp`

OUT=generated/Tests/



for i in $IN; do

     a=${i/sp\//src/}
     elm_src=${a/.sp/.elm}

     b=${i/sp\/Compiler\//generated/SPTests/}
     elm_out=${b/.sp/.elm}

     mkdir -p generated/SPTests

     echo $elm_src + $i = $elm_out

     cat $elm_src > $elm_out
     echo 'tests =' >> $elm_out
     python3 -c "print(open('$i').read().split('tests =')[1])" >> $elm_out

done

