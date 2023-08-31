#!/bin/sh

for i in `find * |grep sp$`; do

    echo $i

    before=$(grep -e '#' $i |wc -l)

    ./build/unstable format $i >/dev/null

    after=$(grep -e '#' $i |wc -l)

    if [ $before != $after ]; then
        echo LOST COMMENTS!!! $before vs $after
    fi

done
