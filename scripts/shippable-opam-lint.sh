#!/bin/sh

TEST=../shippable/testresults
SRC=../tests/files/lint

echo "Checking OPAM files"

cd ./opam

OUT=$(opam lint 2>&1)

if echo $OUT | cmp -s -  $SRC/lint.reference &> /dev/null
then
    echo "OK"
    cp $SRC/passed.xml $TEST/opam-lint.xml
else
    sed "s/%MSG%/$OUT/g" $SRC/error.xml > $TEST/opam-lint.xml
fi
