#!/usr/bin/env bash

for f in *_ctx.dot;
do
    base=${f//_ctx.dot}
    dot ${base}_ctx.dot ${base}_match.dot ${base}_param.dot ${base}_id.dot | gvpack -array_c4 | neato -n2 -s -Tsvg -o ${base}_decomp.svg
done
