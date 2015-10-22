#! /usr/bin/env sh

REPS=10
CMD='time -p ./bigrapher.native full -q -M 2000 ./examples/savannah-general.big'
RES=0

for i in $(seq 1 $REPS); do
    TMP=`$CMD 2>&1 | head -n 1 | awk -F" " '{print $2}'`
    echo "Run $i: $TMP"
    RES=$(echo "$RES + $TMP" | bc -l)
done

AVG=$(echo "$RES / $REPS" | bc -l)
echo "Average time: $AVG"
