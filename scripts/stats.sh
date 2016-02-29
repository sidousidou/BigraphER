#! /usr/bin/env sh

COMMITS=$(git rev-list --all | wc -l)
LOC=$(git ls-files | xargs wc -lc | tail -n 1 | cut -w -f 2)

echo "BigraphER stats:"
echo "------------------"
echo "COMMITS:$COMMITS"
echo "LOC:         $LOC"
