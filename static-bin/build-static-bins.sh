#! /usr/bin/env sh
set -e

IMG_NAME=bigrapher:static
CON_NAME=dummy
# tags are vn.n.n
VERSION=$(echo $1 | cut -c 2-)
OUT=shippable/buildoutput

docker build --tag $IMG_NAME -f static-bin/Dockerfile.static .
docker create --name $CON_NAME $IMG_NAME
cd $OUT
docker cp $CON_NAME:/bin/musl/bigrapher ./bigrapher
docker cp $CON_NAME:/bin/musl/big_match ./big_match
tar -czfv bigrapher-$VERSION+static+musl.tar.gz bigrapher big_match
rm -f bigrapher big_match
docker rm -f $CON_NAME
