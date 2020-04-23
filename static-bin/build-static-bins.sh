#! /usr/bin/env sh
set -e

IMG_NAME=bigrapher:static
CON_NAME=dummy
VERSION=1.9.3

cd ..
docker build --tag $IMG_NAME -f static-bin/Dockerfile.static .
docker create --name $CON_NAME $IMG_NAME
# libc static binaries
cd static-bin
docker cp $CON_NAME:/bin/bigrapher ./bigrapher
docker cp $CON_NAME:/bin/big_match ./big_match
tar -czf bigrapher-$VERSION+static.tar.gz bigrapher big_match
# musl static binaries
docker cp $CON_NAME:/bin/musl/bigrapher ./bigrapher
docker cp $CON_NAME:/bin/musl/big_match ./big_match
tar -czf bigrapher-$VERSION+static+musl.tar.gz bigrapher big_match
rm -f ./bigrapher ./big_match
docker rm -f $CON_NAME
