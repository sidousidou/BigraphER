#! /usr/bin/env sh

IMG_NAME=bigrapher:static
CON_NAME=dummy
DEST=static-bin
VERSION=1.9.1

docker build --tag $IMG_NAME .
docker create --name $CON_NAME $IMG_NAME
mkdir -p $DEST
cd $DEST
docker cp $CON_NAME:/bin/bigrapher ./bigrapher
docker cp $CON_NAME:/bin/big_match ./big_match
docker rm -f $CON_NAME
tar -czf bigrapher-$VERSION+static.tar.gz bigrapher big_match
