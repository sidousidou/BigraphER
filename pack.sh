#!/bin/bash
mkdir pack/
cp _tags README pack/
cp *.mll pack/
cp *.mly pack/
cp *.mli *.ml pack/
cp -R match/ pack/
cd pack/
tar -czf ../bigrapher0.3.tar.gz *
cd ..
rm -R pack/
