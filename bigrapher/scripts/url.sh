#!/usr/bin/env sh

URL="http://www.dcs.gla.ac.uk/~michele/arch/"
MD5=$(md5 -r $1 | cut -d ' ' -f 1)

echo 'archive: "'$URL$1'"'
echo 'checksum: "'$MD5'"'
