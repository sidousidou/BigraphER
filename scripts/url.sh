#!/usr/bin/env sh
#ARCH=$(head -n1 ./opam/url)
URL="http://hidden/arch/"
MD5=$(md5 -r $1 | cut -d ' ' -f 1)

echo 'archive: "'$URL$1'"'
echo 'checksum: "'$MD5'"'
