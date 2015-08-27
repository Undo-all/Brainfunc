#!/bin/bash

out=$(basename $1 .bf)
echo $1 | ./bfc > "$out.c"
gcc "$out.c" -Ofast -o $out -w -g
rm "$out.c"

