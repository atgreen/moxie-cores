#!/bin/sh

echo "(" 
grep "^#" asm/*.S | sed -e "s/S:# /x\" . \"/g" | sed -e "s/^asm/(\"bin/g" | sed -e "s/$/\")/g"
echo ")"

