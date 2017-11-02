#!/bin/sh

echo "(" 
grep "^#" asm/*.S | sed -e "s/:# /\" . \"/g" | sed -e "s/^asm/(\"asm/g" | sed -e "s/$/\")/g"
echo ")"

