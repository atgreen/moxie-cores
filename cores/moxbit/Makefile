# Regenerate moxbit's microcode ROMs after changing microcode.org.

all: moxbit_ucode.v

moxbit_ucode.v moxbit_ucode.vh: microcode.org ucoder.lisp
	sbcl --script ucoder.lisp
