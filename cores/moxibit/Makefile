# Regenerate moxibit's microcode ROMs after changing microcode.org.

all: moxibit_ucode.v

moxibit_ucode.v moxibit_ucode.vh: microcode.org ucoder.lisp
	sbcl --script ucoder.lisp
