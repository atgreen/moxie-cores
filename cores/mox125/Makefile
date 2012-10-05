all: microcode.bin bootrom.vh

microcode.bin: microcode.org ../../scripts/microcoder.lisp
	sbcl --load ../../scripts/microcoder.lisp

bootrom.vh: ../../firmware/bootrom.s
	moxie-elf-as -o bootrom.x $<
	moxie-elf-objcopy -O verilog bootrom.x bootrom.vh

clean:
	rm -rf bootrom.x *~