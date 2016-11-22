all: microcode.bin bootrom.vh

microcode.bin: microcode.org ../../scripts/microcoder.lisp
	sbcl --load ../../scripts/microcoder.lisp

bootrom.vh: ../../firmware/bootrom/tinystart.S \
		../../firmware/bootrom/tinystub.c \
		../../firmware/bootrom/handler.S
	moxie-elf-gcc -O2 -g -o bootrom.x $<
	moxie-elf-objcopy -O verilog bootrom.x bootrom.vh

clean:
	rm -rf bootrom.x *~
