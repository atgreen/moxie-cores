all: microcode.bin bootrom.vh

microcode.bin: microcode.org ../../scripts/microcoder.lisp
	sbcl --load ../../scripts/microcoder.lisp

bootrom.vh: ../../firmware/bootrom/tinystart.S \
		../../firmware/bootrom/tinystub.c \
		../../firmware/bootrom/handler.S
	moxie-elf-gcc -mno-crt0 -Os -g -o bootrom.x -T../../soc/marin/moxie-marin.ld -Wl,-verbose $^
	moxie-elf-objcopy -O verilog bootrom.x bootrom.vh

clean:
	rm -rf bootrom.x bootrom.vh *~
