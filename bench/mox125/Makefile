VSRC = ../../cores/mox125/*.v 

bin/%.x: asm/%.S
	moxie-elf-as -o test.o $<
	moxie-elf-ld -o $@ test.o
	-rm test.o

ASM_FILES := $(wildcard asm/*.S)
BIN_FILES := $(patsubst asm/%.S,bin/%.x,$(ASM_FILES))

all: obj_dir/libmoxie.so microcode.bin bootrom.x $(BIN_FILES)

obj_dir/libmoxie.so: $(VSRC)
	verilator -Wno-fatal --cc -I../../cores/mox125 --top-module moxie $(VSRC)
	wrapilator moxie
	(cd obj_dir; M32=-fpic make -f Vmoxie.mk)
	(cd obj_dir; make -f Makefile.wrap)

microcode.bin: ../../cores/mox125/microcode.bin
	ln -s ../../cores/mox125/microcode.bin .

bootrom.x: ../../firmware/bootrom/tinystart.S \
		../../firmware/bootrom/tinystub.c \
                ../../firmware/bootrom/handler.S
	moxie-elf-gcc -mno-crt0 -O2 -g -o bootrom.x -T../../soc/marin/moxie-marin-onchip.ld -Wl,-verbose $^

clean:
	-rm -rf obj_dir *~ microcode.bin *.x *.o typescript bin/*

expected-results.lisp: collect-expected.sh $(ASM_FILES)
	./collect-expected.sh > expected-results.lisp

check: all expected-results.lisp
	sbcl --load test.lisp
