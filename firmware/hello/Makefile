# hello.x, an ELF program for the simulators, and hello.vh, the same
# for $readmemh into a 16-bit RAM from address zero (one word per
# line).

all: hello.x hello.vh

hello.x: hello.S
	moxie-elf-as -o hello.o $<
	moxie-elf-ld -o $@ hello.o
	rm hello.o

hello.vh: hello.x
	moxie-elf-objcopy -O binary hello.x hello.bin
	od -An -v -tx1 -w2 --skip-bytes=0 hello.bin | awk '{print $$1 $$2}' > $@.tmp
	(i=0; while [ $$i -lt 2048 ]; do echo 0000; i=$$((i+1)); done; cat $@.tmp) > $@
	rm -f hello.bin $@.tmp

clean:
	rm -f hello.x hello.vh hello.bin hello.o
