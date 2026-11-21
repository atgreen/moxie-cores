# The mox125 boot loader.  bootrom.vh is what the ROM in the FPGA is
# built from: its 4096 bytes in hex, one to a line.

CC = moxie-elf-gcc
# Address zero is RAM here, not a null pointer.
CFLAGS = -Os -Wall -ffreestanding -fno-builtin -fno-delete-null-pointer-checks -Wno-array-bounds

all: bootrom.vh

boot.elf: start.S boot.c boot.ld
	$(CC) $(CFLAGS) -nostdlib -Wl,-T,boot.ld -o $@ start.S boot.c -lgcc

bootrom.vh: boot.elf
	moxie-elf-objcopy -O binary --pad-to 0xFFFF1000 -j .text boot.elf boot.bin
	od -An -v -tx1 -w1 boot.bin | tr -d ' ' > $@

clean:
	rm -f boot.elf boot.bin bootrom.vh
