# A first user program for Linux on mox125, and the list that tells the
# kernel build how to wrap it in an initramfs.
CC = moxie-elf-gcc
CFLAGS = -O2 -Wall -ffreestanding -fno-builtin

all: init initramfs.list

init: init.c user.ld
	$(CC) $(CFLAGS) -nostdlib -static -Wl,-T,user.ld -o $@ $< -lgcc

initramfs.list: Makefile
	printf 'dir /dev 0755 0 0\nnod /dev/console 0600 0 0 c 5 1\nfile /init %s/init 0755 0 0\n' $(CURDIR) > $@

clean:
	rm -f init initramfs.list
