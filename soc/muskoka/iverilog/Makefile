MSOURCES = sim.v testram.v \
	$(wildcard ../rtl/*.v) \
	$(wildcard ../../../cores/mox125/*.v) \
	$(wildcard ../../../cores/mox125/*.h) \
	$(wildcard ../../../cores/wishbone/*.v) \
	$(wildcard ../../../cores/uart3/*.v) \
	$(wildcard ../../../cores/ram16bit/*.v) \
	$(wildcard ../../../cores/hex_display/*.v)

# 	$(wildcard ../../../cores/LVT-regs/*.v) \

PROJECT = muskoka

all: a.out bootrom.vh microcode.bin

microcode.bin:
	ln -s ../../../cores/mox125/microcode.bin .

bootrom.vh: ../../../firmware/bootrom/tinystart.S \
		../../../firmware/bootrom/tinystub.c \
		../../../firmware/bootrom/handler.S
	moxie-elf-gcc -mno-crt0 -O2 -g -o bootrom.x -Tiverilog-sim.ld $^
	# Adjust the loadable section addresses down by 1k so they will load
	# properly into bootrom16.v.  Use bootrom.elf for simulators like
	# qemu, and bootrom0.elf for input into the verilog code.
	moxie-elf-objcopy --change-section-lma .rodata-0x1000 \
		--change-section-lma .text-0x1000 \
		--change-section-lma .data-0x1000 \
		bootrom.x bootrom0.x
	moxie-elf-objcopy -O verilog bootrom0.x bootrom.vh

a.out: $(MSOURCES)
	iverilog -I../../../cores/mox125 \
		 -I../../../cores/LVT-regs \
		 -I../../../cores/uart16550/rtl/verilog $(MSOURCES)

clean: 
	-rm -f a.out *.vh *.x *.vcd *~ microcode*


