VSRC = ../../../cores/mox125/*.v \
	../../../cores/hex_display/*.v \
	../../../cores/wishbone/*.v \
	../../../cores/uart3/*.v \
	../../../cores/ram16bit/*.v \
	../rtl/*.v

all: muskoka bootrom.vh

# The boot ROM is 4k at 0x1000, indexed from 0 by bootrom16.v, so
# shift the load addresses down to match.  The linker script already
# places .romdata relative to the start of the ROM.
bootrom.vh: ../../../firmware/bootrom/tinystart.S \
		../../../firmware/bootrom/tinystub.c \
		../../../firmware/bootrom/handler.S
	moxie-elf-gcc -mno-crt0 -Os -g -o bootrom.x -T../../marin/moxie-marin-onchip.ld $^
	moxie-elf-objcopy --change-section-lma .text-0x1000 \
		--change-section-lma .rodata-0x1000 \
		-O verilog bootrom.x bootrom.vh

obj_dir/Vmuskoka__ALL.a: $(VSRC)
	verilator -Wno-fatal --trace --cc \
	-I../../../cores/mox125 \
	-I../../../cores/hex_display \
	-I../../../cores/wishbone \
	-I../../../cores/uart3 \
	-I../../../cores/ram16bit \
	--top-module muskoka ../rtl/*.v
	(cd obj_dir; make -f Vmuskoka.mk)

VERILATOR_INC := $(shell verilator --getenv VERILATOR_ROOT)/include

muskoka: muskoka.cc obj_dir/Vmuskoka__ALL.a
	g++ -I$(VERILATOR_INC) -I$(VERILATOR_INC)/vltstd -Iobj_dir -o muskoka muskoka.cc obj_dir/libVmuskoka.a obj_dir/libverilated.a -lpthread

clean:
	-rm -rf obj_dir
	-rm muskoka *~
