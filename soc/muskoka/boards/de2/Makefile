# The Muskoka SoC for the Altera DE2 board (Cyclone II EP2C35).
#
# Needs Quartus II 13.0sp1, the last release to support Cyclone II.
# The moxie-elf toolchain rebuilds the boot ROM; without it the one in
# prebuilt/ is used.  FuseSoC can build it too: see muskoka-de2.core.
#
#   make           build muskoka.sof
#   make program   load it into the board over the USB-Blaster
#
# The bootloader talks on the RS-232 port at 115200 8N1.

TOP = ../../../..

MSOURCES = $(wildcard $(TOP)/cores/mox125/*.v) \
	$(wildcard $(TOP)/cores/mox125/*.h) \
	$(wildcard $(TOP)/cores/wishbone/*.v) \
	$(wildcard $(TOP)/cores/hex_display/*.v) \
	$(wildcard $(TOP)/cores/uart3/*.v) \
	$(wildcard $(TOP)/cores/ram16bit/*.v) \
	$(wildcard ../../rtl/*.v) \
	muskoka_de2.v pll.v config.v muskoka.sdc

PROJECT = muskoka

all: $(PROJECT).sof

program: $(PROJECT).sof
	quartus_pgm -z -m JTAG -o "p;$(PROJECT).sof"

# The boot ROM is 4k at 0x1000, indexed from 0 by bootrom16.v, so
# shift the load addresses down to match.  The linker script already
# places .romdata relative to the start of the ROM.
# Without the Moxie toolchain, use the boot ROM as it was last built
# and checked in.
bootrom.vh: $(TOP)/firmware/bootrom/tinystart.S \
		$(TOP)/firmware/bootrom/tinystub.c \
		$(TOP)/firmware/bootrom/handler.S prebuilt/bootrom.vh
	if command -v moxie-elf-gcc > /dev/null; then \
	  moxie-elf-gcc -mno-crt0 -Os -g -o bootrom.x \
	    -T../../../marin/moxie-marin-onchip.ld $(filter %.S %.c,$^) && \
	  moxie-elf-objcopy --change-section-lma .text-0x1000 \
	    --change-section-lma .rodata-0x1000 \
	    -O verilog bootrom.x bootrom.vh; \
	else \
	  cp prebuilt/bootrom.vh .; \
	fi

# The core reads its microcode from the directory it is built in.
microcode.bin: $(TOP)/cores/mox125/microcode.bin
	cp $< $@

$(PROJECT).qsf: setup_proj.tcl
	quartus_sh -t setup_proj.tcl

$(PROJECT).sof: $(PROJECT).qsf bootrom.vh microcode.bin $(MSOURCES)
	quartus_sh --flow compile $(PROJECT)

clean:
	-rm -f *.vh *.x *.vcd *~ microcode.bin
	-rm -rf *.rpt *.chg smart.log *.htm *.eqn *.pin *.sof *.pof
	-rm -rf db incremental_db *.qpf *.qsf *.done *.jdi *.sld
	-rm -rf *.summary *.smsg
