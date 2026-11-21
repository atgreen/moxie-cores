# The Muskoka SoC for the Digilent Nexys 3 (Spartan-6 XC6SLX16).
#
# Needs Xilinx ISE 14.7, and Digilent's Adept utilities to load the
# board.  See README.md.
#
#   make           build muskoka.bit
#   make program   load it into the board
#   make timing    report how fast the result can be clocked
#
# The boot loader talks on the USB serial port at 115200 8N1.  The
# same design runs under Verilator in bench/nexys3-soc.

TOP = ../../../..
CORES = $(TOP)/cores
BOOT = $(TOP)/firmware/mox125-boot

SOURCES = muskoka_nexys3.v nexys3_soc.v \
	../../rtl/bootrom16.v \
	$(CORES)/mox125/moxie.v \
	$(CORES)/mox125/cpu_fetch.v \
	$(CORES)/mox125/cpu_decode.v \
	$(CORES)/mox125/cpu_execute.v \
	$(CORES)/mox125/cpu_registerfile.v \
	$(CORES)/mox125/cpu_write.v \
	$(CORES)/mox125/dcache.v \
	$(CORES)/mox125/icache.v \
	$(CORES)/mox125/microcode.v \
	$(CORES)/mox125/tlb.v \
	$(CORES)/mox125_board/mox125_board_wb.v \
	$(CORES)/psram/psram_mox125_wb.v \
	$(CORES)/wishbone/wb_intercon.v \
	$(CORES)/ram16bit/ram16bit_wb.v \
	$(CORES)/uart3/uart.v \
	$(CORES)/nexys7seg/nexys7seg.v

INCLUDES = config.v $(wildcard $(CORES)/mox125/*.h)

PROJECT = muskoka
PART = xc6slx16-csg324-3

all: $(PROJECT).bit

# The board's name as "djtgcfg enum" gives it.
DEVICE = Nexys3

program: $(PROJECT).bit
	djtgcfg prog -d $(DEVICE) -i 0 -f $<

timing: $(PROJECT).twr
	grep -A3 "Design statistics" $<

# The ROM reads its contents, and the core its microcode, from the
# directory they are built in.
# Without the Moxie toolchain, use the boot loader as it was last
# built and checked in.
bootrom.vh: $(BOOT)/boot.c $(BOOT)/start.S $(BOOT)/boot.ld prebuilt/bootrom.vh
	if command -v moxie-elf-gcc > /dev/null; then \
	  $(MAKE) -C $(BOOT) && cp $(BOOT)/bootrom.vh .; \
	else \
	  cp prebuilt/bootrom.vh .; \
	fi

microcode.bin: $(CORES)/mox125/microcode.bin
	cp $< $@

$(PROJECT).prj: Makefile
	rm -f $@
	for i in $(SOURCES); do echo "verilog work $$i" >> $@; done

$(PROJECT).ngc: $(PROJECT).prj $(PROJECT).xst bootrom.vh microcode.bin \
		$(SOURCES) $(INCLUDES)
	xst -ifn $(PROJECT).xst -ofn $(PROJECT).syr

$(PROJECT).ngd: $(PROJECT).ngc muskoka_nexys3.ucf
	ngdbuild -dd . -nt timestamp -uc muskoka_nexys3.ucf -p $(PART) \
		$(PROJECT).ngc $(PROJECT).ngd

$(PROJECT)_map.ncd: $(PROJECT).ngd
	map -w -o $(PROJECT)_map.ncd $(PROJECT).ngd $(PROJECT).pcf

$(PROJECT).ncd: $(PROJECT)_map.ncd
	par -w $(PROJECT)_map.ncd $(PROJECT).ncd $(PROJECT).pcf

$(PROJECT).twr: $(PROJECT).ncd
	trce -v 10 -o $@ $(PROJECT).ncd $(PROJECT).pcf

$(PROJECT).bit: $(PROJECT).ncd $(PROJECT).ut
	bitgen -f $(PROJECT).ut $(PROJECT).ncd

clean:
	-rm -rf *.prj *.ngd *.srp *~ *.lso *.xrpt xst _xmsgs
	-rm -rf *.map *.par *.syr *.log *.ncd *.twr *.twx *xprt
	-rm -rf *.bld xlnx_auto_0_xdb *.mrp *.ngm *.ngc *.xml
	-rm -rf *.lst *.pcf *.bgn *.bit *.xwbt *.drc *.csv
	-rm -rf *.txt *.ptwx *.unroutes *.xpi *.html *.pad
	-rm -rf bootrom.vh microcode.bin
