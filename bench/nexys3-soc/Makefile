# The Nexys 3 SoC under Verilator, with models of the board's RAM and
# a terminal on its serial port.  See sim.cc.

TOP = ../..
CORES = $(TOP)/cores
BOARD = $(TOP)/soc/muskoka/boards/nexys3
BOOT = $(TOP)/firmware/mox125-boot

VSRC = $(BOARD)/nexys3_soc.v \
	$(TOP)/soc/muskoka/rtl/bootrom16.v \
	$(wildcard $(CORES)/mox125/*.v) $(wildcard $(CORES)/mox125/*.h) \
	$(CORES)/mox125_board/mox125_board_wb.v \
	$(CORES)/psram/psram_mox125_wb.v \
	$(CORES)/wishbone/wb_intercon.v \
	$(CORES)/ram16bit/ram16bit_wb.v \
	$(CORES)/uart3/uart.v

all: sim microcode.bin bootrom.vh

# The core reads its microcode, and the ROM its contents, from the
# directory they are run in.
microcode.bin: $(CORES)/mox125/microcode.bin
	ln -sf $< .

bootrom.vh: $(BOOT)/boot.c $(BOOT)/start.S $(BOOT)/boot.ld
	$(MAKE) -C $(BOOT)
	cp $(BOOT)/bootrom.vh .

obj_dir/Vnexys3_soc.mk: $(VSRC) sim.cc
	verilator -Wno-fatal --trace --cc -O3 -CFLAGS -O2 \
	-I$(BOARD) -I$(CORES)/mox125 -I$(CORES)/mox125_board \
	-I$(CORES)/psram -I$(CORES)/wishbone -I$(CORES)/ram16bit \
	-I$(CORES)/uart3 -I$(TOP)/soc/muskoka/rtl \
	--top-module nexys3_soc $(BOARD)/nexys3_soc.v \
	--exe $(CURDIR)/sim.cc

# Verilator leaves its output alone when nothing in it would change,
# so depend on what the program is really built from as well.
sim: obj_dir/Vnexys3_soc.mk $(VSRC) sim.cc
	$(MAKE) -C obj_dir -f Vnexys3_soc.mk
	cp obj_dir/Vnexys3_soc $@

clean:
	-rm -rf obj_dir sim sim.vcd microcode.bin bootrom.vh
