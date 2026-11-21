# The verilated mox125 core on the simulated board, driven from C++.
# See board.cc.

CORE = ../../cores/mox125

all: board microcode.bin

# The core reads its microcode from the directory it is run in.
microcode.bin: $(CORE)/microcode.bin
	ln -s $< .

obj_dir/Vmoxie.mk: $(CORE)/*.v $(CORE)/*.h board.cc
	verilator -Wno-fatal --trace --cc -O3 -CFLAGS -O2 \
	-I$(CORE) --top-module moxie $(CORE)/moxie.v \
	--exe $(CURDIR)/board.cc

board: obj_dir/Vmoxie.mk
	$(MAKE) -C obj_dir -f Vmoxie.mk
	cp obj_dir/Vmoxie $@

clean:
	-rm -rf obj_dir board board.vcd microcode.bin
