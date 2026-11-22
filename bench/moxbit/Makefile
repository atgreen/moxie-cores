# The verilated moxbit core and its tests.  See tb.cc.
#
# `make check' runs each asm/*.S program, and every bench/mox125
# program but the ones that need an MMU, on the core at several
# numbers of bus wait states and on tools/mox125-iss, and wants all of
# them to agree with the result on the program's first line.  tb is
# the core built with FAST_INC; each program runs once more on
# tb-small, the default build.  The asm/m*.S programs run on a core
# built with MDU = 0 too, with firmware/moxbit/mdu-emulate.S standing
# in for the multiplier and divider.

CORE = ../../cores/moxbit
ISS = ../../tools/mox125-iss/mox125-iss
WAITS = 0 1 2 5

MOX125_FILES := $(shell grep -L mmu.inc ../mox125/asm/test*.S)

ASM_FILES := $(wildcard asm/*.S) $(MOX125_FILES)
BIN_FILES := $(patsubst %.S,bin/%.x,$(notdir $(ASM_FILES)))

MDU_FILES := $(wildcard asm/m*.S)

all: tb tb-small tb-nomdu $(BIN_FILES)

vpath %.S asm ../mox125/asm

bin/%.x: %.S
	@mkdir -p bin
	moxie-elf-as -I ../mox125/asm -I asm -I ../../firmware/moxbit -o bin/$*.o $<
	moxie-elf-ld -o $@ bin/$*.o
	rm bin/$*.o

VSRC = $(CORE)/moxbit.v $(CORE)/moxbit_rf.v $(CORE)/moxbit_ucode.v

$(CORE)/moxbit_ucode.v: $(CORE)/microcode.org $(CORE)/ucoder.lisp
	$(MAKE) -C $(CORE)

obj_dir/Vmoxbit.mk: $(VSRC) $(CORE)/moxbit_ucode.vh tb.cc
	verilator -Wall -Wno-fatal --trace --cc -O3 -CFLAGS -O2 -GFAST_INC=1 \
	-I$(CORE) --top-module moxbit $(VSRC) \
	--exe $(CURDIR)/tb.cc

tb: obj_dir/Vmoxbit.mk $(VSRC) tb.cc
	$(MAKE) -C obj_dir -f Vmoxbit.mk
	cp obj_dir/Vmoxbit $@

# The default build, smallest.
obj_small/Vmoxbit.mk: $(VSRC) $(CORE)/moxbit_ucode.vh tb.cc
	verilator -Wno-fatal --cc -O3 -CFLAGS -O2 --Mdir obj_small \
	-I$(CORE) --top-module moxbit $(VSRC) --trace \
	--exe $(CURDIR)/tb.cc

tb-small: obj_small/Vmoxbit.mk $(VSRC) tb.cc
	$(MAKE) -C obj_small -f Vmoxbit.mk
	cp obj_small/Vmoxbit $@

# The same with no multiplier or divider.
obj_nomdu/Vmoxbit.mk: $(VSRC) $(CORE)/moxbit_ucode.vh tb.cc
	verilator -Wno-fatal --cc -O3 -CFLAGS -O2 -GMDU=0 --Mdir obj_nomdu \
	-I$(CORE) --top-module moxbit $(VSRC) --trace \
	--exe $(CURDIR)/tb.cc

tb-nomdu: obj_nomdu/Vmoxbit.mk $(VSRC) tb.cc
	$(MAKE) -C obj_nomdu -f Vmoxbit.mk
	cp obj_nomdu/Vmoxbit $@

# The same, printing each instruction for lockstep.sh.
obj_trace/Vmoxbit.mk: $(VSRC) $(CORE)/moxbit_ucode.vh tb.cc
	verilator -Wno-fatal --cc -O3 -CFLAGS -O2 -DMOXBIT_TRACE -GFAST_INC=1 --Mdir obj_trace \
	-I$(CORE) --top-module moxbit $(VSRC) --trace \
	--exe $(CURDIR)/tb.cc

tb-trace: obj_trace/Vmoxbit.mk $(VSRC) tb.cc
	$(MAKE) -C obj_trace -f Vmoxbit.mk
	cp obj_trace/Vmoxbit $@

$(ISS):
	$(MAKE) -C $(dir $(ISS))

check: all $(ISS)
	@pass=0; fail=0; \
	for s in $(ASM_FILES); do \
	  x=bin/$$(basename $$s .S).x; \
	  want=$$(head -1 $$s | sed 's/^# //' | tr a-f A-F); \
	  got=$$($(ISS) -r -n 1000000 $$x); \
	  if [ "$$want" = "$$got" ]; then pass=$$((pass+1)); \
	  else fail=$$((fail+1)); echo "$$x on the ISS: expecting $$want, got $$got"; fi; \
	  for w in $(WAITS); do \
	    got=$$(./tb -w $$w $$x 2>/dev/null); \
	    if [ "$$want" = "$$got" ]; then pass=$$((pass+1)); \
	    else fail=$$((fail+1)); echo "$$x with $$w wait states: expecting $$want, got $$got"; fi; \
	  done; \
	  got=$$(./tb-small -w 1 $$x 2>/dev/null); \
	  if [ "$$want" = "$$got" ]; then pass=$$((pass+1)); \
	  else fail=$$((fail+1)); echo "$$x on tb-small: expecting $$want, got $$got"; fi; \
	done; \
	for s in $(MDU_FILES); do \
	  x=bin/$$(basename $$s .S).x; \
	  want=$$(head -1 $$s | sed 's/^# //' | tr a-f A-F); \
	  got=$$(./tb-nomdu -n 50000000 $$x 2>/dev/null); \
	  if [ "$$want" = "$$got" ]; then pass=$$((pass+1)); \
	  else fail=$$((fail+1)); echo "$$x with MDU = 0: expecting $$want, got $$got"; fi; \
	done; \
	echo "$$pass passed, $$fail failed"; test $$fail -eq 0

# Compare every instruction of every program with the ISS: the
# registers and condition codes, not just the result.
lockstep: tb-trace $(BIN_FILES) $(ISS)
	@fail=0; \
	for x in $(BIN_FILES); do \
	  out=$$(./lockstep.sh $$x 2) || { fail=$$((fail+1)); echo "$$x: $$out"; }; \
	done; \
	echo "$(words $(BIN_FILES)) programs, $$fail differ"; test $$fail -eq 0

clean:
	-rm -rf obj_dir obj_trace obj_nomdu obj_small tb tb-trace tb-nomdu tb-small tb.vcd bin
