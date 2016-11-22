MSOURCES = sim.v testram.v \
	$(wildcard ../rtl/*.v) \
	$(wildcard ../../../cores/mox125/*.v) \
	$(wildcard ../../../cores/mox125/*.h) \
	$(wildcard ../../../cores/wishbone/*.v) \
	$(wildcard ../../../cores/uart3/*.v) \
	$(wildcard ../../../cores/hex_display/*.v)

# 	$(wildcard ../../../cores/LVT-regs/*.v) \

PROJECT = muskoka

all: a.out

a.out: $(MSOURCES)
	iverilog -I../../../cores/mox125 \
		 -I../../../cores/LVT-regs \
		 -I../../../cores/uart16550/rtl/verilog $(MSOURCES)

clean: 
	-rm -f a.out *.vh *.x *.vcd *~ microcode*


