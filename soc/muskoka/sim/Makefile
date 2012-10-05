MSOURCES = sim.v testram.v \
	$(wildcard ../rtl/*.v) \
	$(wildcard ../../../cores/mox125/*.v) \
	$(wildcard ../../../cores/mox125/*.h) \
	$(wildcard ../../../cores/wishbone/*.v) \
	$(wildcard ../../../cores/LVT-regs/*.v) \
	$(wildcard ../../../cores/uart16550/rtl/verilog/*.v) 

PROJECT = muskoka

all: a.out

a.out: $(MSOURCES)
	iverilog -I../../../cores/mox125 \
		 -I../../../cores/LVT-regs \
		 -I../../../cores/uart16550/rtl/verilog $(MSOURCES)

clean: 
	-rm -f a.out *.vh *.x *.vcd *~ microcode*


