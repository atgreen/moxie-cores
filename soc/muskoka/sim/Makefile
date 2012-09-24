MSOURCES = sim.v testram.v \
	$(wildcard ../rtl/*.v) \
	$(wildcard ../../../cores/mox125/*.v) \
	$(wildcard ../../../cores/wishbone/*.v) \
	$(wildcard ../../../cores/LVT-regs/*.v) \
	$(wildcard ../../../cores/uart16550/rtl/verilog/*.v) 

PROJECT = muskoka

all: a.out

microcode.bin: ../../../cores/mox125/microcode.org ../../../scripts/microcoder.lisp
	cp ../../../cores/mox125/microcode.org .
	sbcl --load ../../../scripts/microcoder.lisp

bootrom.vh: ../../../firmware/bootrom.s
	moxie-elf-as -o bootrom.x $<
	moxie-elf-objcopy -O verilog bootrom.x bootrom.vh

a.out: $(MSOURCES) bootrom.vh microcode.bin
	iverilog -I../../../cores/mox125 \
		 -I../../../cores/LVT-regs \
		 -I../../../cores/uart16550/rtl/verilog $(MSOURCES)

clean: 
	-rm -f a.out *.vh *.x *.vcd *~ microcode*


