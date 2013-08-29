SOURCES = $(wildcard ../../../../cores/mox125/*.v) \
	  $(wildcard ../../../../cores/wishbone/*.v) \
	  $(wildcard ../../../../cores/uart3/*.v) \
	  $(wildcard ../../../../cores/LVT-regs/*.v) \
	  ../../../../cores/hex_display/seg_7.v \
	  ../../../../cores/hex_display/hex_display.v \
	  $(wildcard ../../../../cores/uart16550/rtl/verilog/*.v) \
	  $(wildcard ../../rtl/*.v)

#	  /opt/Xilinx/14.2/ISE_DS/ISE/verilog/src/iSE/unisim_comp.v

PROJECT = muskoka

all: $(PROJECT).ngd

$(PROJECT).prj: $(SOURCES)
	rm -f $(PROJECT).prj
	for i in `echo $^`; do \
		echo "verilog work $$i" >> $@; \
	done;

$(PROJECT).ngd: $(PROJECT).prj $(PROJECT).xst
	xst -ifn $(PROJECT).xst

clean: 
	-rm -rf *.prj *.ngd *.srp *~ *.lso *.xrpt xst _xmsgs


