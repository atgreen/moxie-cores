SOURCES = $(wildcard ../../../../cores/MoxieLite/*.vhd) \
	  $(wildcard ../../../../cores/MoxieLite/*.v) \
	  $(wildcard ../../../../cores/nexys7seg/*.v) \
	  $(wildcard ../../../../cores/wishbone/*.v) \
	  $(wildcard ../../rtl/*.v) \
	  clk_wiz_v3_6.v \
	  /opt/Xilinx/14.2/ISE_DS/ISE/verilog/src/iSE/unisim_comp.v

PROJECT = marin

all: $(PROJECT).bit

$(PROJECT).prj: $(SOURCES)
	rm -f $(PROJECT).prj
	for i in `echo $^`; do \
	  case $$i in \
	    *.vhd) \
		echo "vhdl work $$i" >> $@; \
		;; \
	    *.v) \
		echo "verilog work $$i" >> $@; \
		;; \
	  esac; \
	done;

$(PROJECT).bit: $(PROJECT).ncd $(PROJECT).ut
	bitgen -f $(PROJECT).ut $(PROJECT).ncd

$(PROJECT).ngd: $(PROJECT).ngc
	ngdbuild -dd . -nt timestamp -uc Nexys3_Master.ucf -p xc6slx16-csg324-3 $(PROJECT).ngc $(PROJECT).ngd

$(PROJECT).ngc $(PROJECT).syr: $(PROJECT).prj $(PROJECT).xst
	xst -ifn $(PROJECT).xst -ofn $(PROJECT).syr

$(PROJECT)_map.ncd $(PROJECT).pcf: $(PROJECT).ngd
	map -w -o $(PROJECT)_map.ncd $(PROJECT).ngd $(PROJECT).pcf

$(PROJECT).ncd: $(PROJECT)_map.ncd $(PROJECT).pcf
	par -w $(PROJECT)_map.ncd $(PROJECT).ncd $(PROJECT).pcf

clean: 
	-rm -rf *.prj *.ngd *.srp *~ *.lso *.xrpt xst _xmsgs
	-rm -rf *.map *.par *.syr *.log *.ncd *.twr *xprt *.lso
	-rm -rf *.bld xlnx_auto_0_xdb *.mrp *.ngm *.ngc *.xml 
	-rm -rf *.lst *.pcf *.bgn *.bit *.xwbt *.drc *.csv
	-rm -rf *.txt *.ptwx *.unroutes *.xpi *.html *.pad
	-rm -rf _impact.cmd *.ipf auto_project* *.xsl *.svf
