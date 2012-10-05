MSOURCES = $(wildcard ../../../../cores/muskoka/*.v) \
	$(wildcard ../../../../cores/wishbone/*.v) \
	$(wildcard ../../../../cores/hex_display/*.v)

PROJECT = muskoka

ASSIGNMENT_FILES = $(PROJECT).qpf $(PROJECT).qsf

all: bootrom.vh microcode.bin smart.log $(PROJECT).asm.rpt

bootrom.vh: ../../../../firmware/bootrom.s
	moxie-elf-as -o bootrom.x $<
	moxie-elf-objcopy -O verilog bootrom.x bootrom.vh

microcode.bin: ../../../../cores/mox125/microcode.org ../../../../scripts/microcoder.lisp
	cp ../../../../cores/mox125/microcode.org .
	sbcl --load ../../../../scripts/microcoder.lisp

map: smart.log $(PROJECT).map.rpt
fit: smart.log $(PROJECT).fit.rpt
asm: smart.log $(PROJECT).asm.rpt
smart: smart.log

MAP_ARGS = --family=CycloneII
FIT_ARGS = --part=EP2C35F672C6
ASM_ARGS = 

STAMP = echo done >

$(ASSIGNMENT_FILES).qsf: setup_proj.tcl
	quartus_sh -t setup_proj.tcl

$(PROJECT).map.rpt: map.chg $(MSOURCES)
	quartus_map $(MAP_ARGS) $(PROJECT)
	$(STAMP) fit.chg

$(PROJECT).fit.rpt: fit.chg $(PROJECT).map.rpt
	quartus_fit $(FIT_ARGS) $(PROJECT)
	$(STAMP) asm.chg

$(PROJECT).asm.rpt: asm.chg $(PROJECT).fit.rpt
	quartus_asm $(ASM_ARGS) $(PROJECT)

$(PROJECT).tan.rpt: tan.chg $(PROJECT).fit.rpt
	quartus_tan $(TAN_ARGS) $(PROJECT)

smart.log: $(ASSIGNMENT_FILES)
	quartus_sh --determine_smart_action $(PROJECT) > smart.log

################################################################
# Project initialization
################################################################
map.chg:
	$(STAMP) map.chg

fit.chg:
	$(STAMP) fit.chg

tan.chg:
	$(STAMP) tan.chg

asm.chg:
	$(STAMP) asm.chg

clean: 
	-rm -f a.out *.vh *.x *.vcd *~
	-rm -rf *.rpt *.chg smart.log *.htm *.eqn *.pin *.sof *.pof 
	-rm -rf db incremental_db *.qpf *.qsf muskoka.done
	-rm -rf *.ssf *.csf *.esf *.fsf *.psf *.quartus *.qws
	-rm -rf *.summary *.smsg


