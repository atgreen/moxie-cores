MSOURCES = $(wildcard ../../../../cores/MoxieLite/*.vhd) \
	  $(wildcard ../../../../cores/MoxieLite/*.v) \
	  $(wildcard ../../../../cores/wishbone/*.v) \
	  ../../../../cores/uart3/uart.v \
	  ../../../../cores/uart3/uart_wb.v \
	  $(wildcard ../../../../cores/ram16bit/*.v) \
	  $(wildcard ../../../../cores/statled/rtl/*.v) \
	  $(wildcard ../../../../cores/hex_display/*.v) \
	  ../../../../cores/mpic/mpic.v \
	  ../../../../cores/mtimer/mtimer.v \
	  $(wildcard ../../rtl/*.v) \
	  pll.v

PROJECT = marin

ASSIGNMENT_FILES = $(PROJECT).qpf $(PROJECT).qsf

all: bootrom.vh smart.log $(PROJECT).asm.rpt

CFLAGS = -Os  -mno-crt0

BOOTROM_SOURCES = ../../../../firmware/marin.S ../../../../firmware/bootload.c ../../../../firmware/handler.S

bootrom.vh: $(BOOTROM_SOURCES) ../../moxie-marin-onchip.ld
	moxie-elf-gcc $(CFLAGS) -c -o marin.o ../../../../firmware/marin.S
	moxie-elf-gcc $(CFLAGS) -c -o bootload.o ../../../../firmware/bootload.c
	moxie-elf-gcc $(CFLAGS) -c -o handler.o ../../../../firmware/handler.S
	moxie-elf-gcc $(CFLAGS) marin.o bootload.o handler.o -o bootrom.elf -Wl,--gc-sections -T../../moxie-marin-onchip.ld -lnosys
	moxie-elf-objcopy --adjust-vma -0x1000 bootrom.elf
	/home/green/Xilinx/14.4/ISE_DS/ISE/bin/lin64/data2mem -bd bootrom.elf -d -o m bootrom.mem
	grep -v @ bootrom.mem > bootrom.vh

#bootrom.vh: ../../../../firmware/bootrom.s
#	moxie-elf-as -o bootrom.x ../../../../firmware/bootrom.s
#	moxie-elf-objcopy -O verilog bootrom.x bootrom.vh

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
	-rm -rf db incremental_db *.qpf *.qsf *.qdf
	-rm -rf *.ssf *.csf *.esf *.fsf *.psf *.quartus *.qws
	-rm -rf *.summary *.smsg greybox_tmp
	-rm -rf output_files bootrom* *.jdi *.o
