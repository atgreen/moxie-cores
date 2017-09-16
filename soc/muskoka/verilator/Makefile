VSRC = ../../../cores/mox125/*.v \
	../../../cores/hex_display/*.v \
	../../../cores/wishbone/*.v \
	../../../cores/uart3/*.v \
	../../../cores/ram16bit/*.v \
	../rtl/*.v

all: muskoka

obj_dir/Vmuskoka__ALL.a: $(VSRC)
	verilator -Wno-fatal --trace --cc \
	-I../../../cores/mox125 \
	-I../../../cores/hex_display \
	-I../../../cores/wishbone \
	-I../../../cores/uart3 \
	-I../../../cores/ram16bit \
	--top-module muskoka ../rtl/*.v
	(cd obj_dir; make -f Vmuskoka.mk)

muskoka: muskoka.cc obj_dir/Vmuskoka__ALL.a 
	g++ -I/usr/share/verilator/include -I/usr/share/verilator/include/vltstd -Iobj_dir -o muskoka $^ -lveriuser

clean:
	-rm -rf obj_dir
	-rm muskoka *~
