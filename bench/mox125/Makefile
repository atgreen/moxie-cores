VSRC = ../../cores/mox125/moxie.v 

all: obj_dir/libmoxie.so microcode.bin

obj_dir/libmoxie.so: $(VSRC)
	verilator -Wno-fatal --cc -I../../cores/mox125 --top-module moxie $(VSRC)
	wrapilator moxie
	(cd obj_dir; M32=-fpic make -f Vmoxie.mk)
	(cd obj_dir; make -f Makefile.wrap)

microcode.bin: ../../cores/mox125/microcode.bin
	ln -s ../../cores/mox125/microcode.bin .

clean:
	-rm -rf obj_dir *~ microcode.bin

