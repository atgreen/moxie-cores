VSRC = ../cores/mox125/moxie.v 

all: obj_dir/libmoxie.so

obj_dir/libmoxie.so: $(VSRC)
	verilator -Wno-fatal --cc -I../cores/mox125 --top-module moxie $(VSRC)
	wrapilator moxie
	(cd obj_dir; M32=-fpic make -f Vmoxie.mk)
	(cd obj_dir; make -f Makefile.wrap)

clean:
	-rm -rf obj_dir



