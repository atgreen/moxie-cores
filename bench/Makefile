VSRC = ../cores/mox125/icache.v 

all: obj_dir/libicache.so

obj_dir/libicache.so: $(VSRC)
	verilator -Wno-fatal --trace --cc -I../cores/mox125 --top-module icache $(VSRC)
	wrapilator icache
	(cd obj_dir; M32=-fpic make -f Vicache.mk)
	(cd obj_dir; make -f Makefile.wrap)

clean:
	-rm -rf obj_dir

