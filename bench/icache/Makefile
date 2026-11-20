VSRC = ../../cores/mox125/icache.v

# Makefile.wrap assumes /usr/share/verilator; point g++ at the real install.
VERILATOR_INC := $(shell verilator --getenv VERILATOR_ROOT)/include

all: obj_dir/libicache.so

obj_dir/libicache.so: $(VSRC)
	verilator -Wno-fatal --threads 1 --trace --cc -CFLAGS -fPIC -I../../cores/mox125 --top-module icache $(VSRC)
	wrapilator icache
	(cd obj_dir; make -f Vicache.mk)
	(cd obj_dir; CPATH=$(VERILATOR_INC):$(VERILATOR_INC)/vltstd make -f Makefile.wrap)

clean:
	-rm -rf obj_dir *~
