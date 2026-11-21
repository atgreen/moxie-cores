CFLAGS = -O2 -Wall

all: mox125-iss

mox125-iss: mox125-iss.c
	$(CC) $(CFLAGS) -o $@ $<

# Run the RTL bench's test programs here too.  They have to be built
# first, in bench/mox125.
BENCH = ../../bench/mox125

check: mox125-iss
	@pass=0; fail=0; \
	for s in $(BENCH)/asm/test*.S; do \
	  x=$(BENCH)/bin/$$(basename $$s .S).x; \
	  want=$$(head -1 $$s | sed 's/^# //' | tr a-f A-F); \
	  got=$$(./mox125-iss -r -n 1000000 $$x); \
	  if [ "$$want" = "$$got" ]; then pass=$$((pass+1)); \
	  else fail=$$((fail+1)); echo "$$(basename $$x): expecting $$want, got $$got"; fi; \
	done; \
	echo "$$pass passed, $$fail failed"; test $$fail -eq 0

clean:
	rm -f mox125-iss
