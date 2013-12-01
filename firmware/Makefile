all: tinystub.srec 

tinystub.srec: tinystub.elf
	moxie-elf-objcopy -O srec --srec-forceS3 tinystub.elf tinystub.srec

tinystub.elf: tinystub.c m.S marin.S
	moxie-elf-gcc -Wl,-Map,tinystub.map -g3 -Wl,--gc-sections -Os -Wall \
		-ffunction-sections tinystub.c handler.S tinystart.S \
		-T../soc/marin/moxie-marin.ld -o tinystub.elf -lnosys 

hello.srec: hello.elf
	moxie-elf-objcopy -O srec --srec-forceS3 hello.elf hello.srec

hello.elf: hello.c m.S marin.S
	moxie-elf-gcc -Wl,-Map,hello.map -g3 -Wl,--gc-sections -Os -ffunction-sections hello.c gdbstart.S -T../soc/marin/moxie-marin-test.ld -o hello.elf -lnosys 
