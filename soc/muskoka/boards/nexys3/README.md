# mox125 on the Digilent Nexys 3

The mox125 core with 16 MB of RAM, a serial console, a timer and an
interrupt controller, enough to run Zephyr.  The memory map is the one
the mox125 simulators use, so what runs on them runs here.

**Status: this has run under Verilator (`bench/nexys3-soc`), where the
boot loader comes up and loads and runs Zephyr, and it has been built
with ISE 14.7 through FuseSoC (`fusesoc run --target=nexys3
moxielogic:moxie-cores:muskoka-nexys3`): 6868 of the 9112 LUTs and
2094 registers, with timing met (39.5 MHz for the 12.5 MHz core
clock).  It has not been run on a board.**  Expect to fix something the
first time.

ISE warns that the design uses 9K block RAMs (RAMB8BWER) whose
initialization data "may be incorrect" (Xilinx Answer Record 39999).
The boot ROM and mox125's microcode are initialized RAMs, so check
that before trusting a board.

## What you need

- Xilinx ISE 14.7 (WebPACK is enough for the XC6SLX16), with its
  `settings64.sh` sourced so that `xst`, `map`, `par` and `bitgen` are
  on the path.
- Digilent Adept utilities, for `djtgcfg`.
- Two micro-USB cables: one to **PROG**, which powers and programs the
  board (power jumper on USB), and one to **UART**, the console.

You do not need the Moxie toolchain to get started.  Without
`moxie-elf-gcc` the build uses the boot ROM in `prebuilt/`, and there
are two Zephyr images there to load.

## Build and load the FPGA

    make            # muskoka.bit; several minutes
    make timing     # how fast the result could be clocked
    make program    # load it into the board

Check that Adept sees the board first with `djtgcfg enum`.  If it has a
different name there than `Nexys3`, say `make program DEVICE=name`.

The four-digit display should show `b007`.  LED 7 is lit while the
clock generator is locked; LED 0 only while the centre button, which
is reset, is held.

## Talk to it

The UART port is 115200 baud, 8 data bits, no parity, one stop bit, no
flow control.  On Linux it usually appears as `/dev/ttyACM0` or
`/dev/ttyUSB0`:

    stty -F /dev/ttyACM0 115200 raw -echo
    cat /dev/ttyACM0 &

Press the centre button and you should see

    mox125 boot loader
    RAM: 16 MB ok
    Send S-records, or g to run what is at 0x1000.

LEDs 1 and 2 flicker with received and transmitted characters, which
helps tell a dead board from a wrong serial setting.

If the RAM line says anything else, the message says what was read
back and where; that is about how the FPGA is wired to the RAM, in
`muskoka_nexys3.v` and `muskoka_nexys3.ucf`, and is the most likely
thing to be wrong on first contact with the board.

## Run Zephyr

Send an image as it is:

    cat prebuilt/zephyr-hello.srec > /dev/ttyACM0

The boot loader answers each record with a `.` (or a `!` if it arrived
damaged, in which case it will not start the program), counts records
on the display, and starts the program after the last one:

    Starting at 00001000
    *** Booting Zephyr OS build ... ***
    Hello World! mox125_nexys3/mox125

`prebuilt/zephyr-sync.srec` is the synchronization sample: two threads
taking turns to print twice a second, which shows the timer and its
interrupt working.

Reset returns to the boot loader.  The program is still in RAM, and
typing `g` runs it again without loading.  `t` repeats the RAM check.

## Build your own Zephyr image

The port is the `moxie` branch of https://github.com/atgreen/zephyr and
the board is `mox125_nexys3`:

    export ZEPHYR_TOOLCHAIN_VARIANT=cross-compile
    export CROSS_COMPILE=/path/to/moxie-elf-
    west build -b mox125_nexys3 samples/hello_world -- -DCONFIG_MINIMAL_LIBC=y
    moxie-elf-objcopy -O srec --srec-forceS3 build/zephyr/zephyr.elf hello.srec

## What is where

| Address    | What                                                    |
|------------|---------------------------------------------------------|
| 0x00000000 | 16 MB of RAM, the board's cellular RAM                  |
| 0xF0000000 | console: a byte written here is sent                    |
| 0xF0000020 | interrupt controller: pending, enable                   |
| 0xF0000030 | timer: control, period, acknowledge, count              |
| 0xF0000040 | the four-digit display, 16 bits                         |
| 0xF0000100 | 16550-style UART, registers four bytes apart            |
| 0xFFFF0000 | 4k boot ROM, where the core starts                      |
| 0xFFFF1000 | 4k RAM for the boot loader                              |

`cores/mox125_board/mox125_board_wb.v` describes the peripherals.
Nothing answers at any other address, and an access to one hangs the
core until reset.

## Clock speed

The core runs at 12.5 MHz, which is deliberately slow: it has not been
through timing closure on this part.  To go faster, change
`CLOCK_DIVIDE` and `UART_DIVISOR` together at the top of
`muskoka_nexys3.v`, and the `clock-frequency` in the Zephyr board's
device tree to match, then see what `make timing` says.

## Simulating

`bench/nexys3-soc` runs `nexys3_soc.v`, which is everything here but
the pins and the clock generator, under Verilator, with a model of the
RAM chip and a terminal:

    cd bench/nexys3-soc
    make
    ./sim -n 60000000 -s ../../soc/muskoka/boards/nexys3/prebuilt/zephyr-hello.srec
