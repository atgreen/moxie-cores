# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This repository contains open-source CPU cores and System-on-Chip (SoC) implementations for the Moxie processor architecture. Moxie is a bi-endian processor with support for various configurations and development boards.

## Architecture

### Cores
- **MoxieLite** (`cores/MoxieLite/`): A bi-endian non-pipelined implementation in VHDL
- **mox125** (`cores/mox125/`): A big-endian pipelined implementation in Verilog with 5-stage pipeline (fetch, decode, execute, write)
- Wishbone peripherals also live under `cores/`: `wishbone/` (interconnect, watchdog), `uart3/`, `ram16bit/`, `hex_display/`, `psram/`, `altera_sdram/`, `mtimer/`, `nexys7seg/`, `statled/`, `gdbtarget/`, `LVT-regs/`

### System-on-Chips
- **Marin** (`soc/marin/`): MoxieLite-based SoC for Spartan 6 (Nexys3) and Cyclone II (DE2) evaluation boards
- **Muskoka** (`soc/muskoka/`): mox125-based SoC for Cyclone II (Altera DE2) evaluation board

### Core Implementation Details

The mox125 core (`cores/mox125/moxie.v`) implements a 5-stage pipelined processor with:
- Wishbone bus interface (16-bit data, 32-bit address)
- Separate instruction and data memory interfaces with arbitration
- Register forwarding logic to handle pipeline hazards
- Memory stall handling for waitstates

## Build System and Commands

### Prerequisites
- **SBCL** (Steel Bank Common Lisp) with QuickLisp for build scripts
- **Verilator** for Verilog simulation and testing
- **moxie-elf-gcc** cross-compiler toolchain
- **Quartus** (for Altera FPGA builds) or **ISE/Vivado** (for Xilinx FPGA builds)

### Common Build Commands

**Build microcode and bootrom for mox125:**
```bash
cd cores/mox125
make all
```

**Build and test mox125 core:**
```bash
cd bench/mox125
make all          # Build Verilator simulation and test binaries
make check        # Run complete test suite
make clean        # Clean build artifacts
```

**Build icache benchmark:**
```bash
cd bench/icache
make all
sbcl --load test.lisp
```

**Build SoC for specific boards:**
```bash
# For Altera DE2
cd soc/marin/boards/de2
make

# For Xilinx Nexys3
cd soc/marin/boards/nexys3
make

# For Muskoka SoC with Verilator
cd soc/muskoka/verilator
make
```

**Run complete test suite:**
```bash
./scripts/run-tests.sh
```

### Testing
Tests are implemented in Common Lisp using the FiveAM testing framework. The main test suite:
- Verifies boot address behavior
- Runs NOP instruction sequences with variable memory wait states
- Loads and executes ELF binaries from `bench/mox125/asm/` directory
- Compares execution results against expected values

### Development Tools
The repository includes several Common Lisp scripts in `scripts/`:
- `microcoder.lisp`: Generates microcode from `microcode.org` files
- `msgmaker.lisp`: Creates message files
- `install.lisp`: Installation helper

### Memory Layout and Firmware
- Bootrom code in `firmware/bootrom/` (assembly and C)
- Linker scripts for different memory configurations in `soc/marin/`
- Memory maps documented in `soc/marin/memory-map.txt`

## mox125 Pipeline Notes (Nov 2026)

**Status**: `bench/mox125` passes: boot address, NOP sequence with 0-120 fetch wait states, and all `asm/test*.S` binaries at 0/1/2/5 bus wait states.

**Pipeline contract** (keep this true when changing the core):
- `cpu_execute` owns the data bus. Any instruction touching data memory runs as a sequence of 16-bit wishbone cycles (`mem_start` task, `SEQ_*` sequences) and holds `busy_o` high until the last ack. The 16-bit bus is a requirement.
- `busy_o` is the only stall. It freezes fetch, the decode latches, the register file read ports and the `maybe_forward_*` registers in `moxie.v`. Register writes for a multi-cycle instruction happen when its sequence completes.
- Register write port 1 writes register A (`opcode[7:4]`, or `[11:8]` for inc/dec), port 2 writes register B. `jsr`/`jsra`/`ret` override the indices to write `$sp` (1) and `$fp` (0).
- Forwarding in `moxie.v` compares what execute is actually writing against the operand indices of the instruction in decode; `$sp`/`$fp` are forwarded the same way.
- A taken branch raises `branch_flag_o` for one cycle, which redirects fetch and squashes the one instruction behind it.
- The SoC slaves register their acks (`ack <= stb & cyc`), so `moxie.v` only accepts an ack when the same request was already on the bus one clock earlier. This also makes it safe for the data side to take the bus from the icache mid-fill.
- Byte loads read the high lane (`sel=2'b10`, data on `[15:8]`); byte stores drive the low lane (`sel=2'b01`). This matches the memory model in `bench/mox125/test.lisp`.

**Exceptions and interrupts**:
- Special registers (`gsr`/`ssr`): sr0 status (bit 0 interrupt enable, bits 8:4 condition codes), sr1 handler address, sr2 cause (2 interrupt, 3 `swi`, 5 `brk`), sr3 `swi` number, sr4 status saved at exception entry, sr5 resume address, sr6/sr7 scratch.
- `irq_i` is level sensitive. An interrupt is taken in place of the instruction execute was about to accept, so sr5 is that instruction's address.
- Taking an exception copies sr0 to sr4, then clears the interrupt enable, leaves user mode, turns translation off and jumps to sr1. `eret` (opcode 0x3a) copies sr4 back to sr0 and jumps to sr5 as one step; it is the only way to enter user mode or turn translation on. `wfi` (0x3b) stops the core until `irq_i` is asserted, even with interrupts disabled. `cas.l ($rA), $rB` (0x3c) is an atomic compare and swap: if the word at `$rA` equals `$r0` it is replaced with `$rB`; either way `$r0` gets the old value and the equal condition says whether the write happened. The address must be word aligned. All three are mox125 extensions; `tools/binutils-moxie-extensions.patch` teaches the assembler about them.
- `gsr`, `ssr`, `eret` and `wfi` are privileged: in user mode they raise cause 12.

**Memory management** (software-refilled TLB, see the comments in `cpu_execute.v` and `tlb.v`):
- sr0 bit 2 is user mode and bit 3 turns translation on. sr8 fault address, sr9 tlbhi (virtual page number, address space id), sr10 tlblo (physical page number; flags valid 1, writable 2, user 8, global 16), sr11 write stores tlbhi/tlblo in the data TLB (bit 0 clear) or instruction TLB (bit 0 set), sr12 current address space id, sr13 TLB miss handler (zero means use sr1), sr14 write flushes the instruction cache.
- Each TLB is direct mapped with 64 entries and 4 KiB pages; the low six bits of the virtual page number pick the entry. Entries are not cleared at reset, so software must write all of them before enabling translation.
- Causes: 6 instruction TLB miss, 7/8 data TLB miss on load/store, 9 instruction protection, 10/11 data protection on load/store. A fault puts the address in sr8 and preloads tlbhi.
- Faults are precise: a multi-cycle instruction that faults has changed no register and is restarted from the beginning.
- The instruction cache is tagged with the virtual address plus the translation and user-mode bits. Changing a mapping does not invalidate it; software must flush.
- Translation is combinational between the bus sequencer and the bus, which roughly doubled the longest path through the core. Registering it is a known follow-up.

**Zephyr**: the port lives on the `moxie` branch of https://github.com/atgreen/zephyr (board `mox125_sim`, toolchain variant `cross-compile` with `CROSS_COMPILE=.../moxie-elf-`, `CONFIG_MINIMAL_LIBC=y`). Run an image with `cd bench/mox125 && sbcl --non-interactive --load zephyr.lisp <zephyr.elf> [max-clocks [trace-from]]`; `zephyr.lisp` is the board (RAM, console, interrupt controller, timer) and documents its register map. The Moxie ABI makes every caller reserve 24 bytes at the bottom of its frame for the callee to store its argument registers; hand-written assembly that calls C must do the same.

**Muskoka SoC under Verilator**: `cd soc/muskoka/verilator && make && ./muskoka` builds the on-chip bootloader (`firmware/bootrom`, linked with `soc/marin/moxie-marin-onchip.ld`) and runs it. It prints one trace line per clock (last field is `hex digits,tx`); the bootloader shows FEED on the hex display, prints its banner on the UART at 434 clocks per bit, then polls for input.

**Bench notes**:
- Needs Verilator 5 and wrapilator from git master (the v1.1.0 release predates Verilator 5).
- Run `ocicl install` in `bench/mox125` and `bench/icache` to fetch the Lisp dependencies.
- Each test stores its result at 0x0c0ffee0 and exits by writing to 0xdead; the expected value is the comment on the first line of the `.S` file.

## File Structure Notes

- `bench/`: Contains performance benchmarks and test suites using Verilator
- `cores/`: Core processor implementations (VHDL and Verilog)
- `soc/`: System-on-Chip implementations with board-specific configurations
- `firmware/`: Boot ROM and low-level firmware code
- `tools/`: Build scripts for development toolchain setup
- `docs/`: Documentation including ABI specification

## Hardware Testing

Board programming procedures are documented in the HACKING file:
- DE2 boards use Quartus programming with `.sof` files
- Nexys3 boards have specific Makefile targets for programming
- Serial console connection at 9600 N81 for bootloader interaction