# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This repository contains open-source CPU cores and System-on-Chip (SoC) implementations for the Moxie processor architecture. Moxie is a bi-endian processor with support for various configurations and development boards.

## Architecture

### Cores
- **MoxieLite** (`cores/MoxieLite/`): A bi-endian non-pipelined implementation in VHDL
- **mox125** (`cores/mox125/`): A big-endian pipelined implementation in Verilog with 5-stage pipeline (fetch, decode, execute, write)
- **moxbit** (`cores/moxbit/`): A tiny, non-pipelined, bit-serial implementation in Verilog, modelled on SERV (~/git/serv); see `cores/moxbit/README.md`. The whole moxie ISA plus mox125's exceptions, eret, wfi and cas.l; no MMU. Microcoded: edit `cores/moxbit/microcode.org` and run `make` there (SBCL) to regenerate `moxbit_ucode.v`/`.vh`, which are checked in. It uses mox125's bus convention so it drops into the existing SoCs. Parameters: MDU (0 traps mul/div/mod for `firmware/moxbit/mdu-emulate.S`), FAST_INC. `bench/moxbit`: `make check` runs `asm/*.S` and every non-MMU `bench/mox125` program on several builds and wait states and on the ISS; `make lockstep` compares every instruction's registers with `mox125-iss -T`, and is the fastest way to find a bug. `cores/moxbit/synth.sh` gives the size. Verilator and the moxie toolchain may only be in a container locally (`localhost/moxie-ci-fedora`; toolchain built to `../toolchain`)
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
- Special registers (`gsr`/`ssr`): sr0 status (bit 0 interrupt enable, bits 8:4 condition codes), sr1 handler address, sr2 cause (0 divide by zero, 1 undefined instruction, 2 interrupt, 3 `swi`, 5 `brk`; these follow MoxieLite and `firmware/bootrom/tinystub.c`), sr3 `swi` number, sr4 status saved at exception entry, sr5 resume address, sr6/sr7 scratch.
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

**Instruction-level simulator**: `tools/mox125-iss` models the core an instruction at a time, with the same exception model, TLBs, instruction cache behaviour and simulated board as the RTL bench, plus a 16550 UART. It is hundreds of times faster than the verilated core and is meant for developing software too big to run there. The RTL is the reference: `make check` in that directory runs the bench's test programs on it and both must agree, so a change to the core's behaviour needs the same change there.

**Zephyr**: the port lives on the `moxie` branch of https://github.com/atgreen/zephyr (board `mox125_sim`, toolchain variant `cross-compile` with `CROSS_COMPILE=.../moxie-elf-`, `CONFIG_MINIMAL_LIBC=y`). Run an image with `cd bench/mox125 && sbcl --non-interactive --load zephyr.lisp <zephyr.elf> [max-clocks [trace-from]]`; `zephyr.lisp` is the board (RAM, console, interrupt controller, timer) and documents its register map. The Moxie ABI makes every caller reserve 24 bytes at the bottom of its frame for the callee to store its argument registers; hand-written assembly that calls C must do the same.

**Linux**: the port is `arch/moxie` on the `moxie` branch of https://github.com/atgreen/linux-moxie (`make ARCH=moxie CROSS_COMPILE=moxie-elf- mox125_sim_defconfig vmlinux`). The kernel is linked at 0xC0000000 and loaded at physical zero; its TLB miss handler maps RAM and the device region by arithmetic and walks a two-level page table for everything else. System calls are `swi 0` with the number in `$r12` and arguments in `$r0`-`$r5`. There is no C library yet: `tools/linux-init` is a freestanding `/init` and the initramfs list to point `CONFIG_INITRAMFS_SOURCE` at, and `user.ld` there is needed because the Moxie linker does not page-align segments. Run `vmlinux` with `tools/mox125-iss/mox125-iss vmlinux` (seconds) or on the RTL with `bench/mox125-board/board vmlinux` (about two minutes, 363M clocks, to boot and power off). `bench/mox125-board` is the same simulated board as `zephyr.lisp`, plus the 16550, driven from C++.

**Muskoka SoC under Verilator**: `cd soc/muskoka/verilator && make && ./muskoka` builds the on-chip bootloader (`firmware/bootrom`, linked with `soc/marin/moxie-marin-onchip.ld`) and runs it. It prints one trace line per clock (last field is `hex digits,tx`); the bootloader shows FEED on the hex display, prints its banner on the UART at 434 clocks per bit, then polls for input.

**Muskoka on the DE2**: `soc/muskoka/boards/de2` is a Quartus II 13.0sp1 project for the original DE2 (Cyclone II EP2C35); `make` builds `muskoka.sof` and `make program` loads it. Cyclone II has no RAM that can be read without a clock, so anything the core reads combinationally becomes flip-flops there: the board sets `ICACHE_SET_BITS` to 3 (an 8-line, 256-byte instruction cache; the core's default of 8 is 8 KiB) and runs the SoC at 12.5 MHz with the UART at 115200. Not yet built with Quartus or tried on a board.

**Muskoka on the Nexys 3**: `soc/muskoka/boards/nexys3` is an ISE 14.7 project for the Digilent Nexys 3 (Spartan-6 LX16): `make`, `make program`, `make timing`. `nexys3_soc.v` is everything but pins and clock, and has the simulators' memory map so their software runs unchanged: 16 MiB of cellular RAM at 0 (`cores/psram/psram_mox125_wb.v`), the simulated board's console, interrupt controller, timer and 16550 at 0xF0000000 (`cores/mox125_board`), and a boot ROM at 0xFFFF0000 where the core starts (`BOOT_ADDRESS` parameter). The boot loader (`firmware/mox125-boot`) checks the RAM, loads S-records from the serial port at 115200 and starts them; `g` restarts what is already in RAM. The core runs at 12.5 MHz with a 1 KiB instruction cache. `bench/nexys3-soc` runs the same SoC under Verilator with a strict model of the RAM and a terminal: `./sim -s program.srec`, or `./sim -l vmlinux -g` to skip the serial load. Linux needs the `mox125_nexys3` device tree (16 MiB, 12.5 MHz). Slaves that take more than one clock must notice the request changing under them, because the core can abandon an instruction fetch (see the PSRAM controller). An access to an unmapped address hangs the bus; there is no watchdog. Not yet built with ISE or tried on a board.

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

<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:46cd31e7 -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/core-concepts/sync-concepts.md for details and anti-patterns.

## Agent Context Profiles

The managed Beads block is task-tracking guidance, not permission to override repository, user, or orchestrator instructions.

- **Conservative (default)**: Use `bd` for task tracking. Do not run git commits, git pushes, or Dolt remote sync unless explicitly asked. At handoff, report changed files, validation, and suggested next commands.
- **Minimal**: Keep tool instruction files as pointers to `bd prime`; use the same conservative git policy unless active instructions say otherwise.
- **Team-maintainer**: Only when the repository explicitly opts in, agents may close beads, run quality gates, commit, and push as part of session close. A current "do not commit" or "do not push" instruction still wins.

## Session Completion

This protocol applies when ending a Beads implementation workflow. It is subordinate to explicit user, repository, and orchestrator instructions.

1. **File issues for remaining work** - Create beads for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **Handle git/sync by active profile**:
   ```bash
   # Conservative/minimal/default: report status and proposed commands; wait for approval.
   git status

   # Team-maintainer opt-in only, unless current instructions forbid it:
   git pull --rebase
   bd dolt push
   git push
   git status
   ```
5. **Hand off** - Summarize changes, validation, issue status, and any blocked sync/commit/push step

**Critical rules:**
- Explicit user or orchestrator instructions override this Beads block.
- Do not commit or push without clear authority from the active profile or the current user request.
- If a required sync or push is blocked, stop and report the exact command and error.
<!-- END BEADS INTEGRATION -->
