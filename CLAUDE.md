# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This repository contains open-source CPU cores and System-on-Chip (SoC) implementations for the Moxie processor architecture. Moxie is a bi-endian processor with support for various configurations and development boards.

## Architecture

### Cores
- **MoxieLite** (`cores/MoxieLite/`): A bi-endian non-pipelined implementation in VHDL
- **mox125** (`cores/mox125/`): A big-endian pipelined implementation in Verilog with 5-stage pipeline (fetch, decode, execute, write)

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

## Current Work Session State (Aug 2025)

**Problem**: Register forwarding bug in mox125 processor affecting dual-register store operations (st.*, sto.*)

**Status**: 30/35 tests passing (85%) - CONFIRMED register forwarding bug via NOP insertion test

**Root Cause Identified**: 
- Complex register forwarding logic in `cores/mox125/moxie.v` lines 253-254 fails for dual-register operations
- ST_* operations require both regA_i (address) and regB_i (data) forwarding simultaneously  
- STA_* operations work (single register) but ST_* operations fail (dual register)

**Evidence**:
- Without NOPs: ST_* operations write 0x0 instead of expected register values
- With NOPs: ST_* operations work correctly (tests 004-006 now pass)
- Trace shows `W[3]@C0FFEE0: 6789` instead of `W[3]@C0FFEE0: 0` with NOPs

**Fixed So Far**:
- Added missing memory_address_o assignments to ST_*/STO_* operations in cpu_execute.v
- Fixed microcode.org for st.l (rB? 0→1) and regenerated microcode.bin
- Modified microcoder.lisp to use asdf instead of quicklisp

**Current State**:
- Tests 004-006 (st.s, st.b, st.l) now pass with NOPs
- Tests 007-009, 018, 027 still failing (likely STO_* operations)
- Need to fix forwarding logic in moxie.v without requiring NOPs

**Next Steps**:
1. Analyze complex forwarding logic in moxie.v lines 253-254
2. Fix simultaneous dual-register forwarding for ST_*/STO_* operations  
3. Target: restore to 31/35 tests passing (all but 4) as mentioned in commit 3319a10

**Files Modified**:
- cores/mox125/cpu_execute.v (memory_address_o assignments)
- cores/mox125/microcode.org (st.l microcode fix)  
- scripts/microcoder.lisp (asdf instead of quicklisp)
- bench/mox125/asm/test004.S, test005.S, test006.S (NOPs for validation - revert later)

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