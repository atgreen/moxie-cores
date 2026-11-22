The compact-code extension
==========================

Moxie's instructions are 16 bits, but anything that needs a constant,
an address or a memory offset takes 4 or 6 bytes.  The compact-code
extension adds short forms of the instructions compilers use most for
those, chosen by counting what GCC generates for the Embench-IoT
benchmarks, and a global pointer for reaching variables.  It uses only
encodings that were undefined, so existing programs run unchanged.

mox125, moxibit and tools/mox125-iss implement it.  MoxieLite does
not.


Code size
---------

The linked code (`.text`) of the 19 Embench-IoT programs at `-Os`,
each with libgcc and the same minimal C library, from
`tools/codesize/codesize.sh`.  Both compilers are GCC 15.3; RISC-V
gets its linker relaxation.

| Build                       | Bytes  | Geometric mean vs RV32IC |
|-----------------------------|-------:|------:|
| moxie                       | 93,160 | 1.283 |
| moxie `-mcompact`           | 74,554 | 1.036 |
| moxie `-mcompact -mgp`      | 69,612 | 0.989 |
| RV32I (SERV's default)      | 91,084 | 1.435 |
| RV32IC                      | 67,736 | 1     |
| RV32IM                      | 85,752 | 1.266 |
| RV32IMC                     | 64,842 | 0.901 |

With the extension, moxie code is 31% smaller than RV32I by geometric
mean, and about the same size as RV32IC.  Most of what remains is
libgcc's software floating point in wikisort.


Instructions
------------

Form 1 instructions are `0ooooooo aaaabbbb`, with the opcode in bits
14-8 and the A and B fields in bits 7-4 and 3-0.  A 5-bit immediate
`imm5` is bit 8 (the opcode's low bit) above bits 3-0.

| Opcode    | Instruction             | Bytes | Does |
|-----------|-------------------------|------:|------|
| 0x40-0x41 | `ashli $rA, imm5`       | 2 | $rA <<= imm5 |
| 0x42-0x43 | `lshri $rA, imm5`       | 2 | $rA >>= imm5, logical |
| 0x44-0x45 | `ashri $rA, imm5`       | 2 | $rA >>= imm5, arithmetic |
| 0x46-0x47 | `cmpi $rA, imm5`        | 2 | compare $rA with imm5 sign-extended, as `cmp` does |
| 0x48-0x4b | `ldo.l $rA, off($rB)`   | 2 | `ldo.l` with $rA, $rB in $r0-$r7 and off 0-60 |
| 0x4c-0x4f | `sto.l off($rA), $rB`   | 2 | `sto.l` likewise |
| 0x50-0x51 | `ldo.l $rA, off($fp)`   | 2 | `ldo.l` with off = -4 × (imm5 + 1), -4 to -128 |
| 0x52-0x53 | `sto.l off($fp), $rA`   | 2 | `sto.l` likewise; the A field is the register stored |
| 0x54      | `ldi.l $rA, imm16`      | 4 | $rA = the 16-bit value that follows, sign-extended |
| 0x55      | `lea $rA, off16($rB)`   | 4 | $rA = $rB + the 16-bit offset that follows |

The short `ldo.l` and `sto.l` have 3-bit register fields, bits 6-4
and 2-0, naming $r0-$r7, and a word offset of four bits: bits 9, 8, 7
and 3.

Form 3 instructions are `11oooo vvvvvvvvvv`.  Two of the free
conditions are used:

| Condition | Instruction        | Bytes | Does |
|-----------|--------------------|------:|------|
| 10        | `bra label`        | 2 | branch always, ±1K like the other branches |
| 12-13     | `ldi.l $rA, imm7`  | 2 | $rA = imm7, -64 to 63 |

`ldi.l`'s register is bits 7-4, and its value is bit 10 (the
condition's low bit), bits 9-8 and bits 3-0, sign-extended.

That leaves form 1 opcodes 0x56-0x7f and form 3 conditions 11, 14 and
15 free.


Assembler
---------

`ashli`, `lshri`, `ashri`, `cmpi`, `bra` and `lea` are always
available.  The short `ldo.l`, `sto.l` and `ldi.l` have no names of
their own: with `-mcompact`, or after `.option compact`, the assembler
picks the shortest encoding of an `ldo.l`, `sto.l` or `ldi.l` whose
operands are constants that fit.  `.option nocompact` turns that off.

`%gprel(symbol)` is the offset of a symbol from `_gp`, for the 16-bit
offset of `ldo`, `sto` or `lea`: `ldo.l $r0, %gprel(counter)($r13)`.
Its relocation is `R_MOXIE_GPREL16`.  The linker defines `_gp` 32K
bytes into `.data`.


Compiler
--------

- `-mcompact` uses the new instructions, and passes `-mcompact` to the
  assembler.  It defines `__MOXIE_COMPACT__`, and selects libgcc's
  `compact` multilib.
- `-mgp` addresses writable variables with 16-bit offsets from `_gp`,
  which the startup code must load into $r13; data that is read-only,
  undefined and weak, or in a section of its own is still addressed
  absolutely.  `.data` and `.bss` must then fit in the first 64K bytes
  from the start of `.data`.  It defines `__MOXIE_GP__`.  $r13 is
  reserved for the execution environment (docs/abi.org), so this costs
  no register; interrupt handlers must leave it alone.

`scripts/build-toolchain.sh` builds a toolchain with both, from
tools/binutils-moxie-extensions.patch and
tools/gcc-moxie-extensions.patch.


What it costs
-------------

On mox125 the new instructions reuse its existing loads, stores,
shifter and comparator; the decoder grows.  moxibit grows from 468 to
509 LUT4 on ECP5, and from 416 to 492 with MDU = 0
(cores/moxibit/README.md has the details).


Testing
-------

bench/mox125/asm/test103.S checks the shifts, `cmpi` and `bra`, and
test104.S the short loads, stores and constants, `lea` and `%gprel`,
each against values worked out by hand.  Both run on mox125, moxibit
and the ISS.
