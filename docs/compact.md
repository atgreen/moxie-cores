The compact-code extension
==========================

Moxie's instructions are 16 bits, but anything that needs a constant,
an address or a memory offset takes 4 or 6 bytes.  The compact-code
extension adds short forms of the instructions compilers use most for
those, a 3-operand add, compare-with-zero branches, a PC-relative call,
multiple-register push and pop, and a global pointer for reaching
variables, all chosen by counting what GCC generates for the
Embench-IoT benchmarks.  It uses only encodings that were undefined,
so existing programs run unchanged.

mox125, moxibit and tools/mox125-iss implement it; moxibit can be
built without it (COMPACT = 0).  MoxieLite does not implement it.


Code size
---------

The linked code (`.text`) of the 19 Embench-IoT programs at `-Os`,
each with libgcc and the same minimal C library, from
`tools/codesize/codesize.sh`.  Both compilers are GCC 15.3; RISC-V
gets its linker relaxation.

| Build                       | Bytes  | Geometric mean vs RV32IC |
|-----------------------------|-------:|------:|
| moxie                       | 92,284 | 1.267 |
| moxie `-mcompact`           | 70,176 | 0.946 |
| moxie `-mcompact -mgp`      | 65,092 | 0.898 |
| RV32I (SERV's default)      | 91,084 | 1.435 |
| RV32IC                      | 67,736 | 1     |
| RV32IM                      | 85,752 | 1.266 |
| RV32IMC                     | 64,842 | 0.901 |

With the extension, moxie code is 37% smaller than RV32I by geometric
mean, 10% smaller than RV32IC, and about the size of RV32IMC.  Moxie
has its multiplier and divider in every build; the RISC-V columns
without M call libgcc for them.


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
| 0x56      | `bsr label`             | 4 | call as `jsra` does, to the address of the offset plus twice the 16-bit offset |
| 0x57      | `pushm $rX, $rY`        | 2 | push $rX, $rX+1, ... $rY, as that many `push`es |
| 0x58      | `popm $rX, $rY`         | 2 | pop $rY, $rY-1, ... $rX, undoing a `pushm` |
| 0x5a-0x5b | `add3 $rA, $rC, $rB`    | 2 | $rA = $rC + $rB, all in $r0-$r7 |

The short `ldo.l` and `sto.l` have 3-bit register fields, bits 6-4
and 2-0, naming $r0-$r7, and a word offset of four bits: bits 9, 8, 7
and 3.  `add3` has the same register fields, and its third, $rC, is
bits 8, 7 and 3.

`pushm` and `popm` both step through the B field, so `pushm` keeps
$rX in B and $rY in A, and `popm` the other way round.  They take $r0-$r13, never
$fp or $sp.  Either one behaves as a sequence of `push`es or `pop`s,
except that a fault or interrupt part of the way through leaves $sp
and the registers as they were, so the instruction can be restarted.
`bsr` reaches 64K bytes either way.

Form 3 instructions are `11oooo vvvvvvvvvv`.  Five of the six free
conditions are used:

| Condition | Instruction        | Bytes | Does |
|-----------|--------------------|------:|------|
| 10        | `bra label`        | 2 | branch always, ±1K like the other branches |
| 11        | `beqz $rA, label`  | 2 | branch if $rA is zero, -64 to 62 bytes |
| 12-13     | `ldi.l $rA, imm7`  | 2 | $rA = imm7, -64 to 63 |
| 14        | `bnez $rA, label`  | 2 | branch if $rA is not zero |

The register is bits 7-4.  `ldi.l`'s value is bit 10 (the condition's
low bit), bits 9-8 and bits 3-0, sign-extended; the offset of `beqz`
and `bnez` is bits 9-8 and 3-0, sign-extended and doubled, from the
end of the instruction as for the other branches.

That leaves form 1 opcodes 0x59 and 0x5c-0x7f and form 3 condition 15
free.


Assembler
---------

`ashli`, `lshri`, `ashri`, `cmpi`, `bra`, `beqz`, `bnez`, `lea`,
`bsr`, `pushm`, `popm` and `add3` are always available.  `beqz` and
`bnez` take only labels that the assembler resolves itself, as compiled
code's are; `bsr` has a relocation, `R_MOXIE_PCREL16`.  The short `ldo.l`, `sto.l` and `ldi.l` have no names of
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
  assembler.  Prologues and epilogues save and restore runs of
  registers with `pushm` and `popm`, and calls are `bsr`.  It defines
  `__MOXIE_COMPACT__`, and selects libgcc's and newlib's `compact`
  multilib.
- `-mlong-calls` calls with `jsra` all the same, for programs whose
  calls reach further than `bsr`'s 64K bytes.
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
shifter, comparator and call sequence; the decoder grows, and runs
`pushm` and `popm` as a sequence of stores or loads.  moxibit grows
by about 20%: from 424 to 498 LUT4 on iCE40, or 382 to 448 with
MDU = 0.  Built with COMPACT = 0 it is its old size, and the
extension's instructions are undefined (cores/moxibit/README.md has
the details).


Testing
-------

bench/mox125/asm/test103.S checks the shifts, `cmpi` and `bra`;
test104.S the short loads, stores and constants, `lea` and `%gprel`;
and test105.S `beqz`, `bnez`, `add3`, `pushm`, `popm` and `bsr`, each
against values worked out by hand.  They run on mox125, moxibit and
the ISS.  bench/moxibit/asm/n000.S checks that a moxibit built with
COMPACT = 0 traps the extension's instructions.
