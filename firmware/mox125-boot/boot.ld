/* Link the mox125 boot loader: 4k of ROM at the top of the address
   space and 4k of RAM after it, clear of the RAM at address zero that
   programs are loaded into.  */

OUTPUT_FORMAT("elf32-bigmoxie")
OUTPUT_ARCH(moxie)
ENTRY(_start)

MEMORY
{
  ROM (rx)  : ORIGIN = 0xFFFF0000, LENGTH = 4k
  RAM (rwx) : ORIGIN = 0xFFFF1000, LENGTH = 4k
}

SECTIONS
{
  .text :
  {
    *(.text.boot)
    *(.text .text.*)
    *(.rodata .rodata.*)
  } > ROM

  /* Nothing copies initialized data out of the ROM, so there had
     better not be any.  */
  .data : { *(.data .data.*) } > RAM
  ASSERT (SIZEOF (.data) == 0, "the boot loader cannot have initialized data")

  .bss (NOLOAD) :
  {
    . = ALIGN(4);
    __bss_start__ = .;
    *(.bss .bss.*) *(COMMON)
    . = ALIGN(4);
    __bss_end__ = .;
  } > RAM

  _stack = ORIGIN(RAM) + LENGTH(RAM);

  /DISCARD/ : { *(.comment) }
}
