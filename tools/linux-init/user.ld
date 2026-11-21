/* Link a static Moxie Linux program.

   The kernel maps a program straight from its file, a page at a time,
   so each segment's address has to agree with its file offset modulo
   the page size.  The Moxie ELF linker has no notion of a page size,
   hence this script.  */

OUTPUT_FORMAT("elf32-bigmoxie")
ENTRY(_start)

PHDRS
{
  text PT_LOAD FILEHDR PHDRS FLAGS(5);
  data PT_LOAD FLAGS(6);
}

SECTIONS
{
  . = 0x10000 + SIZEOF_HEADERS;

  .text : { *(.text .text.*) } :text
  .rodata : { *(.rodata .rodata.*) } :text

  /* Start the data on a new page, at the same offset within it. */
  . = ALIGN(0x1000) + (. & 0xfff);

  .data : { *(.data .data.*) } :data
  .bss : { *(.bss .bss.*) *(COMMON) } :data

  /DISCARD/ : { *(.comment) }
}
