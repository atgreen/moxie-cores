/*
 * The Marin SoC Linker Script for running is Cellular RAM
 *
 * Copyright (C) 2012 Anthony Green
 *
 * The authors hereby grant permission to use, copy, modify,
 * distribute, and license this software and its documentation for any
 * purpose, provided that existing copyright notices are retained in
 * all copies and that this notice is included verbatim in any
 * distributions. No written agreement, license, or royalty fee is
 * required for any of the authorized uses. Modifications to this
 * software may be copyrighted by their authors and need not follow
 * the licensing terms described here, provided that the new terms are
 * clearly indicated on the first page of each file where they apply.
 */

OUTPUT_FORMAT("elf32-bigmoxie", "elf32-bigmoxie",
              "elf32-littlemoxie")
OUTPUT_ARCH(moxie)
ENTRY(_start)

GROUP(-lc -lgcc)

MEMORY 
{
  RAM (rwx) : ORIGIN = 0x30000000, LENGTH = 16M  
  PORTS (rw) : ORIGIN = 0xF0000000, LENGTH = 1k
} 

SECTIONS
{
  .text : 
  {
    *(.text.boot)
    *(.text*)
    KEEP(*(.init))
    KEEP(*(.fini))
    . = ALIGN(0x4); 
  } > RAM

  _etext = .;
  PROVIDE (etext = .);

  .rodata :
  { 
    *(.rodata*) 
    *(.ctors) 
    *(.dtors)
    *(.eh_frame)
    *(.jcr)
    *(.tm_clone_table)
    . = ALIGN(0x4); 
  } > RAM

  .data :
  { 
    __data_start__ = .; 
    *(.data .data.* .gnu.linkonce.d.*) 
    . = ALIGN (4); 
    __data_end__ = .; 
  } > RAM

  .bss : 
  { 
    __bss_start__ = . ; 
    *(.bss .bss.* .gnu.linkonce.b.*) 
    *(COMMON)
    . = ALIGN (4); 
    __bss_end__ = .; 
  } > RAM

  .ramtop : 
  {
    __ram_top = 0x1000000 - 0x8000;
    _stack = __ram_top;
  } > RAM

  .ports : 
  {
    *(.ports)    
  } > PORTS
}
