# xdl_boot.S
# 
# The bootrom starts execution at address 0x30000000, while the xdl
# utility is located at 0x30f00000.	
# This file can be linked in at address 0x30000000 to transfer control to the
# xdl.
# The linker script must place the section xdl_boot at address 0x30000000
	
	.section .xdl_boot, "ax", @progbits
	.p2align	1

	jmpa	0x30f00000
	
