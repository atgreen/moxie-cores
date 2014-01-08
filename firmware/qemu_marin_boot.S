# qemu_marin_boot.S
# 
# QEMU starts execution at address 0x1000, while RAM-based applications
# are linked to start at address 0x30000000
# This file can be linked in at address 0x1000 to transfer control to the
# RAM application
# The linker script must place the section qemu_marin_boot at address 0x1000
	
	.section .qemu_marin_boot, "ax", @progbits
	.p2align	1

	jmpa	0x30000000
	