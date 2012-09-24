.text
	ldi.l	$sp, 0x4000000+4096
	ldi.l	$r0, 0x55555555
	mov	$r1, $sp
	mov	$r2, $r1
	ldi.l	$r0, 0x66666666
	mov	$r2, $sp
	nop
	mov	$r3, $r0
	mov	$r4, $r0
        ldi.l   $r5, fun+0x1000
#       jsr     $r5
loop:	push	$sp, $r0
	inc	$r0, 0x1 # Increment $r0
	inc	$r1, 0x1
	inc	$r2, 0x1
 	inc	$r3, 0x1 
	inc	$r4, 0x1
	nop
	sta.l	0x4000000, $r4
	lda.l   $r5, 0x4000000
	jmpa	loop+0x1000 # Offset hack
	nop
	nop
fun:    ldi.l   $r0, 0x55555555
        nop
        ret

  
	

	
	
