mox125 core tests
-----------------

Each test performs some computation, and stores the result at
0x0c0ffee0.  The test ends with we write to 0xdead.

The expected result appears as a hexadecimal value in a comment on the
first line of the assembly file.

Here's a simple, but typical, example:

    # 6789
        .section .text
        .global _start
    _start:     
        ldi.s   $r0, 0x6789
        sta.s   0x0c0ffee0, $r0  // Save the result we are expecting
        sta.b   0xdead, $r0      // Exit the test
