// defines.h - Common definitions
//
// Copyright (c) 2010, 2012 Anthony Green.  All Rights Reserved.
// DO NOT ALTER OR REMOVE COPYRIGHT NOTICES.
// 
// The above named program is free software; you can redistribute it
// and/or modify it under the terms of the GNU General Public License
// version 2 as published by the Free Software Foundation.
// 
// The above named program is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied warranty
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this work; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
// 02110-1301, USA.

`define PCB_WIDTH 6
`define PCB_WA    5
`define PCB_WB    4
`define PCB_RA    3
`define PCB_RB    2
`define PCB_WM    1
`define PCB_RM    0

`define OP_ADD_L  7'b0000000
`define OP_AND    7'b0000001
`define OP_ASHL   7'b0000010
`define OP_ASHR   7'b0000011
`define OP_BAD    7'b0000100
`define OP_BEQ    7'b0000101
`define OP_BGE    7'b0000110
`define OP_BGEU   7'b0000111
`define OP_BGT    7'b0001000
`define OP_BGTU   7'b0001001
`define OP_BLE    7'b0001010
`define OP_BLEU   7'b0001011
`define OP_BLT    7'b0001100
`define OP_BLTU   7'b0001101
`define OP_BNE    7'b0001110
`define OP_BRK    7'b0001111
`define OP_CMP    7'b0010000
`define OP_DEC    7'b0010001
`define OP_DIV_L  7'b0010010
`define OP_GSR    7'b0010011
`define OP_INC    7'b0010100
`define OP_JMP    7'b0010101
`define OP_JMPA   7'b0010110
`define OP_JSR    7'b0010111
`define OP_JSRA   7'b0011000
`define OP_LDA_B  7'b0011001
`define OP_LDA_L  7'b0011010
`define OP_LDA_S  7'b0011011
`define OP_LD_B   7'b0011100
`define OP_LDI_B  7'b0011101
`define OP_LDI_L  7'b0011110
`define OP_LDI_S  7'b0011111
`define OP_LD_L   7'b0100000
`define OP_LDO_B  7'b0100001
`define OP_LDO_L  7'b0100010
`define OP_LDO_S  7'b0100011
`define OP_LD_S   7'b0100100
`define OP_LSHR   7'b0100101
`define OP_MOD_L  7'b0100110
`define OP_MOV    7'b0100111
`define OP_MUL_L  7'b0101000
`define OP_NEG    7'b0101001
`define OP_NOP    7'b0101010
`define OP_NOT    7'b0101011
`define OP_OR     7'b0101100
`define OP_POP    7'b0101101
`define OP_PUSH   7'b0101110
`define OP_RET    7'b0101111
`define OP_SSR    7'b0110000
`define OP_STA_B  7'b0110001
`define OP_STA_L  7'b0110010
`define OP_STA_S  7'b0110011
`define OP_ST_B   7'b0110100
`define OP_ST_L   7'b0110101
`define OP_STO_B  7'b0110110
`define OP_STO_L  7'b0110111
`define OP_STO_S  7'b0111000
`define OP_ST_S   7'b0111001
`define OP_SUB_L  7'b0111010
`define OP_SWI    7'b0111011
`define OP_UDIV_L 7'b0111100
`define OP_UMOD_L 7'b0111101
`define OP_XOR    7'b0111110
`define OP_SEX_B  7'b0111111
`define OP_SEX_S  7'b1000000
`define OP_ZEX_B  7'b1000001
`define OP_ZEX_S  7'b1000011
