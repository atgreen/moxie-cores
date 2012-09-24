// defines.v - Common definitions
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

`define OP_ADD_L  6'b000000
`define OP_AND    6'b000001
`define OP_ASHL   6'b000010
`define OP_ASHR   6'b000011
`define OP_BAD    6'b000100
`define OP_BEQ    6'b000101
`define OP_BGE    6'b000110
`define OP_BGEU   6'b000111
`define OP_BGT    6'b001000
`define OP_BGTU   6'b001001
`define OP_BLE    6'b001010
`define OP_BLEU   6'b001011
`define OP_BLT    6'b001100
`define OP_BLTU   6'b001101
`define OP_BNE    6'b001110
`define OP_BRK    6'b001111
`define OP_CMP    6'b010000
`define OP_DEC    6'b010001
`define OP_DIV_L  6'b010010
`define OP_GSR    6'b010011
`define OP_INC    6'b010100
`define OP_JMP    6'b010101
`define OP_JMPA   6'b010110
`define OP_JSR    6'b010111
`define OP_JSRA   6'b011000
`define OP_LDA_B  6'b011001
`define OP_LDA_L  6'b011010
`define OP_LDA_S  6'b011011
`define OP_LD_B   6'b011100
`define OP_LDI_B  6'b011101
`define OP_LDI_L  6'b011110
`define OP_LDI_S  6'b011111
`define OP_LD_L   6'b100000
`define OP_LDO_B  6'b100001
`define OP_LDO_L  6'b100010
`define OP_LDO_S  6'b100011
`define OP_LD_S   6'b100100
`define OP_LSHR   6'b100101
`define OP_MOD_L  6'b100110
`define OP_MOV    6'b100111
`define OP_MUL_L  6'b101000
`define OP_NEG    6'b101001
`define OP_NOP    6'b101010
`define OP_NOT    6'b101011
`define OP_OR     6'b101100
`define OP_POP    6'b101101
`define OP_PUSH   6'b101110
`define OP_RET    6'b101111
`define OP_SSR    6'b110000
`define OP_STA_B  6'b110001
`define OP_STA_L  6'b110010
`define OP_STA_S  6'b110011
`define OP_ST_B   6'b110100
`define OP_ST_L   6'b110101
`define OP_STO_B  6'b110110
`define OP_STO_L  6'b110111
`define OP_STO_S  6'b111000
`define OP_ST_S   6'b111001
`define OP_SUB_L  6'b111010
`define OP_SWI    6'b111011
`define OP_UDIV_L 6'b111100
`define OP_UMOD_L 6'b111101
`define OP_XOR    6'b111110

