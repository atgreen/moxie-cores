// gdbtarget.v - Hardware Implementation of the GDB Remote Protocol
//
// Copyright (c) 2012  Anthony Green.
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

module gdbtarget (/*AUTOARG*/
  // Outputs
  tx_o, debug_o, wb_dat_o, wb_adr_o, wb_we_o, wb_cyc_o, wb_stb_o,
  gdb_ctrl_o,
  // Inputs
  rst_i, clk_i, rx_i, wb_dat_i, wb_sel_o, wb_ack_i
  );
   
  // --- Clock and Reset ------------------------------------------
  input  rst_i, clk_i;

  // --- UART -----------------------------------------------------
  output tx_o;
  input rx_i;

  wire [7:0] rx_uart_byte;
  wire 	     rx_received;
  wire 	     rx_fifo_empty;
  wire       uart_is_transmitting;

  output [13:0] debug_o;

  assign debug_o = { packet_checksum, state[5:0], rx_fifo_empty };
  
  // --- Wishbone Bus Interconnect ------------------------------------
  input [31:0]  wb_dat_i;
  output [31:0] wb_dat_o;
  output [31:0] wb_adr_o;
  input [1:0]   wb_sel_o;
  output        wb_we_o;
  output        wb_cyc_o;
  output        wb_stb_o;
  input         wb_ack_i;

  // --- State machine states -----------------------------------------
  parameter GDB_IDLE = 6'd0;
  parameter GDB_READ_PACKET_START = 6'd1;
  parameter GDB_READ_PACKET = 6'd2;
  parameter GDB_READ_PACKET_WAIT = 6'd3;
  parameter GDB_READ_PACKET_CHECKSUM1_WAIT = 6'd4;
  parameter GDB_READ_PACKET_CHECKSUM1 = 6'd5;
  parameter GDB_READ_PACKET_CHECKSUM2_WAIT = 6'd6;
  parameter GDB_READ_PACKET_CHECKSUM2 = 6'd7;
  parameter GDB_PACKET_PARSE_LEN1 = 6'd8;
  parameter GDB_PACKET_CHECKSUM_ERROR = 6'd9;
  parameter GDB_PACKET_UNKNOWN_ERROR = 6'd10;
  parameter GDB_SEND_MESSAGE = 6'd11;
  parameter GDB_COMMAND_g = 6'd12;
  parameter GDB_DELAY = 6'd13;
  parameter GDB_SEND_MESSAGE_WAIT = 6'd14;
  parameter GDB_START = 6'd15;
  parameter GDB_DELAY1 = 6'd16;
  parameter GDB_PACKET_SEND_ACK = 6'd17;
  parameter GDB_PACKET_CHECK = 6'd18;
  parameter GDB_PACKET_PARSE = 6'd19;
  parameter GDB_PACKET_SEND_ACK_WAIT = 6'd20;
  parameter GDB_COMMAND_interrupt = 6'd21;
  parameter GDB_COMMAND_g_SEND_REGISTERS = 6'd22;
  parameter GDB_COMMAND_g_FETCH_HIGH = 6'd23;
  parameter GDB_COMMAND_g_FETCH_LOW = 6'd24;
  parameter GDB_COMMAND_g_SEND_WORD = 6'd25;
  parameter GDB_SEND_HEXBYTE = 6'd26;
  parameter GDB_PACKET_SEND_CHECKSUM1 = 6'd27;
  parameter GDB_COMMAND_g_SEND_HEXBYTE = 6'd28;
  parameter GDB_COMMAND_g_SEND_HEXBYTE2 = 6'd29;
  parameter GDB_SEND_HEXBYTE_WAIT = 6'd30;
  parameter GDB_SEND_HEXBYTE_WAIT_2 = 6'd30;
  parameter GDB_SEND_HEXBYTE_2 = 6'd31;
  parameter GDB_SEND_HEXBYTE_2_WAIT = 6'd32;

  parameter CHAR_hash = 8'd35;
  parameter CHAR_dollar = 8'd36;
  parameter CHAR_semi = 8'd59;
  parameter CHAR_g = 8'd103;
  
  reg [5:0] 	state = GDB_START;
  reg [5:0] 	delay_state;
  reg [5:0] 	state_stack[0:3];
  reg [1:0] 	sptr = 0;
  reg [7:0] 	rbuf[0:63];
  reg [5:0] 	rptr;
  reg 		rx_fifo_rd_en = 0;
  wire		received_hash;
  wire 		received_dollar;
  wire 		received_interrupt;
  reg [7:0] 	packet_checksum;
  wire [7:0] 	rx_byte;

  reg [7:0] 	msgbuf[0:14];
  reg [3:0] 	mptr;

  reg [7:0] 	uart_tx_byte;
  reg 		uart_transmit;

  reg [1:0] 	gdb_ctrl_o = 2'b0;
  output 	gdb_ctrl_o;
    
  initial
    begin
      $readmemh("msg.vh", msgbuf);
    end
  
  uart com (.clk (clk_i),
	    .rst (rst_i),
	    .rx (rx_i),
	    .tx (tx_o),
	    .transmit (uart_transmit),
	    .tx_byte (uart_tx_byte),
	    .received (rx_received),
	    .rx_byte (rx_uart_byte),
	    .is_receiving (),
	    .is_transmitting (uart_is_transmitting));

  fifo_generator_v9_2 rx_fifo(.clk (clk_i),
			      .rst (rst_i),
			      .din (rx_uart_byte),
			      .wr_en (rx_received),
			      .rd_en (rx_fifo_rd_en),
			      .dout (rx_byte),
			      .full (),
			      .empty (rx_fifo_empty));

  assign received_interrupt = (rx_byte == 8'd3);
  assign received_hash = (rx_byte == CHAR_hash);
  assign received_dollar = (rx_byte == CHAR_dollar);

  function [3:0] hex2num;
    input [7:0] char;
    case (char)
      8'd48:
	hex2num = 4'd0;
      8'd49:
	hex2num = 4'd1;
      8'd50:
	hex2num = 4'd2;
      8'd51:
	hex2num = 4'd3;
      8'd52:
	hex2num = 4'd4;
      8'd53:
	hex2num = 4'd5;
      8'd54:
	hex2num = 4'd6;
      8'd55:
	hex2num = 4'd7;
      8'd56:
	hex2num = 4'd8;
      8'd57:
	hex2num = 4'd9;
      8'd65:
	hex2num = 4'd10;
      8'd66:
	hex2num = 4'd11;
      8'd67:
	hex2num = 4'd12;
      8'd68:
	hex2num = 4'd13;
      8'd69:
	hex2num = 4'd14;
      8'd70:
	hex2num = 4'd15;
      8'd97:
	hex2num = 4'd10;
      8'd98:
	hex2num = 4'd11;
      8'd99:
	hex2num = 4'd12;
      8'd100:
	hex2num = 4'd13;
      8'd101:
	hex2num = 4'd14;
      8'd102:
	hex2num = 4'd15;
      default:
	hex2num = 4'd0; /* FIXME */
    endcase
  endfunction

  /**
   * Helper function to convert from nibble to ASCII representation
   */
  function [7:0] decode; //output is byte wide
    input [3:0] d_i;     //input is nibble wide
    begin 
      decode = { d_i > 4'h9 ? 4'h4 : 4'h3,
                 d_i == 4'h9 ? 1'b1 : 1'b0,
                 d_i[2:0] };
    end
  endfunction // decode

  reg 	       [24:0]		delay;
  reg [31:0] 			rval;
  reg [2:0] 			fcount;
  reg [7:0] 			tbyte;
  reg 				waitflag;
    
  always @(posedge clk_i) begin
    if (rst_i)
      begin
	state <= GDB_START;
	uart_transmit <= 0;
      end
    else
      begin
	case (state)
	  GDB_START:
	    begin
	      delay <= 25'd5000000;
	      state <= GDB_DELAY;
	      state_stack[0] <= GDB_IDLE;
	      sptr <= 0;
	    end
	  GDB_IDLE:
	    if (!rx_fifo_empty) begin
  	      rx_fifo_rd_en <= 1;
	      delay_state <= GDB_READ_PACKET_START;
	      state <= GDB_DELAY1;
	    end
	  GDB_READ_PACKET_START:
	    begin
              rx_fifo_rd_en <= 0;
	      if (received_dollar)
		begin
		  rptr <= 0;
		  packet_checksum <= 0;
		  state <= GDB_READ_PACKET_WAIT;
		end
	      else if (received_interrupt)
		state <= GDB_COMMAND_interrupt;
	      else 
		state <= GDB_IDLE;
	    end
	  GDB_READ_PACKET_WAIT:
	    if (!rx_fifo_empty) begin
	      rx_fifo_rd_en <= 1;
	      state <= GDB_DELAY1;
	      delay_state <= GDB_READ_PACKET;
	    end
	  GDB_READ_PACKET:
	    begin
	      rx_fifo_rd_en <= 0;
	      if (received_hash) 
		state <= GDB_READ_PACKET_CHECKSUM1_WAIT;
	      else begin
		rbuf[rptr] <= rx_byte;
		rptr <= rptr+1;
		packet_checksum <= packet_checksum + rx_byte;
		state <= GDB_READ_PACKET_WAIT;
	      end
	    end
	  GDB_READ_PACKET_CHECKSUM1_WAIT:
	    if (!rx_fifo_empty) begin
	      rx_fifo_rd_en <= 1;
	      state <= GDB_DELAY1;
	      delay_state <= GDB_READ_PACKET_CHECKSUM1;
	    end
	  GDB_READ_PACKET_CHECKSUM1:
	    begin
	      packet_checksum[7:4] <= packet_checksum[7:4] ^ hex2num(rx_byte);
	      rx_fifo_rd_en <= 0;
	      state <= GDB_READ_PACKET_CHECKSUM2_WAIT;
	    end
	  GDB_READ_PACKET_CHECKSUM2_WAIT:
	    if (!rx_fifo_empty) begin
	      rx_fifo_rd_en <= 1;
	      state <= GDB_DELAY1;
	      delay_state <= GDB_READ_PACKET_CHECKSUM2;
	    end
	  GDB_READ_PACKET_CHECKSUM2:
	    begin
	      packet_checksum[3:0] <= packet_checksum[3:0] ^ hex2num(rx_byte);
	      rx_fifo_rd_en <= 0;
	      state <= GDB_PACKET_CHECK;
	    end
	  GDB_PACKET_CHECK:
	    begin
	      if (packet_checksum == 8'd0)
		state <= GDB_PACKET_SEND_ACK;
	      else
		state <= GDB_PACKET_CHECKSUM_ERROR;
	    end
	  GDB_PACKET_SEND_ACK:
	    begin
	      if (! uart_is_transmitting) 
		begin
		  uart_tx_byte <= 8'h2B; // +
		  uart_transmit <= 1;
		  state <= GDB_PACKET_SEND_ACK_WAIT;
		end
	    end
	  GDB_PACKET_SEND_ACK_WAIT:
	    begin
	      uart_transmit <= 0;
	      delay <= 25'd111111111111111111111;
	      state_stack[sptr] <= GDB_PACKET_PARSE;
	      state <= GDB_DELAY;
	    end
	  GDB_PACKET_PARSE:
	    begin
	      case (rptr)
		2:
		  state <= GDB_PACKET_PARSE_LEN1;
		default:
		  state <= GDB_PACKET_UNKNOWN_ERROR;
	      endcase // case (rptr)
	    end
	  GDB_PACKET_PARSE_LEN1:
	    begin
	      case (rbuf[0])
		103: /* g */
		  state <= GDB_COMMAND_g;
		default:
		  state <= GDB_PACKET_UNKNOWN_ERROR;
	      endcase
	    end

	  // The 'g' command.  Send register dumps in hex format over the wire.
	  GDB_COMMAND_g:
	    begin
	      packet_checksum <= 0;
	      state <= GDB_COMMAND_g_SEND_REGISTERS;
	    end
	  GDB_COMMAND_g_SEND_REGISTERS:
	    begin
	      // TODO - if we've sent all regs, send checksum
	      gdb_ctrl_o <= 2'b11;
	      state <= GDB_COMMAND_g_FETCH_HIGH;
	    end
	  GDB_COMMAND_g_FETCH_HIGH:
	    begin
	      gdb_ctrl_o <= 2'b10;
	      rval[31:16] <= wb_dat_i;
	      state <= GDB_COMMAND_g_FETCH_LOW;
	    end
	  GDB_COMMAND_g_FETCH_LOW:
	    begin
	      rval[15:0] <= wb_dat_i;
	      state <= GDB_COMMAND_g_SEND_WORD;
	      fcount <= 3'b100;
	    end
	  GDB_COMMAND_g_SEND_WORD:
	    begin
	      if (fcount != 4'b0)
		begin
		  tbyte <= rval[31:24];
		  rval[31:8] <= rval[23:0];
		  state <= GDB_SEND_HEXBYTE;
		  fcount <= fcount - 1;
		  state_stack[sptr] <= GDB_COMMAND_g_SEND_WORD;
		end
	      state <= GDB_COMMAND_g_SEND_REGISTERS;
	    end	      

	  GDB_PACKET_SEND_CHECKSUM1:
	    if (! uart_is_transmitting)
	      begin
		uart_tx_byte <= decode (packet_checksum[7:4]);
		uart_transmit <= 1;
		state <= GDB_COMMAND_g_SEND_HEXBYTE;
	      end
	    else
	      state <= GDB_COMMAND_g_SEND_HEXBYTE2;

	  // Transmit a byte in two character ASCII hex format.
	  // Input: tbyte - byte to send
	  // Save return state in stack_stack[sptr].
	  GDB_SEND_HEXBYTE:
	    if (! uart_is_transmitting)
	      begin
		uart_tx_byte <= decode (tbyte[7:4]);
		packet_checksum <= packet_checksum + decode (tbyte[7:4]);
		uart_transmit <= 1;
		state <= GDB_SEND_HEXBYTE_WAIT;
		waitflag <= 0;
	      end
	  GDB_SEND_HEXBYTE_WAIT:
	    begin
	      waitflag <= ~waitflag;
	      uart_transmit <= 0;
	      if (waitflag)
		state <= GDB_SEND_HEXBYTE_2;
	    end
	  GDB_SEND_HEXBYTE_2:
	    if (! uart_is_transmitting)
	      begin
		uart_tx_byte <= decode (tbyte[3:0]);
		packet_checksum <= packet_checksum + decode (tbyte[3:0]);
		uart_transmit <= 1;
		state <= GDB_SEND_HEXBYTE_WAIT_2;
		waitflag <= 0;
	      end
	  GDB_SEND_HEXBYTE_2_WAIT:
	    begin
	      waitflag <= ~waitflag;
	      uart_transmit <= 0;
	      if (waitflag)
		state <= state_stack[sptr];
	    end
	    

	  GDB_COMMAND_interrupt:
	    begin
	      // Put the target core in debug mode.
	      gdb_ctrl_o <= 2'b10;
	      // Send a message.
	      mptr <= 7;
	      state <= GDB_SEND_MESSAGE;
	      state_stack[0] <= GDB_IDLE;
	      sptr <= 1;
	    end
	  GDB_PACKET_CHECKSUM_ERROR:
	    begin
	      mptr <= 6;
	      state <= GDB_SEND_MESSAGE;
	      state_stack[0] <= GDB_IDLE;
	      sptr <= 1;
	    end
	  GDB_PACKET_UNKNOWN_ERROR:
	    begin
	      mptr <= 2;
	      state <= GDB_SEND_MESSAGE;
	      state_stack[0] <= GDB_IDLE;
	      sptr <= 1;
	    end
	  GDB_SEND_MESSAGE:
	    if (msgbuf[mptr] == CHAR_semi) begin
	      state <= state_stack[sptr-1];
	      sptr <= sptr-1;
	    end else if (! uart_is_transmitting) 
	      begin
		uart_tx_byte <= msgbuf[mptr];
		uart_transmit <= 1;
		mptr <= mptr+1;
		state <= GDB_SEND_MESSAGE_WAIT;
	      end
	  GDB_SEND_MESSAGE_WAIT:
	    begin
	      uart_transmit <= 0;
	      delay <= 25'd10;
	      state_stack[sptr] <= GDB_SEND_MESSAGE;
	      state <= GDB_DELAY;
	    end
	  GDB_DELAY1:
	    begin
	      state <= delay_state;
	    end
	  GDB_DELAY:
	    begin
	      delay <= delay - 1;
	      if (delay == 0)
		state <= state_stack[sptr];
	    end
	  default:
	    state <= GDB_IDLE;
	endcase // case (state)
      end
  end // always @ (posedge clk_i)

endmodule // gdbtarget
