// mox125_board_wb.v - The mox125_sim board's peripherals, in hardware.
//
// Copyright (c) 2026  Anthony Green
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

// The simulators (bench/mox125/zephyr.lisp, bench/mox125-board and
// tools/mox125-iss) give software a small board to run on.  This is
// the same board as one Wishbone slave, so that what runs there runs
// on an FPGA unchanged.  Offsets are from the slave's base, which the
// simulators have at 0xF0000000:
//
//   0x000  console: a byte written here is sent on the serial port.
//          The write does not complete until there is room for it.
//   0x020  interrupt controller: pending (read)
//   0x024  interrupt controller: enable
//   0x030  timer: control, write nonzero to start
//   0x034  timer: period, in clocks
//   0x038  timer: write to acknowledge
//   0x03C  timer: clocks since reset; reading the high half latches
//          the low half
//   0x040  16 bits for the board to display
//   0x100  16550 UART, registers four bytes apart
//
// Registers are 32 bits, big-endian, on the core's 16-bit bus, so the
// low half of one is at offset 2.
//
// The timer is interrupt source 0 and the UART source 1.
//
// The UART is the subset of a 16550 that a driver needs when it does
// not ask for FIFOs: data, interrupt enable for received data and
// transmitter empty, interrupt identification, and line status.  The
// line settings are fixed at 8N1 at the rate UART_DIVISOR gives, and
// whatever is written to the line control, modem control and divisor
// registers is remembered and otherwise ignored.  It shares its
// transmitter with the console register.

module mox125_board_wb #(// Clocks to the bit.
			 parameter UART_DIVISOR = 434
			 )(/*AUTOARG*/
   // Outputs
   wb_dat_o, wb_ack_o, irq_o, tx_o, display_o,
   // Inputs
   rst_i, clk_i, wb_adr_i, wb_dat_i, wb_sel_i, wb_we_i, wb_cyc_i,
   wb_stb_i, rx_i
   );

  input             rst_i, clk_i;

  // --- Wishbone -------------------------------------------------
  input [31:0]      wb_adr_i;
  input [15:0]      wb_dat_i;
  output reg [15:0] wb_dat_o;
  input [1:0]       wb_sel_i;
  input             wb_we_i;
  input             wb_cyc_i;
  input             wb_stb_i;
  output reg        wb_ack_o;

  // --- To the core, level sensitive -----------------------------
  output            irq_o;

  // --- Serial port ----------------------------------------------
  input             rx_i;
  output            tx_o;

  // --- Whatever the board has to show 16 bits on ----------------
  output reg [15:0] display_o;

  wire [8:0]        adr = wb_adr_i[8:0];
  wire              uart_sel = adr[8];
  wire [2:0]        uart_reg = adr[4:2];

  // The core holds a request until it is acknowledged, and takes an
  // acknowledge as answering whatever it has had on the bus since the
  // clock before.  So: act on a request once, on a clock when we are
  // not acknowledging, and acknowledge on the next.
  wire              request = wb_stb_i & wb_cyc_i & !wb_ack_o;

  // --- Serial port ----------------------------------------------
  wire              tx_busy;
  wire              rx_ready;
  wire [7:0]        rx_byte;

  reg [3:0]         ier;
  reg [7:0]         lcr, mcr, scr, dll, dlm;
  wire              dlab = lcr[7];

  // A console write waits for the transmitter to have room.  It goes
  // by how things stood a clock ago, since the transmitter's own busy
  // output already includes the write we are deciding on.
  reg               tx_busy_q;
  wire              console_sel = !uart_sel & (adr[7:1] == 7'h00);
  wire              console_write = request & wb_we_i & console_sel;
  wire              thr_write = (request & wb_we_i & uart_sel
				 & (uart_reg == 3'd0) & !dlab);
  wire              tx_write = (console_write & !tx_busy_q) | thr_write;

  wire              rbr_read = (request & !wb_we_i & uart_sel
				& (uart_reg == 3'd0) & !dlab);

  uart #(.UART_DIVISOR (UART_DIVISOR))
  serial (.clk_i (clk_i),
	  .rst_i (rst_i),
	  .rxd_i (rx_i),
	  .txd_o (tx_o),
	  .wr_i (tx_write),
	  .data_i (wb_dat_i[7:0]),
	  .rx_ready_o (rx_ready),
	  .data_o (rx_byte),
	  .rd_i (rbr_read),
	  .tx_busy_o (tx_busy));

  // The transmitter-empty interrupt is raised when the transmitter
  // runs dry or the interrupt is newly enabled, and dropped by
  // reading the identification register or writing another byte.
  reg               thr_irq;
  wire              rx_int = ier[0] & rx_ready;
  wire              thr_int = ier[1] & thr_irq;
  wire              ier_write = (request & wb_we_i & uart_sel
				 & (uart_reg == 3'd1) & !dlab);
  wire              iir_read = (request & !wb_we_i & uart_sel
				& (uart_reg == 3'd2));

  always @(posedge clk_i)
    if (rst_i) begin
      thr_irq <= 1'b0;
      tx_busy_q <= 1'b0;
    end else begin
      tx_busy_q <= tx_busy;
      if (thr_write)
	thr_irq <= 1'b0;
      else if (tx_busy_q & !tx_busy)
	thr_irq <= 1'b1;
      else if (ier_write & wb_dat_i[1] & !ier[1])
	thr_irq <= 1'b1;
      else if (iir_read & !rx_int & thr_int)
	thr_irq <= 1'b0;
    end

  reg [7:0]         uart_q;

  always @(*)
    case (uart_reg)
      3'd0: uart_q = dlab ? dll : rx_byte;
      3'd1: uart_q = dlab ? dlm : {4'b0, ier};
      3'd2: uart_q = rx_int ? 8'hc4 : (thr_int ? 8'hc2 : 8'hc1);
      3'd3: uart_q = lcr;
      3'd4: uart_q = mcr;
      3'd5: uart_q = {1'b0, !tx_busy, !tx_busy, 4'b0, rx_ready};
      3'd7: uart_q = scr;
      default: uart_q = 8'h00;
    endcase

  // --- Timer and interrupt controller ---------------------------
  reg [31:0]        count;
  reg [15:0]        count_latch;
  reg [31:0]        period;
  reg [31:0]        timer_next;
  reg               timer_running;
  reg               timer_pending;
  reg [1:0]         pic_enable;

  wire [1:0]        pic_pending = {rx_int | thr_int, timer_pending};

  assign irq_o = |(pic_pending & pic_enable);

  always @(posedge clk_i)
    if (rst_i) begin
      wb_ack_o <= 1'b0;
      wb_dat_o <= 16'h0000;
      count <= 32'd0;
      count_latch <= 16'd0;
      period <= 32'd0;
      timer_next <= 32'd0;
      timer_running <= 1'b0;
      timer_pending <= 1'b0;
      pic_enable <= 2'b00;
      display_o <= 16'h0000;
      ier <= 4'h0;
      lcr <= 8'h00;
      mcr <= 8'h00;
      scr <= 8'h00;
      dll <= 8'h00;
      dlm <= 8'h00;
    end else begin
      // A console write is the one thing that can be kept waiting.
      wb_ack_o <= request & !(console_write & tx_busy_q);

      count <= count + 32'd1;
      if (timer_running & (count == timer_next)) begin
	timer_pending <= 1'b1;
	timer_next <= timer_next + period;
      end

      if (request & !wb_we_i) begin
	if (uart_sel)
	  // A byte is read from the high lane.
	  wb_dat_o <= {uart_q, 8'h00};
	else
	  case (adr[7:1])
	    7'h11: wb_dat_o <= {14'b0, pic_pending};      // 0x22
	    7'h13: wb_dat_o <= {14'b0, pic_enable};       // 0x26
	    7'h1e: begin                                  // 0x3c
	      wb_dat_o <= count[31:16];
	      count_latch <= count[15:0];
	    end
	    7'h1f: wb_dat_o <= count_latch;               // 0x3e
	    7'h20: wb_dat_o <= display_o;                 // 0x40
	    default: wb_dat_o <= 16'h0000;
	  endcase
      end

      if (request & wb_we_i) begin
	if (uart_sel) begin
	  // A byte is written from the low lane.
	  case (uart_reg)
	    3'd0: if (dlab) dll <= wb_dat_i[7:0];
	    3'd1: if (dlab) dlm <= wb_dat_i[7:0];
	          else ier <= wb_dat_i[3:0];
	    3'd3: lcr <= wb_dat_i[7:0];
	    3'd4: mcr <= wb_dat_i[7:0];
	    3'd7: scr <= wb_dat_i[7:0];
	    default: ;
	  endcase
	end else
	  case (adr[7:1])
	    7'h13: pic_enable <= wb_dat_i[1:0];           // 0x26
	    7'h19: begin                                  // 0x32
	      timer_running <= |wb_dat_i;
	      timer_next <= count + period;
	    end
	    7'h1a: period[31:16] <= wb_dat_i;             // 0x34
	    7'h1b: period[15:0] <= wb_dat_i;              // 0x36
	    7'h1d: timer_pending <= 1'b0;                 // 0x3a
	    7'h20: display_o <= wb_dat_i;                 // 0x40
	    default: ;
	  endcase
      end
    end

endmodule // mox125_board_wb
