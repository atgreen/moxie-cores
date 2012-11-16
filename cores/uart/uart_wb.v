module uart_wb (input         rst_i,
		input 	      clk_i,
		output [15:0] wb_dat_o,
		input [15:0]  wb_dat_i,
		input [1:0]   wb_sel_i,
		input 	      wb_we_i,
		input 	      wb_cyc_i,
		input 	      wb_stb_i,
		output 	      wb_ack_o,
		input 	      rx_i,
		output 	      tx_o);

  wire 			      tx_fifo_full, tx_fifo_empty, uart_received;
  wire 			      uart_is_transmitting;
  wire [7:0] 		      tx_uart_byte;
  wire [7:0] 		      rx_uart_byte;
  wire [7:0] 		      rx_fifo_dout;
  wire 			      transmit;

  reg 				  ack = 1'b0;
  reg [15:0] 			  rx_dat = 16'd0;

  localparam [0:0]
    IDLE = 1'b0,
    STRB = 1'b1;
  reg transmit_state = IDLE;
  reg post_transmit = 1'b0;
  
  assign wb_ack_o = ack;
  assign wb_dat_o = rx_dat;
  assign transmit = (transmit_state == IDLE) & !(uart_is_transmitting | tx_fifo_empty);
   
  always @(posedge clk_i)
    begin
      ack <= wb_stb_i & wb_cyc_i;
      rx_dat <= rx_fifo_empty ? 16'hFFFF : { 8'd0, rx_fifo_dout };
      transmit_state <= STRB ? IDLE : (transmit ? STRB : IDLE);
      post_transmit <= transmit ? 1'b1 : 1'b0;
    end

  fifo_generator_v9_2 tx_fifo(.clk (clk_i),
			      .rst (rst_i),
			      .din (wb_dat_i[7:0]),
			      .wr_en (wb_stb_i & wb_cyc_i & wb_we_i & (transmit_state == IDLE)),
			      .rd_en (transmit),
			      .dout (tx_uart_byte),
			      .full (tx_fifo_full),
			      .empty (tx_fifo_empty));

  fifo_generator_v9_2 rx_fifo(.clk (clk_i),
			      .rst (rst_i),
			      .din (rx_uart_byte),
			      .wr_en (uart_received),
			      .rd_en (wb_stb_i & wb_cyc_i & !wb_we_i),
			      .dout (rx_fifo_dout),
			      .full (rx_fifo_full),
			      .empty (rx_fifo_empty));

   uart uart_instance (.clk (clk_i),
		       .rst (rst_i),
		       .rx (rx_i),
		       .tx (tx_o),
		       .transmit (post_transmit),
		       .tx_byte (tx_uart_byte),
		       .received (uart_received),
		       .rx_byte (rx_uart_byte),
		       .is_receiving (),
		       .is_transmitting (uart_is_transmitting));
  
endmodule // uart_wb
