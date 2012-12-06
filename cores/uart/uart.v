`timescale 1ns / 1ns

//`define BAUD_RATE 9600

module uart(
	input clk,
	input rst,
	input rx,
	output reg tx = 1'b1,
	input transmit,
	input [7:0] tx_byte,
	output received,
	output reg [7:0] rx_byte,
	output is_receiving,
	output is_transmitting,
	output recv_error
);

/* (/ 50000000 (* 9600 2)) */
parameter RX_CLOCK_DIVIDE = 2604; /* clock rate (50Mhz) / (BAUD_RATE * 2) */
reg [12:0] rx_clk_divider = RX_CLOCK_DIVIDE;

parameter RX_IDLE          = 3'd0;
parameter RX_START         = 3'd1;
parameter RX_READ_BITS     = 3'd2;
parameter RX_STOP          = 3'd3;
parameter RX_DELAY_RESTART = 3'd4;
parameter RX_ERROR         = 3'd5;
parameter RX_RECEIVED      = 3'd6;
 
reg [2:0] rx_state = RX_IDLE;
reg [5:0] rx_countdown;
reg [3:0] rx_bits_remaining;

assign received     = rx_state == RX_RECEIVED;
assign recv_error   = rx_state == RX_ERROR;
assign is_receiving = rx_state != RX_IDLE;

always @(posedge clk) begin
	if(rst)
		rx_state <= RX_IDLE;
 
	if(rx_clk_divider)
		rx_clk_divider <= rx_clk_divider - 13'b1;
	else if(rx_countdown) begin
		rx_clk_divider <= RX_CLOCK_DIVIDE;
		rx_countdown <= rx_countdown - 6'b1;
	end

	/* Receive state machine */
	case(rx_state)
	RX_IDLE:
		if(!rx) begin
			rx_clk_divider <= RX_CLOCK_DIVIDE;
			rx_countdown <= 1;
			rx_state <= RX_START;
		end
	RX_START:
		if(!rx_countdown) begin
			if(!rx) begin
				rx_countdown <= 2;
				rx_bits_remaining <= 4'd8;
				rx_state <= RX_READ_BITS;
			end else
				rx_state <= RX_ERROR;
		end
	RX_READ_BITS:
		if(!rx_countdown) begin
			rx_byte <= {rx, rx_byte[7:1]};
			rx_countdown <= 2;
			rx_bits_remaining <= rx_bits_remaining - 4'b1;
			rx_state <= (rx_bits_remaining != 4'd1) ? RX_READ_BITS : RX_STOP;
		end
	RX_STOP:
		if(!rx_countdown)
			rx_state <= rx ? RX_RECEIVED : RX_ERROR;
	RX_DELAY_RESTART:
		if(!rx_countdown)
			rx_state <= RX_IDLE;
	RX_ERROR:
		begin
			rx_countdown <= 4;
			rx_state <= RX_DELAY_RESTART;
		end
	RX_RECEIVED:
		rx_state <= RX_IDLE;
	endcase
end

// Clock rate divided by 9600 
parameter TX_CLOCK_DIVIDE = 5208; /* (/ 50000000 9600) */
reg [14:0] tx_clk_divider = TX_CLOCK_DIVIDE;

parameter TX_IDLE          = 0;
parameter TX_SENDING       = 1;
parameter TX_DELAY_RESTART = 2;

reg [1:0] tx_state = TX_IDLE;
reg [2:0] tx_countdown;
reg [3:0] tx_bits_remaining;
reg [7:0] tx_data;
 
assign is_transmitting = tx_state != TX_IDLE;

always @(posedge clk) begin
	if(rst)
		tx_state <= TX_IDLE;

	if(tx_clk_divider)
		tx_clk_divider <= tx_clk_divider - 15'b1;
	else if(tx_countdown) begin
		tx_clk_divider <= TX_CLOCK_DIVIDE;
		tx_countdown <= tx_countdown - 3'b1;
	end
 
	case(tx_state)
	TX_IDLE:
		if(transmit) begin
			tx_data <= tx_byte;
			tx_clk_divider <= TX_CLOCK_DIVIDE;
			tx_countdown <= 1;
			tx <= 0;
			tx_bits_remaining <= 8;
			tx_state <= TX_SENDING;
		end
	TX_SENDING:
		if(!tx_countdown)
			if(tx_bits_remaining) begin
				tx_bits_remaining <= tx_bits_remaining - 1;
				tx <= tx_data[0];
				tx_data <= {1'b0, tx_data[7:1]};
				tx_countdown <= 1;
				tx_state <= TX_SENDING;
			end else begin
				tx <= 1;
				tx_countdown <= 2;
				tx_state <= TX_DELAY_RESTART;
			end
	TX_DELAY_RESTART:
		if(!tx_countdown)
			tx_state <= TX_IDLE;
	endcase
end

endmodule
