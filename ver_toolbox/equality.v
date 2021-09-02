// write_verilog_all

module DVL_GEQ_8_8  (A,B,Y);
output [0:0] Y;
input [7:0] B;
input [7:0] A;

assign Y = (A >= B);
   
endmodule

