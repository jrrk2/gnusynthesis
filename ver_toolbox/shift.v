// write_verilog_all

module DVL_SH_L_8_8  (A,B,Y);
output [7:0] Y;
input [7:0] B;
input [7:0] A;

assign Y = (A << B);
   
endmodule

