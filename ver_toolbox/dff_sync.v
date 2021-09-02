// write_verilog_all

module DVL_DFF_SYNC_5  (CK,D,DEF,Q,RN);
input[31:0] DEF;
input[4:0] D;
input[0:0] CK;
output reg [4:0] Q;
input[0:0] RN;

always @(posedge CK or negedge RN)
  if (RN)
    Q <= 32'h0;
  else
    Q <= D;

endmodule

