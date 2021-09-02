
module DVL_MUX_33  (A,B,S,Y);

   input [32:0] A;
   input [32:0] B;
   input 	S;
   output [32:0] Y;

   assign Y = S ? B : A;
   
endmodule

