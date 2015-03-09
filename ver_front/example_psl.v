module example_psl(clk, out);
   
input clk;
output reg out;

   
// Using Verilog flavor.
psl default clock = (posedge clk);
// Then a simple assertion can be specified like:
psl a_with_implicit_clk : assert always {fifo_full |-> !wr_fifo};
// The above assertion will derive its clock from the default clock statement and is equivalent to the following:
psl a_with_explicit_clk : assert always {fifo_full |-> !wr_fifo} @(posedge clk);

// psl assert always {a;b} |-> {c;d};
   
psl ERRORwritefull: assert never {full && wr_en && wr_cs};
psl ERRORreadempty: assert never {empty && rd_en && rd_cs};
psl line_1	: assert always {! ((G (q | F G p) & G (r | F G  ! p)) | G q | G r)};
psl line_2	: assert always {! ((G (q | F G p) & G (r | F G  ! p)) | G q | G r) };
psl line_3	: assert always {! ((G (q | G F p) & G (r | G F  ! p)) | G q | G r)};
psl line_4	: assert always {! ((G (q | G F p) & G (r | G F  ! p)) | G q | G r) };
psl line_5	: assert always {! (F F p <-> F p)};
psl line_6	: assert always {! (F F p <-> F p) };
psl line_7	: assert always {! (G F p -> G F q)};
psl line_8	: assert always {! (G F p -> G F q) };
psl line_9	: assert always {! (G F p <-> G F q)};
psl line_10	: assert always {! (G F p <-> G F q) };
psl line_11	: assert always {! (G F p | F G q)};
psl line_12	: assert always {! (G F p | F G q) };
psl line_13	: assert always {! (p U (q U r))};
psl line_14	: assert always {! (p U (q U r)) };
psl line_15	: assert always {! G (p -> X(q V r))};
psl line_16	: assert always {! G (p -> X(q V r)) };
psl line_17	: assert always {(G (q | F G p) & G (r | F G  ! p)) | G q | G r};
psl line_18	: assert always {(G (q | F G p) & G (r | F G  ! p)) | G q | G r };
psl line_19	: assert always {(G (q | G F p) & G (r | G F  ! p)) | G q | G r};
psl line_20	: assert always {(G (q | G F p) & G (r | G F  ! p)) | G q | G r };
psl line_21	: assert always {(X p U X q) | ! X(p U q)};
psl line_22	: assert always {(X p U X q) | ! X(p U q) };
psl line_23	: assert always {(X p U q) | ! X(p U (p & q))};
psl line_24	: assert always {(X p U q) | ! X(p U (p & q)) };
psl line_25	: assert always {(X q & r) V X(((s U p) V r) U (s V r))};
psl line_26	: assert always {(X q & r) V X(((s U p) V r) U (s V r)) };
psl line_27	: assert always {(p U q) | (q U p)};
psl line_28	: assert always {(p U q) | (q U p) };
psl line_29	: assert always {F p & F  ! p};
psl line_30	: assert always {F p & F  ! p };
psl line_31	: assert always {F p U G q};
psl line_32	: assert always {F p U G q };
psl line_33	: assert always {G (F p & F q)};
psl line_34	: assert always {G (F p & F q) };
psl line_35	: assert always {G (p -> F q)};
psl line_36	: assert always {G (p -> F q) };
psl line_37	: assert always {G (p -> F q) & ((X p U X q) | ! X(p U q))};
psl line_38	: assert always {G (p -> F q) & ((X p U X q) | ! X(p U q)) };
psl line_39	: assert always {G (p -> F q) & ((X p U q) | ! X(p U (p & q)))};
psl line_40	: assert always {G (p -> F q) & ((X p U q) | ! X(p U (p & q))) };
psl line_41	: assert always {G (q | (X p & X ! p))};
psl line_42	: assert always {G (q | (X p & X ! p)) };
psl line_43	: assert always {G (q | X G p) & G (r | X G  ! p)};
psl line_44	: assert always {G (q | X G p) & G (r | X G  ! p) };
psl line_45	: assert always {G F p -> G F q};
psl line_46	: assert always {G F p -> G F q };
psl line_47	: assert always {G p U q};
psl line_48	: assert always {G p U q };
psl line_49	: assert always {p U (q U r)};
psl line_50	: assert always {p U (q U r) };
psl line_51	: assert always {p U q};
psl line_52	: assert always {p U q };
psl line_53	: assert always {p V (p | q)};
psl line_54	: assert always {p V (p | q) };

   /*
 psl property assert_s0_error : always (
   {G(q -> ((! p & ! r) U (r | ((p & ! r) U (r | ((! p & ! r) U (r | ((p & ! r) U (r | ((! p U r) | G ! p) | G p)))))))))});
*/
   
initial
     out = 0;

/*
psl property assert_s0_error : always {G(grant -> {request; tt*} <->-> tt)};
*/
 
endmodule // example_psl
