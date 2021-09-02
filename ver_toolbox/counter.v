module counter(
    input  CLK  /*  BIT */ ,
    input  RST  /*  BIT */ ,
    input  CLEAR  /*  BIT */ ,
    input  LOAD  /*  BIT */ ,
    input  ENABLE  /*  BIT */ ,
    input  DOWN  /*  BIT */ ,
    input [3:0] D  /*  ARNG(3:0) */ ,
    output [3:0] Q  /*  ARNG(3:0) */ ,
    output  OVERFLOW  /*  BIT */ );
    reg [4:0] iCounter  /*  ARNG(4:0) */ ;
    always @(posedge CLK,posedge RST)
        begin
        if (RST) 
            begin
            iCounter <= 32'h0;
            end
        else
            begin
            if (CLEAR == 1'h1) 
                begin
                iCounter <= 32'h0;
                end
            else
                begin
                if (LOAD == 1'h1) 
                    begin
                    iCounter <= D;
                    end
                else
                    begin
                    if (ENABLE == 1'h1) 
                        begin
                        if (DOWN == 1'h0) 
                            begin
                            iCounter <= (iCounter+32'h1);
                            end
                        else
                            begin
                            iCounter <= (iCounter-32'h1);
                            end
                        end
                    end
                end;
            if (iCounter[4] == 1'h1) 
                begin
                iCounter[4] <= 32'h0;
                end
            end
        end
    endmodule
