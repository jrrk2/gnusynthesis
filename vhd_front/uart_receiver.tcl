set hdlin_unresolved_modules black_box
set hdlin_warn_on_mismatch_message "FMR_ELAB-115 FMR_ELAB-146 FMR_ELAB-147 FMR_ELAB-149"
read_vhdl -container r -libname WORK -2008 { uart_receiver.vhd slib_counter.vhd slib_mv_filter.vhd slib_input_filter.vhd } 
set_top r:/WORK/uart_receiver
read_sverilog -container i -libname WORK -12 { fpga-support/rtl/uart_sv/uart_receiver.sv fpga-support/rtl/uart_sv/slib_counter.sv fpga-support/rtl/uart_sv/slib_mv_filter.sv fpga-support/rtl/uart_sv/slib_input_filter.sv } 
set_top i:/WORK/uart_receiver
match 
verify 
quit
