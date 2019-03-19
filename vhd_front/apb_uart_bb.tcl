set hdlin_unresolved_modules black_box
set hdlin_warn_on_mismatch_message "FMR_ELAB-115 FMR_ELAB-146 FMR_ELAB-147 FMR_ELAB-149"
read_vhdl -container r -libname WORK -2008 { apb_uart.vhd slib_clock_div.vhd slib_counter.vhd slib_edge_detect.vhd slib_fifo.vhd slib_input_filter.vhd slib_input_sync.vhd slib_mv_filter.vhd uart_baudgen.vhd uart_interrupt.vhd uart_receiver.vhd uart_transmitter.vhd }
set_black_box slib_clock_div
set_black_box slib_counter
set_black_box slib_edge_detect
set_black_box slib_fifo
set_black_box slib_input_filter
set_black_box slib_input_sync
set_black_box slib_mv_filter
set_black_box uart_baudgen
set_black_box uart_interrupt
set_black_box uart_receiver
set_black_box uart_transmitter
set_top r:/WORK/apb_uart
read_sverilog -container i -libname WORK -12 { fpga-support/rtl/uart_sv/apb_uart.sv fpga-support/rtl/uart_sv/slib_clock_div.sv fpga-support/rtl/uart_sv/slib_counter.sv fpga-support/rtl/uart_sv/slib_edge_detect.sv fpga-support/rtl/uart_sv/slib_fifo.sv fpga-support/rtl/uart_sv/slib_input_filter.sv fpga-support/rtl/uart_sv/slib_input_sync.sv fpga-support/rtl/uart_sv/slib_mv_filter.sv fpga-support/rtl/uart_sv/uart_baudgen.sv fpga-support/rtl/uart_sv/uart_interrupt.sv fpga-support/rtl/uart_sv/uart_receiver.sv fpga-support/rtl/uart_sv/uart_transmitter.sv }
set_black_box slib_clock_div
set_black_box slib_counter
set_black_box slib_edge_detect
set_black_box slib_fifo
set_black_box slib_input_filter
set_black_box slib_input_sync
set_black_box slib_mv_filter
set_black_box uart_baudgen
set_black_box uart_interrupt
set_black_box uart_receiver
set_black_box uart_transmitter
set_top i:/WORK/apb_uart
report_hdlin_mismatches
match 
verify
analyze_points -all
quit
