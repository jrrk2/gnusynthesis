read_vhdl -container r -libname WORK -2008 { AXI_ADDRESS_CONTROL_CHANNEL.vhd AXI_master_transaction.vhd AXI_READ_DATA_CHANNEL.vhd AXI_WRITE_DATA_CHANNEL.vhd AXI_WRITE_DATA_RESPONSE_CHANNEL.vhd }
set_top r:/WORK/AXI_master 
read_sverilog -container i -libname WORK -12 { AXI_ADDRESS_CONTROL_CHANNEL_edited.sv AXI_master_edited.sv AXI_READ_DATA_CHANNEL_edited.sv AXI_WRITE_DATA_CHANNEL_edited.sv AXI_WRITE_DATA_RESPONSE_CHANNEL_edited.sv } 
set_top i:/WORK/AXI_master 
match 
verify 
#exit 
