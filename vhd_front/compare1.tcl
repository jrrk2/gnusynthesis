read_vhdl -container r -libname WORK -2008 { /local/scratch/jrrk2/gnusynthesis/vhd_front/AXI_ADDRESS_CONTROL_CHANNEL.vhd } 
set_top r:/WORK/AXI_ADDRESS_CONTROL_CHANNEL 
read_sverilog -container i -libname WORK -12 { /local/scratch/jrrk2/gnusynthesis/vhd_front/AXI_ADDRESS_CONTROL_CHANNEL_edited.sv } 
set_top i:/WORK/AXI_ADDRESS_CONTROL_CHANNEL 
match 
verify 
#exit 
