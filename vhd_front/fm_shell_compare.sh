cat > $$.tcl <<EOF
set hdlin_warn_on_mismatch_message "FMR_ELAB-115 FMR_ELAB-146 FMR_ELAB-147 FMR_ELAB-149"
read_vhdl -container r -libname WORK -2008 { $1.vhd } 
set_top r:/WORK/$1
read_sverilog -container i -libname WORK -12 { $1_tran_edited.sv } 
set_top i:/WORK/$1
match 
verify 
quit
EOF
fm_shell -f $$.tcl
rm -f $$.tcl
