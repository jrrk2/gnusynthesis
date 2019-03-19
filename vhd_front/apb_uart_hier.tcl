###
### Formality (R) hierarchical verification script: /local/scratch/jrrk2/gnusynthesis/vhd_front/apb_uart_hier.tcl
###
### Reference design: r:/WORK/apb_uart
### Implementation design: i:/WORK/apb_uart
### Generated Thu Mar 14 16:55:56 2019
###

global ref
global impl
global verification_constant_prop_mode
global signature_analysis_match_blackbox_input
global signature_analysis_match_blackbox_output
global signature_analysis_match_primary_input
global signature_analysis_match_primary_output
global verification_status
global fm_tmp_result_count
global fm_hier_result_array

redirect         /local/scratch/jrrk2/gnusynthesis/vhd_front/fm_apb_uart_hier.log {echo "**************************************************************************************************"}
redirect -append /local/scratch/jrrk2/gnusynthesis/vhd_front/fm_apb_uart_hier.log {echo "Results of hierarchical verification script: /local/scratch/jrrk2/gnusynthesis/vhd_front/apb_uart_hier.tcl"}
redirect -append /local/scratch/jrrk2/gnusynthesis/vhd_front/fm_apb_uart_hier.log {echo "**************************************************************************************************"}

setup

if [info exists verification_constant_prop_mode] {
  set fm_write_hier_saved_vars(verification_constant_prop_mode) $verification_constant_prop_mode
}
set verification_constant_prop_mode none

if [info exists signature_analysis_match_blackbox_input] {
  set fm_write_hier_saved_vars(signature_analysis_match_blackbox_input) $signature_analysis_match_blackbox_input
}
set signature_analysis_match_blackbox_input false

if [info exists signature_analysis_match_blackbox_output] {
  set fm_write_hier_saved_vars(signature_analysis_match_blackbox_output) $signature_analysis_match_blackbox_output
}
set signature_analysis_match_blackbox_output false

if [info exists signature_analysis_match_primary_input] {
  set fm_write_hier_saved_vars(signature_analysis_match_primary_input) $signature_analysis_match_primary_input
}
set signature_analysis_match_primary_input false

if [info exists signature_analysis_match_primary_output] {
  set fm_write_hier_saved_vars(signature_analysis_match_primary_output) $signature_analysis_match_primary_output
}
set signature_analysis_match_primary_output false

if [info exists fm_hier_result_array] {
  unset fm_hier_result_array
}
set fm_hier_result_count 0
set fm_tmp_result_count 0
set fm_session_files_saved 0
set fm_save_file_limit 1
set fm_save_time_limit 0

proc get_verification_status {ref_inst imp_inst} {
  global fm_tmp_result_count
  global fm_hier_result_array
  
  for {set i 0} {$i < $fm_tmp_result_count} {incr i} {
    if [expr (![string compare [lindex $fm_hier_result_array([expr $i + 1]) 0] $ref_inst])] {
      if [expr (![string compare [lindex $fm_hier_result_array([expr $i + 1]) 1] $imp_inst])] {
        return [lindex $fm_hier_result_array([expr $i + 1]) 2]
      }
    }
  }
  
  return UNKNOWN
}

###
### Verifying instances: 
###   Ref: r:/WORK/apb_uart
###   Imp: i:/WORK/apb_uart
###
set_reference_design      r:/WORK/apb_uart
set_implementation_design i:/WORK/apb_uart
set at_least_one_black_box 0
if [expr true] {
  set_black_box  $ref/UART_BG16
  set_black_box $impl/UART_BG16
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_BG16 $ref/UART_BG16
  set_user_match -type pin $impl/UART_BG16/CLK $ref/UART_BG16/CLK
  set_user_match -type pin $impl/UART_BG16/RST $ref/UART_BG16/RST
  set_user_match -type pin $impl/UART_BG16/CE $ref/UART_BG16/CE
  set_user_match -type pin $impl/UART_BG16/CLEAR $ref/UART_BG16/CLEAR
  set_user_match -type pin $impl/UART_BG16/DIVIDER[15] $ref/UART_BG16/DIVIDER[15]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[14] $ref/UART_BG16/DIVIDER[14]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[13] $ref/UART_BG16/DIVIDER[13]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[12] $ref/UART_BG16/DIVIDER[12]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[11] $ref/UART_BG16/DIVIDER[11]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[10] $ref/UART_BG16/DIVIDER[10]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[9] $ref/UART_BG16/DIVIDER[9]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[8] $ref/UART_BG16/DIVIDER[8]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[7] $ref/UART_BG16/DIVIDER[7]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[6] $ref/UART_BG16/DIVIDER[6]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[5] $ref/UART_BG16/DIVIDER[5]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[4] $ref/UART_BG16/DIVIDER[4]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[3] $ref/UART_BG16/DIVIDER[3]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[2] $ref/UART_BG16/DIVIDER[2]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[1] $ref/UART_BG16/DIVIDER[1]
  set_user_match -type pin $impl/UART_BG16/DIVIDER[0] $ref/UART_BG16/DIVIDER[0]
  set_user_match -type pin $impl/UART_BG16/BAUDTICK $ref/UART_BG16/BAUDTICK
  # The following matched pin(s) are constant,
  # therefore need not be verified at this level.
  set_dont_verify_points -type pin  $ref/UART_BG16/CE
  set_dont_verify_points -type pin  $ref/UART_BG16/CLEAR
  # The following matched pin(s) are constant,
  # therefore need not be verified at this level.
  set_dont_verify_points -type pin $impl/UART_BG16/CE
  set_dont_verify_points -type pin $impl/UART_BG16/CLEAR
}
if [expr true] {
  set_black_box  $ref/UART_BG2
  set_black_box $impl/UART_BG2
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_BG2 $ref/UART_BG2
  set_user_match -type pin $impl/UART_BG2/CLK $ref/UART_BG2/CLK
  set_user_match -type pin $impl/UART_BG2/RST $ref/UART_BG2/RST
  set_user_match -type pin $impl/UART_BG2/CE $ref/UART_BG2/CE
  set_user_match -type pin $impl/UART_BG2/Q $ref/UART_BG2/Q
}
if [expr true] {
  set_black_box  $ref/UART_BIDET
  set_black_box $impl/UART_BIDET
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_BIDET $ref/UART_BIDET
  set_user_match -type pin $impl/UART_BIDET/CLK $ref/UART_BIDET/CLK
  set_user_match -type pin $impl/UART_BIDET/RST $ref/UART_BIDET/RST
  set_user_match -type pin $impl/UART_BIDET/D $ref/UART_BIDET/D
  set_user_match -type pin $impl/UART_BIDET/RE $ref/UART_BIDET/RE
  set_constant -type pin  $ref/UART_BIDET/RE 0
  set_constant -type pin $impl/UART_BIDET/RE 0
  set_constant -type pin $impl/UART_BIDET/FE 0
  # The following matched pin(s) are unread at lower level,
  # therefore need not be verified at this level.
  set_dont_verify_points -type pin  $ref/UART_BIDET/CLK
  set_dont_verify_points -type pin  $ref/UART_BIDET/RST
  # The following matched pin(s) are unread at lower level,
  # therefore need not be verified at this level.
  set_dont_verify_points -type pin $impl/UART_BIDET/CLK
  set_dont_verify_points -type pin $impl/UART_BIDET/RST
}
if [expr true] {
  set_black_box  $ref/UART_ED_CTS
  set_black_box $impl/UART_ED_CTS
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_ED_CTS $ref/UART_ED_CTS
  set_user_match -type pin $impl/UART_ED_CTS/CLK $ref/UART_ED_CTS/CLK
  set_user_match -type pin $impl/UART_ED_CTS/RST $ref/UART_ED_CTS/RST
  set_user_match -type pin $impl/UART_ED_CTS/D $ref/UART_ED_CTS/D
  set_user_match -type pin $impl/UART_ED_CTS/RE $ref/UART_ED_CTS/RE
  set_user_match -type pin $impl/UART_ED_CTS/FE $ref/UART_ED_CTS/FE
}
if [expr true] {
  set_black_box  $ref/UART_ED_DCD
  set_black_box $impl/UART_ED_DCD
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_ED_DCD $ref/UART_ED_DCD
  set_user_match -type pin $impl/UART_ED_DCD/CLK $ref/UART_ED_DCD/CLK
  set_user_match -type pin $impl/UART_ED_DCD/RST $ref/UART_ED_DCD/RST
  set_user_match -type pin $impl/UART_ED_DCD/D $ref/UART_ED_DCD/D
  set_user_match -type pin $impl/UART_ED_DCD/RE $ref/UART_ED_DCD/RE
  set_user_match -type pin $impl/UART_ED_DCD/FE $ref/UART_ED_DCD/FE
}
if [expr true] {
  set_black_box  $ref/UART_ED_DSR
  set_black_box $impl/UART_ED_DSR
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_ED_DSR $ref/UART_ED_DSR
  set_user_match -type pin $impl/UART_ED_DSR/CLK $ref/UART_ED_DSR/CLK
  set_user_match -type pin $impl/UART_ED_DSR/RST $ref/UART_ED_DSR/RST
  set_user_match -type pin $impl/UART_ED_DSR/D $ref/UART_ED_DSR/D
  set_user_match -type pin $impl/UART_ED_DSR/RE $ref/UART_ED_DSR/RE
  set_user_match -type pin $impl/UART_ED_DSR/FE $ref/UART_ED_DSR/FE
}
if [expr true] {
  set_black_box  $ref/UART_ED_RI
  set_black_box $impl/UART_ED_RI
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_ED_RI $ref/UART_ED_RI
  set_user_match -type pin $impl/UART_ED_RI/CLK $ref/UART_ED_RI/CLK
  set_user_match -type pin $impl/UART_ED_RI/RST $ref/UART_ED_RI/RST
  set_user_match -type pin $impl/UART_ED_RI/D $ref/UART_ED_RI/D
  set_user_match -type pin $impl/UART_ED_RI/FE $ref/UART_ED_RI/FE
}
if [expr true] {
  set_black_box  $ref/UART_FEDET
  set_black_box $impl/UART_FEDET
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_FEDET $ref/UART_FEDET
  set_user_match -type pin $impl/UART_FEDET/CLK $ref/UART_FEDET/CLK
  set_user_match -type pin $impl/UART_FEDET/RST $ref/UART_FEDET/RST
  set_user_match -type pin $impl/UART_FEDET/D $ref/UART_FEDET/D
  set_user_match -type pin $impl/UART_FEDET/RE $ref/UART_FEDET/RE
  set_constant -type pin  $ref/UART_FEDET/RE 0
  set_constant -type pin $impl/UART_FEDET/RE 0
  set_constant -type pin $impl/UART_FEDET/FE 0
  # The following matched pin(s) are unread at lower level,
  # therefore need not be verified at this level.
  set_dont_verify_points -type pin  $ref/UART_FEDET/CLK
  set_dont_verify_points -type pin  $ref/UART_FEDET/RST
  # The following matched pin(s) are unread at lower level,
  # therefore need not be verified at this level.
  set_dont_verify_points -type pin $impl/UART_FEDET/CLK
  set_dont_verify_points -type pin $impl/UART_FEDET/RST
}
if [expr true] {
  set_black_box  $ref/UART_IF_CTS
  set_black_box $impl/UART_IF_CTS
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_IF_CTS $ref/UART_IF_CTS
  set_user_match -type pin $impl/UART_IF_CTS/CLK $ref/UART_IF_CTS/CLK
  set_user_match -type pin $impl/UART_IF_CTS/RST $ref/UART_IF_CTS/RST
  set_user_match -type pin $impl/UART_IF_CTS/CE $ref/UART_IF_CTS/CE
  set_user_match -type pin $impl/UART_IF_CTS/D $ref/UART_IF_CTS/D
  set_user_match -type pin $impl/UART_IF_CTS/Q $ref/UART_IF_CTS/Q
}
if [expr true] {
  set_black_box  $ref/UART_IF_DCD
  set_black_box $impl/UART_IF_DCD
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_IF_DCD $ref/UART_IF_DCD
  set_user_match -type pin $impl/UART_IF_DCD/CLK $ref/UART_IF_DCD/CLK
  set_user_match -type pin $impl/UART_IF_DCD/RST $ref/UART_IF_DCD/RST
  set_user_match -type pin $impl/UART_IF_DCD/CE $ref/UART_IF_DCD/CE
  set_user_match -type pin $impl/UART_IF_DCD/D $ref/UART_IF_DCD/D
  set_user_match -type pin $impl/UART_IF_DCD/Q $ref/UART_IF_DCD/Q
}
if [expr true] {
  set_black_box  $ref/UART_IF_DSR
  set_black_box $impl/UART_IF_DSR
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_IF_DSR $ref/UART_IF_DSR
  set_user_match -type pin $impl/UART_IF_DSR/CLK $ref/UART_IF_DSR/CLK
  set_user_match -type pin $impl/UART_IF_DSR/RST $ref/UART_IF_DSR/RST
  set_user_match -type pin $impl/UART_IF_DSR/CE $ref/UART_IF_DSR/CE
  set_user_match -type pin $impl/UART_IF_DSR/D $ref/UART_IF_DSR/D
  set_user_match -type pin $impl/UART_IF_DSR/Q $ref/UART_IF_DSR/Q
}
if [expr true] {
  set_black_box  $ref/UART_IF_RI
  set_black_box $impl/UART_IF_RI
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_IF_RI $ref/UART_IF_RI
  set_user_match -type pin $impl/UART_IF_RI/CLK $ref/UART_IF_RI/CLK
  set_user_match -type pin $impl/UART_IF_RI/RST $ref/UART_IF_RI/RST
  set_user_match -type pin $impl/UART_IF_RI/CE $ref/UART_IF_RI/CE
  set_user_match -type pin $impl/UART_IF_RI/D $ref/UART_IF_RI/D
  set_user_match -type pin $impl/UART_IF_RI/Q $ref/UART_IF_RI/Q
}
if [expr true] {
  set_black_box  $ref/UART_IIC
  set_black_box $impl/UART_IIC
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_IIC $ref/UART_IIC
  set_user_match -type pin $impl/UART_IIC/CLK $ref/UART_IIC/CLK
  set_user_match -type pin $impl/UART_IIC/RST $ref/UART_IIC/RST
  set_user_match -type pin $impl/UART_IIC/IER[3] $ref/UART_IIC/IER[3]
  set_user_match -type pin $impl/UART_IIC/IER[2] $ref/UART_IIC/IER[2]
  set_user_match -type pin $impl/UART_IIC/IER[1] $ref/UART_IIC/IER[1]
  set_user_match -type pin $impl/UART_IIC/IER[0] $ref/UART_IIC/IER[0]
  set_user_match -type pin $impl/UART_IIC/LSR[4] $ref/UART_IIC/LSR[4]
  set_user_match -type pin $impl/UART_IIC/LSR[3] $ref/UART_IIC/LSR[3]
  set_user_match -type pin $impl/UART_IIC/LSR[2] $ref/UART_IIC/LSR[2]
  set_user_match -type pin $impl/UART_IIC/LSR[1] $ref/UART_IIC/LSR[1]
  set_user_match -type pin $impl/UART_IIC/THI $ref/UART_IIC/THI
  set_user_match -type pin $impl/UART_IIC/RDA $ref/UART_IIC/RDA
  set_user_match -type pin $impl/UART_IIC/CTI $ref/UART_IIC/CTI
  set_user_match -type pin $impl/UART_IIC/AFE $ref/UART_IIC/AFE
  set_user_match -type pin $impl/UART_IIC/MSR[3] $ref/UART_IIC/MSR[3]
  set_user_match -type pin $impl/UART_IIC/MSR[2] $ref/UART_IIC/MSR[2]
  set_user_match -type pin $impl/UART_IIC/MSR[1] $ref/UART_IIC/MSR[1]
  set_user_match -type pin $impl/UART_IIC/MSR[0] $ref/UART_IIC/MSR[0]
  set_user_match -type pin $impl/UART_IIC/IIR[3] $ref/UART_IIC/IIR[3]
  set_user_match -type pin $impl/UART_IIC/IIR[2] $ref/UART_IIC/IIR[2]
  set_user_match -type pin $impl/UART_IIC/IIR[1] $ref/UART_IIC/IIR[1]
  set_user_match -type pin $impl/UART_IIC/IIR[0] $ref/UART_IIC/IIR[0]
  set_user_match -type pin $impl/UART_IIC/INT $ref/UART_IIC/INT
  current_des r:/WORK/apb_uart
  set master_obj UART_IIC/IIR[0]
  set slave_obj UART_IIC/INT
  rewire_connection -invert -type pin -to $master_obj -from $slave_obj
  current_des i:/WORK/apb_uart
  set master_obj UART_IIC/IIR[0]
  set slave_obj UART_IIC/INT
  rewire_connection -invert -type pin -to $master_obj -from $slave_obj
  # The following unmatched pin(s) will cause verification failure at lower level if used there,
  # therefore need not cause verification failure at this level.
  set_dont_verify_points -type pin  $ref/UART_IIC/LSR[0]
  # The following unmatched pin(s) will cause verification failure at lower level if used there,
  # therefore need not cause verification failure at this level.
  set_dont_verify_points -type pin $impl/UART_IIC/LSR[0]
}
if [expr true] {
  set_black_box  $ref/UART_IIC_THRE_ED
  set_black_box $impl/UART_IIC_THRE_ED
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_IIC_THRE_ED $ref/UART_IIC_THRE_ED
  set_user_match -type pin $impl/UART_IIC_THRE_ED/CLK $ref/UART_IIC_THRE_ED/CLK
  set_user_match -type pin $impl/UART_IIC_THRE_ED/RST $ref/UART_IIC_THRE_ED/RST
  set_user_match -type pin $impl/UART_IIC_THRE_ED/D $ref/UART_IIC_THRE_ED/D
  set_user_match -type pin $impl/UART_IIC_THRE_ED/RE $ref/UART_IIC_THRE_ED/RE
}
if [expr true] {
  set_black_box  $ref/UART_IS_CTS
  set_black_box $impl/UART_IS_CTS
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_IS_CTS $ref/UART_IS_CTS
  set_user_match -type pin $impl/UART_IS_CTS/CLK $ref/UART_IS_CTS/CLK
  set_user_match -type pin $impl/UART_IS_CTS/RST $ref/UART_IS_CTS/RST
  set_user_match -type pin $impl/UART_IS_CTS/D $ref/UART_IS_CTS/D
  set_user_match -type pin $impl/UART_IS_CTS/Q $ref/UART_IS_CTS/Q
}
if [expr true] {
  set_black_box  $ref/UART_IS_DCD
  set_black_box $impl/UART_IS_DCD
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_IS_DCD $ref/UART_IS_DCD
  set_user_match -type pin $impl/UART_IS_DCD/CLK $ref/UART_IS_DCD/CLK
  set_user_match -type pin $impl/UART_IS_DCD/RST $ref/UART_IS_DCD/RST
  set_user_match -type pin $impl/UART_IS_DCD/D $ref/UART_IS_DCD/D
  set_user_match -type pin $impl/UART_IS_DCD/Q $ref/UART_IS_DCD/Q
}
if [expr true] {
  set_black_box  $ref/UART_IS_DSR
  set_black_box $impl/UART_IS_DSR
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_IS_DSR $ref/UART_IS_DSR
  set_user_match -type pin $impl/UART_IS_DSR/CLK $ref/UART_IS_DSR/CLK
  set_user_match -type pin $impl/UART_IS_DSR/RST $ref/UART_IS_DSR/RST
  set_user_match -type pin $impl/UART_IS_DSR/D $ref/UART_IS_DSR/D
  set_user_match -type pin $impl/UART_IS_DSR/Q $ref/UART_IS_DSR/Q
}
if [expr true] {
  set_black_box  $ref/UART_IS_RI
  set_black_box $impl/UART_IS_RI
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_IS_RI $ref/UART_IS_RI
  set_user_match -type pin $impl/UART_IS_RI/CLK $ref/UART_IS_RI/CLK
  set_user_match -type pin $impl/UART_IS_RI/RST $ref/UART_IS_RI/RST
  set_user_match -type pin $impl/UART_IS_RI/D $ref/UART_IS_RI/D
  set_user_match -type pin $impl/UART_IS_RI/Q $ref/UART_IS_RI/Q
}
if [expr true] {
  set_black_box  $ref/UART_IS_SIN
  set_black_box $impl/UART_IS_SIN
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_IS_SIN $ref/UART_IS_SIN
  set_user_match -type pin $impl/UART_IS_SIN/CLK $ref/UART_IS_SIN/CLK
  set_user_match -type pin $impl/UART_IS_SIN/RST $ref/UART_IS_SIN/RST
  set_user_match -type pin $impl/UART_IS_SIN/D $ref/UART_IS_SIN/D
  set_user_match -type pin $impl/UART_IS_SIN/Q $ref/UART_IS_SIN/Q
}
if [expr true] {
  set_black_box  $ref/UART_PEDET
  set_black_box $impl/UART_PEDET
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_PEDET $ref/UART_PEDET
  set_user_match -type pin $impl/UART_PEDET/CLK $ref/UART_PEDET/CLK
  set_user_match -type pin $impl/UART_PEDET/RST $ref/UART_PEDET/RST
  set_user_match -type pin $impl/UART_PEDET/D $ref/UART_PEDET/D
  set_user_match -type pin $impl/UART_PEDET/RE $ref/UART_PEDET/RE
  set_constant -type pin  $ref/UART_PEDET/RE 0
  set_constant -type pin $impl/UART_PEDET/RE 0
  set_constant -type pin $impl/UART_PEDET/FE 0
  # The following matched pin(s) are unread at lower level,
  # therefore need not be verified at this level.
  set_dont_verify_points -type pin  $ref/UART_PEDET/CLK
  set_dont_verify_points -type pin  $ref/UART_PEDET/RST
  # The following matched pin(s) are unread at lower level,
  # therefore need not be verified at this level.
  set_dont_verify_points -type pin $impl/UART_PEDET/CLK
  set_dont_verify_points -type pin $impl/UART_PEDET/RST
}
if [expr true] {
  set_black_box  $ref/UART_RCLK
  set_black_box $impl/UART_RCLK
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_RCLK $ref/UART_RCLK
  set_user_match -type pin $impl/UART_RCLK/CLK $ref/UART_RCLK/CLK
  set_user_match -type pin $impl/UART_RCLK/RST $ref/UART_RCLK/RST
  set_user_match -type pin $impl/UART_RCLK/D $ref/UART_RCLK/D
  set_user_match -type pin $impl/UART_RCLK/RE $ref/UART_RCLK/RE
}
if [expr true] {
  set_black_box  $ref/UART_RX
  set_black_box $impl/UART_RX
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_RX $ref/UART_RX
  set_user_match -type pin $impl/UART_RX/CLK $ref/UART_RX/CLK
  set_user_match -type pin $impl/UART_RX/RST $ref/UART_RX/RST
  set_user_match -type pin $impl/UART_RX/RXCLK $ref/UART_RX/RXCLK
  set_user_match -type pin $impl/UART_RX/WLS[1] $ref/UART_RX/WLS[1]
  set_user_match -type pin $impl/UART_RX/WLS[0] $ref/UART_RX/WLS[0]
  set_user_match -type pin $impl/UART_RX/PEN $ref/UART_RX/PEN
  set_user_match -type pin $impl/UART_RX/EPS $ref/UART_RX/EPS
  set_user_match -type pin $impl/UART_RX/SP $ref/UART_RX/SP
  set_user_match -type pin $impl/UART_RX/SIN $ref/UART_RX/SIN
  set_user_match -type pin $impl/UART_RX/PE $ref/UART_RX/PE
  set_user_match -type pin $impl/UART_RX/FE $ref/UART_RX/FE
  set_user_match -type pin $impl/UART_RX/BI $ref/UART_RX/BI
  set_user_match -type pin $impl/UART_RX/DOUT[0] $ref/UART_RX/DOUT[0]
  set_user_match -type pin $impl/UART_RX/RXFINISHED $ref/UART_RX/RXFINISHED
  # The following unmatched pin(s) will cause verification failure at lower level if used there,
  # therefore need not cause verification failure at this level.
  set_dont_verify_points -type pin  $ref/UART_RX/RXCLEAR
  set_dont_verify_points -type pin  $ref/UART_RX/STB
  # The following unmatched pin(s) will cause verification failure at lower level if used there,
  # therefore need not cause verification failure at this level.
  set_dont_verify_points -type pin $impl/UART_RX/RXCLEAR
  set_dont_verify_points -type pin $impl/UART_RX/STB
}
if [expr true] {
  set_black_box  $ref/UART_RXFF
  set_black_box $impl/UART_RXFF
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_RXFF $ref/UART_RXFF
  set_user_match -type pin $impl/UART_RXFF/CLK $ref/UART_RXFF/CLK
  set_user_match -type pin $impl/UART_RXFF/RST $ref/UART_RXFF/RST
  set_user_match -type pin $impl/UART_RXFF/CLEAR $ref/UART_RXFF/CLEAR
  set_user_match -type pin $impl/UART_RXFF/WRITE $ref/UART_RXFF/WRITE
  set_user_match -type pin $impl/UART_RXFF/READ $ref/UART_RXFF/READ
  set_user_match -type pin $impl/UART_RXFF/D[10] $ref/UART_RXFF/D[10]
  set_user_match -type pin $impl/UART_RXFF/D[9] $ref/UART_RXFF/D[9]
  set_user_match -type pin $impl/UART_RXFF/D[8] $ref/UART_RXFF/D[8]
  set_user_match -type pin $impl/UART_RXFF/D[7] $ref/UART_RXFF/D[7]
  set_user_match -type pin $impl/UART_RXFF/D[6] $ref/UART_RXFF/D[6]
  set_user_match -type pin $impl/UART_RXFF/D[5] $ref/UART_RXFF/D[5]
  set_user_match -type pin $impl/UART_RXFF/D[4] $ref/UART_RXFF/D[4]
  set_user_match -type pin $impl/UART_RXFF/D[3] $ref/UART_RXFF/D[3]
  set_user_match -type pin $impl/UART_RXFF/D[2] $ref/UART_RXFF/D[2]
  set_user_match -type pin $impl/UART_RXFF/D[1] $ref/UART_RXFF/D[1]
  set_user_match -type pin $impl/UART_RXFF/D[0] $ref/UART_RXFF/D[0]
  set_user_match -type pin $impl/UART_RXFF/Q[10] $ref/UART_RXFF/Q[10]
  set_user_match -type pin $impl/UART_RXFF/Q[9] $ref/UART_RXFF/Q[9]
  set_user_match -type pin $impl/UART_RXFF/Q[8] $ref/UART_RXFF/Q[8]
  set_user_match -type pin $impl/UART_RXFF/Q[7] $ref/UART_RXFF/Q[7]
  set_user_match -type pin $impl/UART_RXFF/Q[6] $ref/UART_RXFF/Q[6]
  set_user_match -type pin $impl/UART_RXFF/Q[5] $ref/UART_RXFF/Q[5]
  set_user_match -type pin $impl/UART_RXFF/Q[4] $ref/UART_RXFF/Q[4]
  set_user_match -type pin $impl/UART_RXFF/Q[3] $ref/UART_RXFF/Q[3]
  set_user_match -type pin $impl/UART_RXFF/Q[2] $ref/UART_RXFF/Q[2]
  set_user_match -type pin $impl/UART_RXFF/Q[1] $ref/UART_RXFF/Q[1]
  set_user_match -type pin $impl/UART_RXFF/Q[0] $ref/UART_RXFF/Q[0]
  set_user_match -type pin $impl/UART_RXFF/EMPTY $ref/UART_RXFF/EMPTY
  set_user_match -type pin $impl/UART_RXFF/FULL $ref/UART_RXFF/FULL
  set_user_match -type pin $impl/UART_RXFF/USAGE[5] $ref/UART_RXFF/USAGE[5]
  set_user_match -type pin $impl/UART_RXFF/USAGE[4] $ref/UART_RXFF/USAGE[4]
  set_user_match -type pin $impl/UART_RXFF/USAGE[3] $ref/UART_RXFF/USAGE[3]
  set_user_match -type pin $impl/UART_RXFF/USAGE[2] $ref/UART_RXFF/USAGE[2]
  set_user_match -type pin $impl/UART_RXFF/USAGE[1] $ref/UART_RXFF/USAGE[1]
  set_constant -type pin  $ref/UART_RXFF/Q[10] 0
  set_constant -type pin  $ref/UART_RXFF/Q[9] 0
  set_constant -type pin  $ref/UART_RXFF/Q[8] 0
  set_constant -type pin  $ref/UART_RXFF/Q[7] 0
  set_constant -type pin  $ref/UART_RXFF/Q[6] 0
  set_constant -type pin  $ref/UART_RXFF/Q[5] 0
  set_constant -type pin  $ref/UART_RXFF/Q[4] 0
  set_constant -type pin  $ref/UART_RXFF/Q[3] 0
  set_constant -type pin  $ref/UART_RXFF/Q[2] 0
  set_constant -type pin  $ref/UART_RXFF/Q[1] 0
  set_constant -type pin $impl/UART_RXFF/Q[10] 0
  set_constant -type pin $impl/UART_RXFF/Q[9] 0
  set_constant -type pin $impl/UART_RXFF/Q[8] 0
  set_constant -type pin $impl/UART_RXFF/Q[7] 0
  set_constant -type pin $impl/UART_RXFF/Q[6] 0
  set_constant -type pin $impl/UART_RXFF/Q[5] 0
  set_constant -type pin $impl/UART_RXFF/Q[4] 0
  set_constant -type pin $impl/UART_RXFF/Q[3] 0
  set_constant -type pin $impl/UART_RXFF/Q[2] 0
  set_constant -type pin $impl/UART_RXFF/Q[1] 0
}
if [expr true] {
  set_black_box  $ref/UART_TX
  set_black_box $impl/UART_TX
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_TX $ref/UART_TX
  set_user_match -type pin $impl/UART_TX/CLK $ref/UART_TX/CLK
  set_user_match -type pin $impl/UART_TX/RST $ref/UART_TX/RST
  set_user_match -type pin $impl/UART_TX/TXCLK $ref/UART_TX/TXCLK
  set_user_match -type pin $impl/UART_TX/TXSTART $ref/UART_TX/TXSTART
  set_user_match -type pin $impl/UART_TX/WLS[1] $ref/UART_TX/WLS[1]
  set_user_match -type pin $impl/UART_TX/WLS[0] $ref/UART_TX/WLS[0]
  set_user_match -type pin $impl/UART_TX/STB $ref/UART_TX/STB
  set_user_match -type pin $impl/UART_TX/PEN $ref/UART_TX/PEN
  set_user_match -type pin $impl/UART_TX/EPS $ref/UART_TX/EPS
  set_user_match -type pin $impl/UART_TX/SP $ref/UART_TX/SP
  set_user_match -type pin $impl/UART_TX/BC $ref/UART_TX/BC
  set_user_match -type pin $impl/UART_TX/DIN[7] $ref/UART_TX/DIN[7]
  set_user_match -type pin $impl/UART_TX/DIN[6] $ref/UART_TX/DIN[6]
  set_user_match -type pin $impl/UART_TX/DIN[5] $ref/UART_TX/DIN[5]
  set_user_match -type pin $impl/UART_TX/DIN[4] $ref/UART_TX/DIN[4]
  set_user_match -type pin $impl/UART_TX/DIN[3] $ref/UART_TX/DIN[3]
  set_user_match -type pin $impl/UART_TX/DIN[2] $ref/UART_TX/DIN[2]
  set_user_match -type pin $impl/UART_TX/DIN[1] $ref/UART_TX/DIN[1]
  set_user_match -type pin $impl/UART_TX/DIN[0] $ref/UART_TX/DIN[0]
  set_user_match -type pin $impl/UART_TX/TXFINISHED $ref/UART_TX/TXFINISHED
  set_user_match -type pin $impl/UART_TX/SOUT $ref/UART_TX/SOUT
  # The following unmatched pin(s) will cause verification failure at lower level if used there,
  # therefore need not cause verification failure at this level.
  set_dont_verify_points -type pin  $ref/UART_TX/CLEAR
  # The following unmatched pin(s) will cause verification failure at lower level if used there,
  # therefore need not cause verification failure at this level.
  set_dont_verify_points -type pin $impl/UART_TX/CLEAR
}
if [expr true] {
  set_black_box  $ref/UART_TXFF
  set_black_box $impl/UART_TXFF
  set at_least_one_black_box 1
  set_user_match -type cell $impl/UART_TXFF $ref/UART_TXFF
  set_user_match -type pin $impl/UART_TXFF/CLK $ref/UART_TXFF/CLK
  set_user_match -type pin $impl/UART_TXFF/RST $ref/UART_TXFF/RST
  set_user_match -type pin $impl/UART_TXFF/CLEAR $ref/UART_TXFF/CLEAR
  set_user_match -type pin $impl/UART_TXFF/WRITE $ref/UART_TXFF/WRITE
  set_user_match -type pin $impl/UART_TXFF/READ $ref/UART_TXFF/READ
  set_user_match -type pin $impl/UART_TXFF/D[7] $ref/UART_TXFF/D[7]
  set_user_match -type pin $impl/UART_TXFF/D[6] $ref/UART_TXFF/D[6]
  set_user_match -type pin $impl/UART_TXFF/D[5] $ref/UART_TXFF/D[5]
  set_user_match -type pin $impl/UART_TXFF/D[4] $ref/UART_TXFF/D[4]
  set_user_match -type pin $impl/UART_TXFF/D[3] $ref/UART_TXFF/D[3]
  set_user_match -type pin $impl/UART_TXFF/D[2] $ref/UART_TXFF/D[2]
  set_user_match -type pin $impl/UART_TXFF/D[1] $ref/UART_TXFF/D[1]
  set_user_match -type pin $impl/UART_TXFF/D[0] $ref/UART_TXFF/D[0]
  set_user_match -type pin $impl/UART_TXFF/Q[7] $ref/UART_TXFF/Q[7]
  set_user_match -type pin $impl/UART_TXFF/Q[6] $ref/UART_TXFF/Q[6]
  set_user_match -type pin $impl/UART_TXFF/Q[5] $ref/UART_TXFF/Q[5]
  set_user_match -type pin $impl/UART_TXFF/Q[4] $ref/UART_TXFF/Q[4]
  set_user_match -type pin $impl/UART_TXFF/Q[3] $ref/UART_TXFF/Q[3]
  set_user_match -type pin $impl/UART_TXFF/Q[2] $ref/UART_TXFF/Q[2]
  set_user_match -type pin $impl/UART_TXFF/Q[1] $ref/UART_TXFF/Q[1]
  set_user_match -type pin $impl/UART_TXFF/Q[0] $ref/UART_TXFF/Q[0]
  set_user_match -type pin $impl/UART_TXFF/EMPTY $ref/UART_TXFF/EMPTY
  set_user_match -type pin $impl/UART_TXFF/FULL $ref/UART_TXFF/FULL
  set_user_match -type pin $impl/UART_TXFF/USAGE[4] $ref/UART_TXFF/USAGE[4]
}
set_user_match -type cell $impl/rx_State_reg $ref/UART_RXPROC.rx_State_reg
set_user_match -type cell $impl/tx_State_reg[0] $ref/UART_TXPROC.tx_State_reg[0]
set_user_match -type cell $impl/tx_State_reg[1] $ref/UART_TXPROC.tx_State_reg[1]
set fm_begin_cputime [cputime]
verify
set fm_end_cputime [cputime]
set fm_this_verification_cputime [expr $fm_end_cputime - $fm_begin_cputime]
set fm_cumulative_memory [expr [memory] / 1000]
set fm_tmp_result_count [expr $fm_tmp_result_count + 1]
set fm_hier_result_array($fm_tmp_result_count) [list {r:/WORK/apb_uart} {i:/WORK/apb_uart} $verification_status]
redirect -append /local/scratch/jrrk2/gnusynthesis/vhd_front/fm_apb_uart_hier.log {format "  "}
redirect -append /local/scratch/jrrk2/gnusynthesis/vhd_front/fm_apb_uart_hier.log {format "Verification %s:" $verification_status}
redirect -append /local/scratch/jrrk2/gnusynthesis/vhd_front/fm_apb_uart_hier.log {format "  Ref: %s (top)" {r:/WORK/apb_uart}}
redirect -append /local/scratch/jrrk2/gnusynthesis/vhd_front/fm_apb_uart_hier.log {format "  Imp: %s (top)" {i:/WORK/apb_uart}}
redirect -append /local/scratch/jrrk2/gnusynthesis/vhd_front/fm_apb_uart_hier.log {format "  %s, %4.0fMB (cumulative), %7.2fsec (incremental)" [date] $fm_cumulative_memory $fm_this_verification_cputime}
if [expr ((![string compare $verification_status "FAILED"]) || \
          (![string compare $verification_status "INCONCLUSIVE"])) && \
         ($fm_session_files_saved < $fm_save_file_limit) && \
         ($fm_this_verification_cputime >= $fm_save_time_limit)] {
  save_session -replace /local/scratch/jrrk2/gnusynthesis/vhd_front/fm_apb_uart_hier.tcl.$fm_tmp_result_count.fss
  lappend fm_hier_result_array($fm_tmp_result_count) /local/scratch/jrrk2/gnusynthesis/vhd_front/fm_apb_uart_hier.tcl.$fm_tmp_result_count.fss
  redirect -append /local/scratch/jrrk2/gnusynthesis/vhd_front/fm_apb_uart_hier.log {format "  Session file: /local/scratch/jrrk2/gnusynthesis/vhd_front/fm_apb_uart_hier.tcl.$fm_tmp_result_count.fss"}
  set fm_session_files_saved [expr $fm_session_files_saved + 1]
}
setup
if [expr ($at_least_one_black_box)] {
  set at_least_one_black_box 0
  if [expr true] {
    remove_black_box  $ref/UART_BG16
    remove_black_box $impl/UART_BG16
    remove_user_match -type cell $impl/UART_BG16
    remove_user_match -type pin $impl/UART_BG16/CLK
    remove_user_match -type pin $impl/UART_BG16/RST
    remove_user_match -type pin $impl/UART_BG16/CE
    remove_user_match -type pin $impl/UART_BG16/CLEAR
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[15]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[14]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[13]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[12]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[11]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[10]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[9]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[8]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[7]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[6]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[5]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[4]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[3]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[2]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[1]
    remove_user_match -type pin $impl/UART_BG16/DIVIDER[0]
    remove_user_match -type pin $impl/UART_BG16/BAUDTICK
    remove_dont_verify_points -type pin  $ref/UART_BG16/CE
    remove_dont_verify_points -type pin  $ref/UART_BG16/CLEAR
    remove_dont_verify_points -type pin $impl/UART_BG16/CE
    remove_dont_verify_points -type pin $impl/UART_BG16/CLEAR
  }
  if [expr true] {
    remove_black_box  $ref/UART_BG2
    remove_black_box $impl/UART_BG2
    remove_user_match -type cell $impl/UART_BG2
    remove_user_match -type pin $impl/UART_BG2/CLK
    remove_user_match -type pin $impl/UART_BG2/RST
    remove_user_match -type pin $impl/UART_BG2/CE
    remove_user_match -type pin $impl/UART_BG2/Q
  }
  if [expr true] {
    remove_black_box  $ref/UART_BIDET
    remove_black_box $impl/UART_BIDET
    remove_user_match -type cell $impl/UART_BIDET
    remove_user_match -type pin $impl/UART_BIDET/CLK
    remove_user_match -type pin $impl/UART_BIDET/RST
    remove_user_match -type pin $impl/UART_BIDET/D
    remove_user_match -type pin $impl/UART_BIDET/RE
    remove_constant -type pin  $ref/UART_BIDET/RE 
    remove_constant -type pin $impl/UART_BIDET/RE 
    remove_constant -type pin $impl/UART_BIDET/FE 
    remove_dont_verify_points -type pin  $ref/UART_BIDET/CLK
    remove_dont_verify_points -type pin  $ref/UART_BIDET/RST
    remove_dont_verify_points -type pin $impl/UART_BIDET/CLK
    remove_dont_verify_points -type pin $impl/UART_BIDET/RST
  }
  if [expr true] {
    remove_black_box  $ref/UART_ED_CTS
    remove_black_box $impl/UART_ED_CTS
    remove_user_match -type cell $impl/UART_ED_CTS
    remove_user_match -type pin $impl/UART_ED_CTS/CLK
    remove_user_match -type pin $impl/UART_ED_CTS/RST
    remove_user_match -type pin $impl/UART_ED_CTS/D
    remove_user_match -type pin $impl/UART_ED_CTS/RE
    remove_user_match -type pin $impl/UART_ED_CTS/FE
  }
  if [expr true] {
    remove_black_box  $ref/UART_ED_DCD
    remove_black_box $impl/UART_ED_DCD
    remove_user_match -type cell $impl/UART_ED_DCD
    remove_user_match -type pin $impl/UART_ED_DCD/CLK
    remove_user_match -type pin $impl/UART_ED_DCD/RST
    remove_user_match -type pin $impl/UART_ED_DCD/D
    remove_user_match -type pin $impl/UART_ED_DCD/RE
    remove_user_match -type pin $impl/UART_ED_DCD/FE
  }
  if [expr true] {
    remove_black_box  $ref/UART_ED_DSR
    remove_black_box $impl/UART_ED_DSR
    remove_user_match -type cell $impl/UART_ED_DSR
    remove_user_match -type pin $impl/UART_ED_DSR/CLK
    remove_user_match -type pin $impl/UART_ED_DSR/RST
    remove_user_match -type pin $impl/UART_ED_DSR/D
    remove_user_match -type pin $impl/UART_ED_DSR/RE
    remove_user_match -type pin $impl/UART_ED_DSR/FE
  }
  if [expr true] {
    remove_black_box  $ref/UART_ED_RI
    remove_black_box $impl/UART_ED_RI
    remove_user_match -type cell $impl/UART_ED_RI
    remove_user_match -type pin $impl/UART_ED_RI/CLK
    remove_user_match -type pin $impl/UART_ED_RI/RST
    remove_user_match -type pin $impl/UART_ED_RI/D
    remove_user_match -type pin $impl/UART_ED_RI/FE
  }
  if [expr true] {
    remove_black_box  $ref/UART_FEDET
    remove_black_box $impl/UART_FEDET
    remove_user_match -type cell $impl/UART_FEDET
    remove_user_match -type pin $impl/UART_FEDET/CLK
    remove_user_match -type pin $impl/UART_FEDET/RST
    remove_user_match -type pin $impl/UART_FEDET/D
    remove_user_match -type pin $impl/UART_FEDET/RE
    remove_constant -type pin  $ref/UART_FEDET/RE 
    remove_constant -type pin $impl/UART_FEDET/RE 
    remove_constant -type pin $impl/UART_FEDET/FE 
    remove_dont_verify_points -type pin  $ref/UART_FEDET/CLK
    remove_dont_verify_points -type pin  $ref/UART_FEDET/RST
    remove_dont_verify_points -type pin $impl/UART_FEDET/CLK
    remove_dont_verify_points -type pin $impl/UART_FEDET/RST
  }
  if [expr true] {
    remove_black_box  $ref/UART_IF_CTS
    remove_black_box $impl/UART_IF_CTS
    remove_user_match -type cell $impl/UART_IF_CTS
    remove_user_match -type pin $impl/UART_IF_CTS/CLK
    remove_user_match -type pin $impl/UART_IF_CTS/RST
    remove_user_match -type pin $impl/UART_IF_CTS/CE
    remove_user_match -type pin $impl/UART_IF_CTS/D
    remove_user_match -type pin $impl/UART_IF_CTS/Q
  }
  if [expr true] {
    remove_black_box  $ref/UART_IF_DCD
    remove_black_box $impl/UART_IF_DCD
    remove_user_match -type cell $impl/UART_IF_DCD
    remove_user_match -type pin $impl/UART_IF_DCD/CLK
    remove_user_match -type pin $impl/UART_IF_DCD/RST
    remove_user_match -type pin $impl/UART_IF_DCD/CE
    remove_user_match -type pin $impl/UART_IF_DCD/D
    remove_user_match -type pin $impl/UART_IF_DCD/Q
  }
  if [expr true] {
    remove_black_box  $ref/UART_IF_DSR
    remove_black_box $impl/UART_IF_DSR
    remove_user_match -type cell $impl/UART_IF_DSR
    remove_user_match -type pin $impl/UART_IF_DSR/CLK
    remove_user_match -type pin $impl/UART_IF_DSR/RST
    remove_user_match -type pin $impl/UART_IF_DSR/CE
    remove_user_match -type pin $impl/UART_IF_DSR/D
    remove_user_match -type pin $impl/UART_IF_DSR/Q
  }
  if [expr true] {
    remove_black_box  $ref/UART_IF_RI
    remove_black_box $impl/UART_IF_RI
    remove_user_match -type cell $impl/UART_IF_RI
    remove_user_match -type pin $impl/UART_IF_RI/CLK
    remove_user_match -type pin $impl/UART_IF_RI/RST
    remove_user_match -type pin $impl/UART_IF_RI/CE
    remove_user_match -type pin $impl/UART_IF_RI/D
    remove_user_match -type pin $impl/UART_IF_RI/Q
  }
  if [expr true] {
    remove_black_box  $ref/UART_IIC
    remove_black_box $impl/UART_IIC
    remove_user_match -type cell $impl/UART_IIC
    remove_user_match -type pin $impl/UART_IIC/CLK
    remove_user_match -type pin $impl/UART_IIC/RST
    remove_user_match -type pin $impl/UART_IIC/IER[3]
    remove_user_match -type pin $impl/UART_IIC/IER[2]
    remove_user_match -type pin $impl/UART_IIC/IER[1]
    remove_user_match -type pin $impl/UART_IIC/IER[0]
    remove_user_match -type pin $impl/UART_IIC/LSR[4]
    remove_user_match -type pin $impl/UART_IIC/LSR[3]
    remove_user_match -type pin $impl/UART_IIC/LSR[2]
    remove_user_match -type pin $impl/UART_IIC/LSR[1]
    remove_user_match -type pin $impl/UART_IIC/THI
    remove_user_match -type pin $impl/UART_IIC/RDA
    remove_user_match -type pin $impl/UART_IIC/CTI
    remove_user_match -type pin $impl/UART_IIC/AFE
    remove_user_match -type pin $impl/UART_IIC/MSR[3]
    remove_user_match -type pin $impl/UART_IIC/MSR[2]
    remove_user_match -type pin $impl/UART_IIC/MSR[1]
    remove_user_match -type pin $impl/UART_IIC/MSR[0]
    remove_user_match -type pin $impl/UART_IIC/IIR[3]
    remove_user_match -type pin $impl/UART_IIC/IIR[2]
    remove_user_match -type pin $impl/UART_IIC/IIR[1]
    remove_user_match -type pin $impl/UART_IIC/IIR[0]
    remove_user_match -type pin $impl/UART_IIC/INT
    remove_dont_verify_points -type pin  $ref/UART_IIC/LSR[0]
    remove_dont_verify_points -type pin $impl/UART_IIC/LSR[0]
  }
  if [expr true] {
    remove_black_box  $ref/UART_IIC_THRE_ED
    remove_black_box $impl/UART_IIC_THRE_ED
    remove_user_match -type cell $impl/UART_IIC_THRE_ED
    remove_user_match -type pin $impl/UART_IIC_THRE_ED/CLK
    remove_user_match -type pin $impl/UART_IIC_THRE_ED/RST
    remove_user_match -type pin $impl/UART_IIC_THRE_ED/D
    remove_user_match -type pin $impl/UART_IIC_THRE_ED/RE
  }
  if [expr true] {
    remove_black_box  $ref/UART_IS_CTS
    remove_black_box $impl/UART_IS_CTS
    remove_user_match -type cell $impl/UART_IS_CTS
    remove_user_match -type pin $impl/UART_IS_CTS/CLK
    remove_user_match -type pin $impl/UART_IS_CTS/RST
    remove_user_match -type pin $impl/UART_IS_CTS/D
    remove_user_match -type pin $impl/UART_IS_CTS/Q
  }
  if [expr true] {
    remove_black_box  $ref/UART_IS_DCD
    remove_black_box $impl/UART_IS_DCD
    remove_user_match -type cell $impl/UART_IS_DCD
    remove_user_match -type pin $impl/UART_IS_DCD/CLK
    remove_user_match -type pin $impl/UART_IS_DCD/RST
    remove_user_match -type pin $impl/UART_IS_DCD/D
    remove_user_match -type pin $impl/UART_IS_DCD/Q
  }
  if [expr true] {
    remove_black_box  $ref/UART_IS_DSR
    remove_black_box $impl/UART_IS_DSR
    remove_user_match -type cell $impl/UART_IS_DSR
    remove_user_match -type pin $impl/UART_IS_DSR/CLK
    remove_user_match -type pin $impl/UART_IS_DSR/RST
    remove_user_match -type pin $impl/UART_IS_DSR/D
    remove_user_match -type pin $impl/UART_IS_DSR/Q
  }
  if [expr true] {
    remove_black_box  $ref/UART_IS_RI
    remove_black_box $impl/UART_IS_RI
    remove_user_match -type cell $impl/UART_IS_RI
    remove_user_match -type pin $impl/UART_IS_RI/CLK
    remove_user_match -type pin $impl/UART_IS_RI/RST
    remove_user_match -type pin $impl/UART_IS_RI/D
    remove_user_match -type pin $impl/UART_IS_RI/Q
  }
  if [expr true] {
    remove_black_box  $ref/UART_IS_SIN
    remove_black_box $impl/UART_IS_SIN
    remove_user_match -type cell $impl/UART_IS_SIN
    remove_user_match -type pin $impl/UART_IS_SIN/CLK
    remove_user_match -type pin $impl/UART_IS_SIN/RST
    remove_user_match -type pin $impl/UART_IS_SIN/D
    remove_user_match -type pin $impl/UART_IS_SIN/Q
  }
  if [expr true] {
    remove_black_box  $ref/UART_PEDET
    remove_black_box $impl/UART_PEDET
    remove_user_match -type cell $impl/UART_PEDET
    remove_user_match -type pin $impl/UART_PEDET/CLK
    remove_user_match -type pin $impl/UART_PEDET/RST
    remove_user_match -type pin $impl/UART_PEDET/D
    remove_user_match -type pin $impl/UART_PEDET/RE
    remove_constant -type pin  $ref/UART_PEDET/RE 
    remove_constant -type pin $impl/UART_PEDET/RE 
    remove_constant -type pin $impl/UART_PEDET/FE 
    remove_dont_verify_points -type pin  $ref/UART_PEDET/CLK
    remove_dont_verify_points -type pin  $ref/UART_PEDET/RST
    remove_dont_verify_points -type pin $impl/UART_PEDET/CLK
    remove_dont_verify_points -type pin $impl/UART_PEDET/RST
  }
  if [expr true] {
    remove_black_box  $ref/UART_RCLK
    remove_black_box $impl/UART_RCLK
    remove_user_match -type cell $impl/UART_RCLK
    remove_user_match -type pin $impl/UART_RCLK/CLK
    remove_user_match -type pin $impl/UART_RCLK/RST
    remove_user_match -type pin $impl/UART_RCLK/D
    remove_user_match -type pin $impl/UART_RCLK/RE
  }
  if [expr true] {
    remove_black_box  $ref/UART_RX
    remove_black_box $impl/UART_RX
    remove_user_match -type cell $impl/UART_RX
    remove_user_match -type pin $impl/UART_RX/CLK
    remove_user_match -type pin $impl/UART_RX/RST
    remove_user_match -type pin $impl/UART_RX/RXCLK
    remove_user_match -type pin $impl/UART_RX/WLS[1]
    remove_user_match -type pin $impl/UART_RX/WLS[0]
    remove_user_match -type pin $impl/UART_RX/PEN
    remove_user_match -type pin $impl/UART_RX/EPS
    remove_user_match -type pin $impl/UART_RX/SP
    remove_user_match -type pin $impl/UART_RX/SIN
    remove_user_match -type pin $impl/UART_RX/PE
    remove_user_match -type pin $impl/UART_RX/FE
    remove_user_match -type pin $impl/UART_RX/BI
    remove_user_match -type pin $impl/UART_RX/DOUT[0]
    remove_user_match -type pin $impl/UART_RX/RXFINISHED
    remove_dont_verify_points -type pin  $ref/UART_RX/RXCLEAR
    remove_dont_verify_points -type pin  $ref/UART_RX/STB
    remove_dont_verify_points -type pin $impl/UART_RX/RXCLEAR
    remove_dont_verify_points -type pin $impl/UART_RX/STB
  }
  if [expr true] {
    remove_black_box  $ref/UART_RXFF
    remove_black_box $impl/UART_RXFF
    remove_user_match -type cell $impl/UART_RXFF
    remove_user_match -type pin $impl/UART_RXFF/CLK
    remove_user_match -type pin $impl/UART_RXFF/RST
    remove_user_match -type pin $impl/UART_RXFF/CLEAR
    remove_user_match -type pin $impl/UART_RXFF/WRITE
    remove_user_match -type pin $impl/UART_RXFF/READ
    remove_user_match -type pin $impl/UART_RXFF/D[10]
    remove_user_match -type pin $impl/UART_RXFF/D[9]
    remove_user_match -type pin $impl/UART_RXFF/D[8]
    remove_user_match -type pin $impl/UART_RXFF/D[7]
    remove_user_match -type pin $impl/UART_RXFF/D[6]
    remove_user_match -type pin $impl/UART_RXFF/D[5]
    remove_user_match -type pin $impl/UART_RXFF/D[4]
    remove_user_match -type pin $impl/UART_RXFF/D[3]
    remove_user_match -type pin $impl/UART_RXFF/D[2]
    remove_user_match -type pin $impl/UART_RXFF/D[1]
    remove_user_match -type pin $impl/UART_RXFF/D[0]
    remove_user_match -type pin $impl/UART_RXFF/Q[10]
    remove_user_match -type pin $impl/UART_RXFF/Q[9]
    remove_user_match -type pin $impl/UART_RXFF/Q[8]
    remove_user_match -type pin $impl/UART_RXFF/Q[7]
    remove_user_match -type pin $impl/UART_RXFF/Q[6]
    remove_user_match -type pin $impl/UART_RXFF/Q[5]
    remove_user_match -type pin $impl/UART_RXFF/Q[4]
    remove_user_match -type pin $impl/UART_RXFF/Q[3]
    remove_user_match -type pin $impl/UART_RXFF/Q[2]
    remove_user_match -type pin $impl/UART_RXFF/Q[1]
    remove_user_match -type pin $impl/UART_RXFF/Q[0]
    remove_user_match -type pin $impl/UART_RXFF/EMPTY
    remove_user_match -type pin $impl/UART_RXFF/FULL
    remove_user_match -type pin $impl/UART_RXFF/USAGE[5]
    remove_user_match -type pin $impl/UART_RXFF/USAGE[4]
    remove_user_match -type pin $impl/UART_RXFF/USAGE[3]
    remove_user_match -type pin $impl/UART_RXFF/USAGE[2]
    remove_user_match -type pin $impl/UART_RXFF/USAGE[1]
    remove_constant -type pin  $ref/UART_RXFF/Q[10] 
    remove_constant -type pin  $ref/UART_RXFF/Q[9] 
    remove_constant -type pin  $ref/UART_RXFF/Q[8] 
    remove_constant -type pin  $ref/UART_RXFF/Q[7] 
    remove_constant -type pin  $ref/UART_RXFF/Q[6] 
    remove_constant -type pin  $ref/UART_RXFF/Q[5] 
    remove_constant -type pin  $ref/UART_RXFF/Q[4] 
    remove_constant -type pin  $ref/UART_RXFF/Q[3] 
    remove_constant -type pin  $ref/UART_RXFF/Q[2] 
    remove_constant -type pin  $ref/UART_RXFF/Q[1] 
    remove_constant -type pin $impl/UART_RXFF/Q[10] 
    remove_constant -type pin $impl/UART_RXFF/Q[9] 
    remove_constant -type pin $impl/UART_RXFF/Q[8] 
    remove_constant -type pin $impl/UART_RXFF/Q[7] 
    remove_constant -type pin $impl/UART_RXFF/Q[6] 
    remove_constant -type pin $impl/UART_RXFF/Q[5] 
    remove_constant -type pin $impl/UART_RXFF/Q[4] 
    remove_constant -type pin $impl/UART_RXFF/Q[3] 
    remove_constant -type pin $impl/UART_RXFF/Q[2] 
    remove_constant -type pin $impl/UART_RXFF/Q[1] 
  }
  if [expr true] {
    remove_black_box  $ref/UART_TX
    remove_black_box $impl/UART_TX
    remove_user_match -type cell $impl/UART_TX
    remove_user_match -type pin $impl/UART_TX/CLK
    remove_user_match -type pin $impl/UART_TX/RST
    remove_user_match -type pin $impl/UART_TX/TXCLK
    remove_user_match -type pin $impl/UART_TX/TXSTART
    remove_user_match -type pin $impl/UART_TX/WLS[1]
    remove_user_match -type pin $impl/UART_TX/WLS[0]
    remove_user_match -type pin $impl/UART_TX/STB
    remove_user_match -type pin $impl/UART_TX/PEN
    remove_user_match -type pin $impl/UART_TX/EPS
    remove_user_match -type pin $impl/UART_TX/SP
    remove_user_match -type pin $impl/UART_TX/BC
    remove_user_match -type pin $impl/UART_TX/DIN[7]
    remove_user_match -type pin $impl/UART_TX/DIN[6]
    remove_user_match -type pin $impl/UART_TX/DIN[5]
    remove_user_match -type pin $impl/UART_TX/DIN[4]
    remove_user_match -type pin $impl/UART_TX/DIN[3]
    remove_user_match -type pin $impl/UART_TX/DIN[2]
    remove_user_match -type pin $impl/UART_TX/DIN[1]
    remove_user_match -type pin $impl/UART_TX/DIN[0]
    remove_user_match -type pin $impl/UART_TX/TXFINISHED
    remove_user_match -type pin $impl/UART_TX/SOUT
    remove_dont_verify_points -type pin  $ref/UART_TX/CLEAR
    remove_dont_verify_points -type pin $impl/UART_TX/CLEAR
  }
  if [expr true] {
    remove_black_box  $ref/UART_TXFF
    remove_black_box $impl/UART_TXFF
    remove_user_match -type cell $impl/UART_TXFF
    remove_user_match -type pin $impl/UART_TXFF/CLK
    remove_user_match -type pin $impl/UART_TXFF/RST
    remove_user_match -type pin $impl/UART_TXFF/CLEAR
    remove_user_match -type pin $impl/UART_TXFF/WRITE
    remove_user_match -type pin $impl/UART_TXFF/READ
    remove_user_match -type pin $impl/UART_TXFF/D[7]
    remove_user_match -type pin $impl/UART_TXFF/D[6]
    remove_user_match -type pin $impl/UART_TXFF/D[5]
    remove_user_match -type pin $impl/UART_TXFF/D[4]
    remove_user_match -type pin $impl/UART_TXFF/D[3]
    remove_user_match -type pin $impl/UART_TXFF/D[2]
    remove_user_match -type pin $impl/UART_TXFF/D[1]
    remove_user_match -type pin $impl/UART_TXFF/D[0]
    remove_user_match -type pin $impl/UART_TXFF/Q[7]
    remove_user_match -type pin $impl/UART_TXFF/Q[6]
    remove_user_match -type pin $impl/UART_TXFF/Q[5]
    remove_user_match -type pin $impl/UART_TXFF/Q[4]
    remove_user_match -type pin $impl/UART_TXFF/Q[3]
    remove_user_match -type pin $impl/UART_TXFF/Q[2]
    remove_user_match -type pin $impl/UART_TXFF/Q[1]
    remove_user_match -type pin $impl/UART_TXFF/Q[0]
    remove_user_match -type pin $impl/UART_TXFF/EMPTY
    remove_user_match -type pin $impl/UART_TXFF/FULL
    remove_user_match -type pin $impl/UART_TXFF/USAGE[4]
  }
}
remove_user_match -type cell $impl/rx_State_reg
remove_user_match -type cell $impl/tx_State_reg[0]
remove_user_match -type cell $impl/tx_State_reg[1]

if [info exists fm_write_hier_saved_vars] {
  foreach _var [array names fm_write_hier_saved_vars] {
    set $_var $fm_write_hier_saved_vars($_var)
  };
  unset fm_write_hier_saved_vars
}

###
### Report results
###
set fm_hier_result_count $fm_tmp_result_count
set fm_log_fp [open /local/scratch/jrrk2/gnusynthesis/vhd_front/fm_apb_uart_hier.log]
puts [read $fm_log_fp]
close $fm_log_fp

