(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)
(* $Id: printast.ml 10263 2010-04-17 14:45:12Z garrigue $ *)

open Asttypes0;;
open Format;;
open Lexing;;
open Location;;
open Parsetree0;;
open Ctypes

let fmt_position f l =
  if l.pos_fname = "" && l.pos_lnum = 1
  then fprintf f "%d" l.pos_cnum
  else if l.pos_lnum = -1
  then fprintf f "%s[%d]" l.pos_fname l.pos_cnum
  else fprintf f "%s[%d,%d+%d]" l.pos_fname l.pos_lnum l.pos_bol
               (l.pos_cnum - l.pos_bol)
;;

let fmt_location f loc = ()
 (*
  fprintf f " (%a..%a)" fmt_position loc.loc_start fmt_position loc.loc_end;
  if loc.loc_ghost then fprintf f " ghost";
*)
;;

let rec fmt_longident_aux x:ctypes =
  match x with
  | Longident.Lident (s) -> CID s;
  | Longident.Ldot (y, s) -> (match fmt_longident_aux y with
      | CID s' -> CIDLIST (s :: [s'])
      | _ -> CID s)
  | Longident.Lapply (y, z) -> CAPPLIST (fmt_longident_aux y, fmt_longident_aux z)
;;

let fmt_longident x = fmt_longident_aux x;;

let fmt_constant x =
  match x with
  | Const_int (i) -> VINT i;
  | Const_char (c) -> VCHAR (Char.code c);
  | Const_string (s) -> VSTRING s;
  | Const_float (s) -> VFLOAT (float_of_string s);
  | Const_int32 (i) -> VINT32 i;
  | Const_int64 (i) -> VINT64 i;
  | Const_nativeint (i) -> VNATIVEINT i;
;;

let fmt_mutable_flag x =
  match x with
  | Immutable -> CImmutable
  | Mutable -> CMutable
;;

let fmt_virtual_flag x =
  match x with
  | Virtual -> CVirtual
  | Concrete -> CConcrete
;;

let fmt_override_flag x =
  match x with
  | Override -> COverride
  | Fresh -> CFresh
;;

let fmt_rec_flag x =
  match x with
  | Nonrecursive -> CNonrec
  | Recursive -> CRec
  | Default -> CDefault
;;

let fmt_direction_flag x =
  match x with
  | Upto -> CUp
  | Downto -> CDown
;;

let fmt_private_flag x =
  match x with
  | Public -> CPublic
  | Private -> CPrivate
;;

let line _ f s (*...*) =
  fprintf f s (*...*)
;;

let list f l =
  match l with
  | [] -> []
  | h::t -> List.map f l;
;;

let option f x =
  match x with
  | None -> CNONE
  | Some x -> CSOME (f x)
;;

let longident li = fmt_longident li;;
let string s = VSTRING s;;
let bool x = VBOOL x;;
let label x = fprintf Format.err_formatter "label=\"%s\"\n" x;;

let rec core_type x =
 (*
  fprintf Format.err_formatter "core_type %a\n" fmt_location x.ptyp_loc;
*)
  match x.ptyp_desc with
  | Ptyp_any -> CTYPANY
  | Ptyp_var (s) -> CTYPVAR (VSTRING s);
  | Ptyp_arrow (l, ct1, ct2) -> CTYPARROW (core_type ct1, core_type ct2)
  | Ptyp_tuple l -> CTYPTUPLE (List.map (fun itm -> core_type itm) l)
  | Ptyp_constr (li, l) -> CTYPCONSTR (longident li, List.map (fun itm -> core_type itm) l)
  | Ptyp_variant (l, closed, low) -> CTYPVARIANT (List.map (fun itm -> label_x_bool_x_core_type_list itm) l)
  | Ptyp_object (l) -> CTYPOBJ (List.map (fun itm -> core_field_type itm) l)
  | Ptyp_class (li, l, low) -> CTYPCLASS (List.map (fun itm -> core_type itm) l)
  | Ptyp_alias (ct, s) -> CTYPALIAS (core_type ct)
  | Ptyp_poly (sl, ct) -> CTYPPOLY (sl, core_type ct)
  | Ptyp_package (s, l) -> CTYPPACK (List.map (fun (str, itm) -> core_type itm) l)

and package_with (s, t) =
  fprintf Format.err_formatter "with type %s\n" s;
  core_type t

and core_field_type x =
  fprintf Format.err_formatter "core_field_type %a\n" fmt_location x.pfield_loc;
  
  match x.pfield_desc with
  | Pfield (s, ct) ->
      fprintf Format.err_formatter "Pfield \"%s\"\n" s;
      core_type ct;
  | Pfield_var -> CFIELDVAR

and pattern x:ctypes =
 (*
  fprintf Format.err_formatter "pattern %a\n" fmt_location x.ppat_loc;
*)  
  match x.ppat_desc with
  | Ppat_any -> CPATANY
  | Ppat_var (s) -> CPATVAR (VSTRING s)
  | Ppat_alias (p, s) -> CPATALIAS (VSTRING s, pattern p)
  | Ppat_constant (c) -> CPATCONS [fmt_constant c];
  | Ppat_tuple (l) -> CPATTUPLE (List.map (fun itm -> pattern itm) l)
  | Ppat_construct (li, po, b) -> CPATCONS [option pattern po]
  | Ppat_variant (l, po) -> CPATVAR (option pattern po)
  | Ppat_record (l, c) -> CPATRECORD (List.map (fun itm -> pattern (snd itm)) l)
  | Ppat_array (l) -> CPATARRAY (List.map (fun itm -> pattern itm) l)
  | Ppat_or (p1, p2) -> CPATOR [pattern p1; pattern p2]
  | Ppat_lazy p -> CPATLAZ (pattern p)
  | Ppat_constraint (p, ct) -> CPATCONSTRAIN (pattern p)
  | Ppat_type li -> CPATTYP (fmt_longident_aux li)

and expression x =
 (*
  fprintf Format.err_formatter "expression %a\n" fmt_location x.pexp_loc;
*)
  
  match x.pexp_desc with
  | Pexp_ident (li) -> PEXPIDENT (fmt_longident li);
  | Pexp_constant (c) -> PEXPCONSTANT (fmt_constant c);
  | Pexp_let (rf, l, e) -> PEXPLET (List.map pattern_x_expression_def l, expression e);
  | Pexp_function (p, eo, l) -> PEXPFUN (p, option expression eo, list pattern_x_expression_case l)
  | Pexp_apply (e, l) -> PEXPAPP (expression e, list label_x_expression l)
  | Pexp_match (e, l) -> PEXPMATCH (expression e, list pattern_x_expression_case l)
  | Pexp_try (e, l) -> PEXPTRY (expression e, list pattern_x_expression_case l)
  | Pexp_tuple (l) -> PEXPTUPLE (list expression l)
  | Pexp_construct (li, eo, b) -> PEXPCONSTR (fmt_longident li, option expression eo)
  | Pexp_variant (l, eo) -> PEXPVARIANT (VSTRING l, option expression eo)
  | Pexp_record (l, eo) -> PEXPRECORD (list longident_x_expression l, option expression eo)
  | Pexp_field (e, li) -> PEXPFIELD (expression e, longident li)
  | Pexp_setfield (e1, li, e2) -> PEXPSETFIELD (expression e1, longident li, expression e2)
  | Pexp_array (l) -> PEXPARRAY (list expression l)
  | Pexp_ifthenelse (e1, e2, eo) -> PEXPIFTHENELSE (expression e1, expression e2, option expression eo)
  | Pexp_sequence (e1, e2) -> PEXPSEQ (expression e1, expression e2)
  | Pexp_while (e1, e2) -> PEXPWHILE (expression e1, expression e2)
  | Pexp_for (s, e1, e2, df, e3) -> PEXPFOR (VSTRING s, fmt_direction_flag df, expression e1, expression e2, expression e3)
  | Pexp_constraint (e, cto1, cto2) -> PEXPCONSTRAINT (expression e, option core_type cto1, option core_type cto2)
  | Pexp_when (e1, e2) -> PEXPWHEN (expression e1, expression e2)
  | Pexp_send (e, s) -> PEXPSEND (VSTRING s, expression e)
  | Pexp_new (li) -> PEXPNEW (fmt_longident li)
  | Pexp_setinstvar (s, e) -> PEXPSETINSTVAR (VSTRING s, expression e)
  | Pexp_override (l) -> PEXPOVERRIDE (list string_x_expression l)
  | Pexp_letmodule (s, me, e) -> PEXPLETMODULE (VSTRING s, module_expr me, expression e)
  | Pexp_assert (e) -> PEXPASSERT (expression e)
  | Pexp_assertfalse -> PEXPASSERTFALSE
  | Pexp_lazy (e) -> PEXPLAZY (expression e)
  | Pexp_poly (e, cto) -> PEXPPOLY (expression e, option core_type cto)
  | Pexp_object s -> PEXPOBJECT (class_structure s)
  | Pexp_newtype (s, e) -> PEXPNEWTYPE (VSTRING s, expression e)
  | Pexp_pack (me, (p,l)) -> PEXPPACK (fmt_longident p, list package_with l, module_expr me)
  | Pexp_open (m, e) -> PEXPOPEN (fmt_longident m, expression e)

and value_description x = CVALUEDESC (core_type x.pval_type, list string x.pval_prim)

and type_declaration x:ctypes =
 (*
  fprintf Format.err_formatter "type_declaration %a\n" fmt_location x.ptype_loc;
*)
PTYPEDECL (
  list string x.ptype_params,
  list core_type_x_core_type_x_location x.ptype_cstrs,
  type_kind x.ptype_kind,
  fmt_private_flag x.ptype_private,
  option core_type x.ptype_manifest)

and type_kind x =
  match x with
  | Ptype_abstract -> PTYPEABSTRACT
  | Ptype_variant l -> PTYPEVARIANT (list string_x_core_type_list_x_location l)
  | Ptype_record l -> PTYPERECORD (list string_x_mutable_flag_x_core_type_x_location l)

and exception_declaration x = list core_type x

and class_type x =
 (*
  fprintf Format.err_formatter "class_type %a\n" fmt_location x.pcty_loc;
*)
  
  match x.pcty_desc with
  | Pcty_constr (li, l) -> PCTYCONSTR (fmt_longident li, list core_type l)
  | Pcty_signature (cs) -> PCTYSIGNATURE (class_signature cs)
  | Pcty_fun (l, co, cl) -> PCTYFUN (VSTRING l, core_type co, class_type cl)

and class_signature (ct, l) = CLASSSIGNATURE (core_type ct, list class_type_field l)

and class_type_field x = VUNIT (*
  match x with
  | Pctf_inher (ct) ->
      fprintf Format.err_formatter "Pctf_inher\n";
      class_type ct;
  | Pctf_val (s, mf, vf, ct, loc) ->
      fprintf Format.err_formatter
        "Pctf_val \"%s\" %a %a %a\n" s
        fmt_mutable_flag mf fmt_virtual_flag vf fmt_location loc;
      core_type ct;
  | Pctf_virt (s, pf, ct, loc) ->
      fprintf Format.err_formatter
        "Pctf_virt \"%s\" %a %a\n" s fmt_private_flag pf fmt_location loc;
      core_type ct;
  | Pctf_meth (s, pf, ct, loc) ->
      fprintf Format.err_formatter
        "Pctf_meth \"%s\" %a %a\n" s fmt_private_flag pf fmt_location loc;
      core_type ct;
  | Pctf_cstr (ct1, ct2, loc) ->
      fprintf Format.err_formatter "Pctf_cstr %a\n" fmt_location loc;
      core_type ct1;
      core_type ct2;
				 *)
and class_description x = VUNIT
 (*
  fprintf Format.err_formatter "class_description %a\n" fmt_location x.pci_loc;
  
  fprintf Format.err_formatter "pci_virt = %a\n" fmt_virtual_flag x.pci_virt;
  fprintf Format.err_formatter "pci_params =\n";
  string_list_x_location x.pci_params;
  fprintf Format.err_formatter "pci_name = \"%s\"\n" x.pci_name;
  fprintf Format.err_formatter "pci_expr =\n";
  class_type x.pci_expr;
*)
and class_type_declaration x = VUNIT
 (*
  fprintf Format.err_formatter "class_type_declaration %a\n" fmt_location x.pci_loc;
  
  fprintf Format.err_formatter "pci_virt = %a\n" fmt_virtual_flag x.pci_virt;
  fprintf Format.err_formatter "pci_params =\n";
  string_list_x_location x.pci_params;
  fprintf Format.err_formatter "pci_name = \"%s\"\n" x.pci_name;
  fprintf Format.err_formatter "pci_expr =\n";
  class_type x.pci_expr;
*)

and class_expr x = VUNIT
 (*
  fprintf Format.err_formatter "class_expr %a\n" fmt_location x.pcl_loc;
  
  match x.pcl_desc with
  | Pcl_constr (li, l) ->
      fprintf Format.err_formatter "Pcl_constr %a\n" fmt_longident li;
      list core_type l;
  | Pcl_structure (cs) ->
      fprintf Format.err_formatter "Pcl_structure\n";
      class_structure cs;
  | Pcl_fun (l, eo, p, e) ->
      fprintf Format.err_formatter "Pcl_fun\n";
      label l;
      option expression eo;
      pattern p;
      class_expr e;
  | Pcl_apply (ce, l) ->
      fprintf Format.err_formatter "Pcl_apply\n";
      class_expr ce;
      list label_x_expression l;
  | Pcl_let (rf, l, ce) ->
      fprintf Format.err_formatter "Pcl_let %a\n" fmt_rec_flag rf;
      list pattern_x_expression_def l;
      class_expr ce;
  | Pcl_constraint (ce, ct) ->
      fprintf Format.err_formatter "Pcl_constraint\n";
      class_expr ce;
      class_type ct;
*)

and class_structure (p, l) = VUNIT
 (*
  fprintf Format.err_formatter "class_structure\n";
  pattern p;
  list class_field l;
*)

and class_field x = VUNIT
 (*
  match x with
  | Pcf_inher (ovf, ce, so) ->
      fprintf Format.err_formatter "Pcf_inher %a\n" fmt_override_flag ovf;
      class_expr ce;
      option string so;
  | Pcf_valvirt (s, mf, ct, loc) ->
      fprintf Format.err_formatter "Pcf_valvirt \"%s\" %a %a\n"
        s fmt_mutable_flag mf fmt_location loc;
      core_type ct;
  | Pcf_val (s, mf, ovf, e, loc) ->
      fprintf Format.err_formatter "Pcf_val \"%s\" %a %a %a\n"
        s fmt_mutable_flag mf fmt_override_flag ovf fmt_location loc;
      expression e;
  | Pcf_virt (s, pf, ct, loc) ->
      fprintf Format.err_formatter "Pcf_virt \"%s\" %a %a\n"
        s fmt_private_flag pf fmt_location loc;
      core_type ct;
  | Pcf_meth (s, pf, ovf, e, loc) ->
      fprintf Format.err_formatter "Pcf_meth \"%s\" %a %a %a\n"
        s fmt_private_flag pf fmt_override_flag ovf fmt_location loc;
      expression e;
  | Pcf_cstr (ct1, ct2, loc) ->
      fprintf Format.err_formatter "Pcf_cstr %a\n" fmt_location loc;
      core_type ct1;
      core_type ct2;
  | Pcf_let (rf, l, loc) ->
      fprintf Format.err_formatter "Pcf_let %a %a\n" fmt_rec_flag rf fmt_location loc;
      list pattern_x_expression_def l;
  | Pcf_init (e) ->
      fprintf Format.err_formatter "Pcf_init\n";
      expression e;
*)

and class_declaration x = VUNIT
 (*
  fprintf Format.err_formatter "class_declaration %a\n" fmt_location x.pci_loc;
  
  fprintf Format.err_formatter "pci_virt = %a\n" fmt_virtual_flag x.pci_virt;
  fprintf Format.err_formatter "pci_params =\n";
  string_list_x_location x.pci_params;
  fprintf Format.err_formatter "pci_name = \"%s\"\n" x.pci_name;
  fprintf Format.err_formatter "pci_expr =\n";
  class_expr x.pci_expr;
*)

and module_type x = VUNIT
 (*
  fprintf Format.err_formatter "module_type %a\n" fmt_location x.pmty_loc;
  
  match x.pmty_desc with
  | Pmty_ident (li) -> fprintf Format.err_formatter "Pmty_ident %a\n" fmt_longident li;
  | Pmty_signature (s) ->
      fprintf Format.err_formatter "Pmty_signature\n";
      signature s;
  | Pmty_functor (s, mt1, mt2) ->
      fprintf Format.err_formatter "Pmty_functor \"%s\"\n" s;
      module_type mt1;
      module_type mt2;
  | Pmty_with (mt, l) ->
      fprintf Format.err_formatter "Pmty_with\n";
      module_type mt;
      list longident_x_with_constraint l;
  | Pmty_typeof m ->
      fprintf Format.err_formatter "Pmty_typeof\n";
      module_expr m
*)

and signature x = list signature_item x

and signature_item x = VUNIT
 (*
  fprintf Format.err_formatter "signature_item %a\n" fmt_location x.psig_loc;
  
  match x.psig_desc with
  | Psig_value (s, vd) ->
      fprintf Format.err_formatter "Psig_value \"%s\"\n" s;
      value_description vd;
  | Psig_type (l) ->
      fprintf Format.err_formatter "Psig_type\n";
      list string_x_type_declaration l;
  | Psig_exception (s, ed) ->
      fprintf Format.err_formatter "Psig_exception \"%s\"\n" s;
      exception_declaration ed;
  | Psig_module (s, mt) ->
      fprintf Format.err_formatter "Psig_module \"%s\"\n" s;
      module_type mt;
  | Psig_recmodule decls ->
      fprintf Format.err_formatter "Psig_recmodule\n";
      list string_x_module_type decls;
  | Psig_modtype (s, md) ->
      fprintf Format.err_formatter "Psig_modtype \"%s\"\n" s;
      modtype_declaration md;
  | Psig_open (li) -> fprintf Format.err_formatter "Psig_open %a\n" fmt_longident li;
  | Psig_include (mt) ->
      fprintf Format.err_formatter "Psig_include\n";
      module_type mt;
  | Psig_class (l) ->
      fprintf Format.err_formatter "Psig_class\n";
      list class_description l;
  | Psig_class_type (l) ->
      fprintf Format.err_formatter "Psig_class_type\n";
      list class_type_declaration l;
*)

and modtype_declaration x = VUNIT
 (*
  match x with
  | Pmodtype_abstract -> fprintf Format.err_formatter "Pmodtype_abstract\n";
  | Pmodtype_manifest (mt) ->
      fprintf Format.err_formatter "Pmodtype_manifest\n";
      module_type mt;
*)

and with_constraint x = VUNIT
 (*
  match x with
  | Pwith_type (td) ->
      fprintf Format.err_formatter "Pwith_type\n";
      type_declaration td;
  | Pwith_typesubst (td) ->
      fprintf Format.err_formatter "Pwith_typesubst\n";
      type_declaration td;
  | Pwith_module (li) -> fprintf Format.err_formatter "Pwith_module %a\n" fmt_longident li;
  | Pwith_modsubst (li) -> fprintf Format.err_formatter "Pwith_modsubst %a\n" fmt_longident li;
*)

and module_expr x = VUNIT
 (*
  fprintf Format.err_formatter "module_expr %a\n" fmt_location x.pmod_loc;
  
  match x.pmod_desc with
  | Pmod_ident (li) -> fprintf Format.err_formatter "Pmod_ident %a\n" fmt_longident li;
  | Pmod_structure (s) ->
      fprintf Format.err_formatter "Pmod_structure\n";
      structure s;
  | Pmod_functor (s, mt, me) ->
      fprintf Format.err_formatter "Pmod_functor \"%s\"\n" s;
      module_type mt;
      module_expr me;
  | Pmod_apply (me1, me2) ->
      fprintf Format.err_formatter "Pmod_apply\n";
      module_expr me1;
      module_expr me2;
  | Pmod_constraint (me, mt) ->
      fprintf Format.err_formatter "Pmod_constraint\n";
      module_expr me;
      module_type mt;
  | Pmod_unpack (e, (p, l)) ->
      fprintf Format.err_formatter "Pmod_unpack %a\n" fmt_longident p;
      list package_with l;
      expression e;
*)

and structure x = list structure_item x

and structure_item x =
 (*
  fprintf Format.err_formatter "structure_item %a\n" fmt_location x.pstr_loc;
*)
  
  match x.pstr_desc with
  | Pstr_eval (e) -> PSTREVAL (expression e)
  | Pstr_value (rf, l) -> PSTRVALUE (fmt_rec_flag rf, list pattern_x_expression_def l)
  | Pstr_primitive (s, vd) -> PSTRPRIMITIVE (VSTRING s, value_description vd)
  | Pstr_type (l) -> PSTRTYPE (list string_x_type_declaration l)
  | Pstr_exception (s, ed) -> PSTREXCEPTION (VSTRING s, exception_declaration ed)
  | Pstr_exn_rebind (s, li) -> PSTREXNREBIND (VSTRING s, fmt_longident li)
  | Pstr_module (s, me) -> PSTRMODULE (VSTRING s, module_expr me)
  | Pstr_recmodule bindings -> PSTRRECMODULE (list string_x_modtype_x_module bindings)
  | Pstr_modtype (s, mt) -> PSTRMODTYPE (VSTRING s, module_type mt)
  | Pstr_open (li) -> PSTROPEN (fmt_longident li)
  | Pstr_class (l) -> PSTRCLASS (list class_declaration l)
  | Pstr_class_type (l) -> PSTRCLASSTYPE (list class_type_declaration l)
  | Pstr_include me -> PSTRINCLUDE (module_expr me)

and string_x_type_declaration (s, td) = CSTRINGXTYPDECL (string s, type_declaration td)

and string_x_module_type (s, mty) = CSTRINGXMODTYPE (string s, module_type mty)

and string_x_modtype_x_module (s, mty, modl) = CSTRINGXMODTYPXMOD (string s, module_type mty, module_expr modl)

and longident_x_with_constraint (li, wc) = VUNIT
 (*
  fprintf Format.err_formatter "%a\n" fmt_longident li;
  with_constraint wc;
*)

and core_type_x_core_type_x_location (ct1, ct2, l) = CONSTRAINT (core_type ct1, core_type ct2)

and string_x_core_type_list_x_location (s, l, loc) = CSTRINGTYPLOC (VSTRING s, list core_type l)

and string_x_mutable_flag_x_core_type_x_location (s, mf, ct, loc) = CSTRINGXMUT (
  VSTRING s, fmt_mutable_flag mf,  core_type ct)
 (*
and string_list_x_location (l, loc) =
  fprintf Format.err_formatter "<params> %a\n" fmt_location loc;
  list string l;

and longident_x_pattern (li, p) =
  fprintf Format.err_formatter "%a\n" fmt_longident li;
  pattern p;
*)
and pattern_x_expression_case (p, e) = CPATXEXPCASE (pattern p, expression e)

and pattern_x_expression_def (p, e) = CPATXEXP (pattern p, expression e)

and string_x_expression (s, e) = COVERRIDE (VSTRING s, expression e)

and longident_x_expression (li, e) = CIDENTX (fmt_longident li, expression e)

and label_x_expression (l,e) = CLABELXEXP (VSTRING l, expression e)

and label_x_bool_x_core_type_list x =
  match x with
    Rtag (l, b, ctl) -> CRTAG (VSTRING l,VBOOL b, list core_type ctl)
  | Rinherit (ct) -> CRINHERIT (core_type ct)
;;

let rec toplevel_phrase x =
  match x with
  | Ptop_def (s) -> CTOPDEF (structure s)
  | Ptop_dir (s, da) -> CTOPDIR (s, directive_argument da)

and directive_argument x = []
 (*
  match x with
  | Pdir_none -> fprintf Format.err_formatter "Pdir_none\n"
  | Pdir_string (s) -> fprintf Format.err_formatter "Pdir_string \"%s\"\n" s;
  | Pdir_int (i) -> fprintf Format.err_formatter "Pdir_int %d\n" i;
  | Pdir_ident (li) -> fprintf Format.err_formatter "Pdir_ident %a\n" fmt_longident li;
  | Pdir_bool (b) -> fprintf Format.err_formatter "Pdir_bool %s\n" (string_of_bool b);
;;
*)

let interface x = list signature_item x;;

let implementation x = list structure_item x;;

let top_phrase x = toplevel_phrase x;;
