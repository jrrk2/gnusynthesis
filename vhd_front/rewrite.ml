open VhdlTree
let mymap fn (x:vhdintf list) = List.map fn x
let maxidx idx = int_of_float(ceil(log(float_of_int idx)/.log(2.))) - 1

let debug = ref false

let rec abstraction = function
  | Octuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) -> Octuple(
     abstraction arg1,
     abstraction arg2,
     abstraction arg3,
     abstraction arg4,
     abstraction arg5,
     abstraction arg6,
     abstraction arg7,
     abstraction arg8)
  | Septuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7) -> Septuple(
     abstraction arg1,
     abstraction arg2,
     abstraction arg3,
     abstraction arg4,
     abstraction arg5,
     abstraction arg6,
     abstraction arg7)
  | Sextuple(arg1,arg2,arg3,arg4,arg5,arg6) -> Sextuple(
     abstraction arg1,
     abstraction arg2,
     abstraction arg3,
     abstraction arg4,
     abstraction arg5,
     abstraction arg6)
  | Quintuple(arg1,arg2,arg3,arg4,arg5) -> Quintuple(
     abstraction arg1,
     abstraction arg2,
     abstraction arg3,
     abstraction arg4,
     abstraction arg5)
  | Quadruple(arg1,arg2,arg3,arg4) -> Quadruple(
     abstraction arg1,
     abstraction arg2,
     abstraction arg3,
     abstraction arg4)
  | Double (VhdSimpleName, Str vector) -> Str vector
  | Double (VhdBlockTypeDeclaration,
      Double (VhdFullType,
       Triple (Vhdfull_type_declaration, typ,
        Double (VhdEnumerationTypeDefinition,
         List enumlst)))) ->
     Triple (VhdEnumerationTypeDefinition,
	     abstraction typ,
	     List (List.map abstraction enumlst))
  | Triple (Vhdcase_statement_alternative,
            Double (VhdChoiceSimpleExpression,
             Double (VhdOperatorString, str)),
            Double (VhdSequentialReturn,
             Triple (Vhdreturn_statement, Str "",
              Triple (VhdNameParametersPrimary, nam,
               Triple (Vhdassociation_element, VhdFormalIndexed,
                Double (VhdActualDiscreteRange,
			Triple (VhdRange, hi, lo))))))) ->
     Quintuple (Vhdcase_statement_alternative,
	     abstraction str,
	     abstraction nam,
	     abstraction hi,
	     abstraction lo)
  | Triple(arg1,arg2,VhdNone) -> Double(abstraction arg1,abstraction arg2)
  | Triple(arg1,arg2,arg3) -> Triple(
     abstraction arg1,
     abstraction arg2,
     abstraction arg3)
(* *)
  | List [Triple (Vhdwaveform_element, Str s, VhdNone)] -> Str s
  | List (hd :: []) -> abstraction hd
  | List lst -> List (mymap abstraction lst)
  | Double (VhdTargetDotted, Str s) -> Str s
  | Double (VhdNamePrimary, Double (VhdSimpleName, Str "")) -> VhdNone
  | Double (VhdNamePrimary, Double (VhdSimpleName, Str s)) -> Str s
  | Double (VhdNamePrimary, s) -> abstraction s
(* *)
  | Double(arg1,arg2) -> Double(
     abstraction arg1,
     abstraction arg2)
  | VhdNone ->  VhdNone
  | Str s -> Str s
  | Num s -> Num s
  | Real s -> Real s
  | Char s -> Char s
  | others -> others

let unmatched = ref []
let cselst = ref []
let blocklst = ref []
  
let mkblk = function
  | List lst -> List lst
  | oth -> List [oth]

let digit = function
  | '0'..'9' -> true
  | _ -> false

type match2_args = {
  chan: out_channel;
  indent: string;
  liblst: (((vhdintf*string)*vhdintf) list) ref;
  subtype: ((string*(string*vhdintf)) list) ref;
  localp: ((string*vhdintf) list) ref;
  matched: vhdintf list ref;
  fns: ((string*(vhdintf*string)) list) ref;
  siglst: (string*vhdintf) list ref;
}

type const =
  | Unmatched
  | Symbolic
  | Int of int
  | Up of int*int
  | Down of int*int

let dummyrng hi lo = (Double(VhdRange,
		       (Triple (VhdDecreasingRange,
				Double (VhdIntPrimary, Num (string_of_int hi)),
				Double (VhdIntPrimary, Num (string_of_int lo))))))

let lookuprng args nam =
  if List.mem_assoc nam !(args.siglst) then
    List.assoc nam !(args.siglst)
  else
    dummyrng 0 0

let rec const_expr args = function
  | Str nam ->
     if List.mem_assoc nam !(args.localp) then
       begin
	 let param = List.assoc nam !(args.localp) in
	 const_expr args param
       end
     else
       begin
	 unmatched := (Str nam) :: !unmatched;
	 Symbolic
       end
  | Double (VhdIntPrimary, Num num) -> Int (int_of_string num)
  | Double (VhdRange, expr) -> const_expr args expr
  | Triple (VhdDecreasingRange, lft, rght) ->
     (match (const_expr args lft, const_expr args rght) with
      | Int lft, Int rght -> Down(lft, rght)
      | oth -> failwith "const_expr_range")
  | Double (VhdParenthesedPrimary, expr) -> const_expr args expr
  | Triple (VhdAddSimpleExpression, lft, rght) ->
     (match (const_expr args lft, const_expr args rght) with
      | Int lft, Int rght -> Int(lft + rght)
      | oth -> failwith "const_expr_add")
  | Triple (VhdSubSimpleExpression, lft, rght) ->
     (match (const_expr args lft, const_expr args rght) with
      | Int lft, Int rght -> Int(lft - rght)
      | oth -> failwith "const_expr_sub")
  | Double (VhdActualDiscreteRange, expr) -> const_expr args expr
  | oth -> blocklst := ("",oth,[]) :: !blocklst; Unmatched

let rec match2' (args:match2_args) = function
  | List lst12 ->
    output_string args.chan (args.indent^"begin\n");
    List.iter (output_string args.chan args.indent; match2 {args with indent=args.indent^"  "}) lst12;
    output_string args.chan (args.indent^"end\n"^args.indent);
  | Str str -> output_string args.chan str
  | Triple (VhdEqualRelation, lft, rght) ->
     (match2 args) lft; output_string args.chan " == "; (match2 args) rght
  | Triple (VhdNotEqualRelation, lft, rght) ->
     (match2 args) lft; output_string args.chan " != "; (match2 args) rght
  | Triple (VhdLessRelation, lft, rght) ->
     (match2 args) lft; output_string args.chan " < "; (match2 args) rght
  | Triple (VhdGreaterRelation, lft, rght) ->
     (match2 args) lft; output_string args.chan " > "; (match2 args) rght
  | Triple (VhdAddSimpleExpression, lft, rght) ->
      (match2 args) lft; output_string args.chan " + "; (match2 args) rght
  | Triple (VhdSubSimpleExpression, lft, rght) ->
      (match2 args) lft; output_string args.chan " - "; (match2 args) rght
  | Triple (VhdOrLogicalExpression, lft, rght) ->
      (match2 args) lft; output_string args.chan " | "; (match2 args) rght
  | Triple (VhdXorLogicalExpression, lft, rght) ->
      (match2 args) lft; output_string args.chan " ^ "; (match2 args) rght
  | Triple (VhdShiftLeftLogicalExpression, lft, rght) ->
      (match2 args) lft; output_string args.chan " << "; (match2 args) rght
  | Triple (VhdShiftRightLogicalExpression, lft, rght) ->
      (match2 args) lft; output_string args.chan " >> "; (match2 args) rght
  | Double (VhdParenthesedPrimary, x) ->
     output_string args.chan "(";
     match2 args x;
     output_string args.chan ")"
  | Triple (VhdAndLogicalExpression, lft, rght) ->
      (match2 args) lft; output_string args.chan " && "; (match2 args) rght
  | Triple (VhdExpFactor, Double (VhdIntPrimary, Num "2"), expr) ->
      output_string args.chan "1 << "; (match2 args) expr
  | Double (VhdCondition, VhdNone) -> output_string args.chan ("/* cond none */");
  | Double (VhdCondition,
            Triple (VhdNameParametersPrimary, Str rising_edge,
		    Triple (Vhdassociation_element, VhdFormalIndexed,
			    Double (VhdActualExpression, Str clk)))) ->
     output_string args.chan ("@(posedge "^clk)
  | Triple (VhdNameParametersPrimary,
            Str src,
            Triple (Vhdassociation_element,
                    VhdFormalIndexed,
                    Double (VhdActualExpression, idx))) ->
	    output_string args.chan (src^"[");
	    match2 args idx;
	    output_string args.chan ("]");
  | Triple (VhdNameParametersPrimary, Str src,
	    Triple (Vhdassociation_element, VhdFormalIndexed,
		    Double (VhdActualDiscreteRange, range))) ->
	    output_string args.chan (src);
	    match2 args range
  | Double (VhdCondition, x) -> output_string args.chan "("; (match2 args) x; output_string args.chan ")"
  | Double (VhdOperatorString, Str v) when digit v.[0] ->
     output_string args.chan (string_of_int (String.length v)^"'b"^v);
  | Double (VhdCharPrimary, Char ch) -> output_string args.chan (" 1'b"^String.make 1 ch)
  | Triple (VhdNameParametersPrimary,
	    Double (VhdAttributeName,
		    Triple (Vhdattribute_name,
			    Double (VhdSuffixSimpleName, Str typ), Str "succ")),
	    Triple (Vhdassociation_element, VhdFormalIndexed,
					    Double (VhdActualExpression, Str state))) ->
					    output_string args.chan (state^"+1");
  | Triple (VhdNameParametersPrimary, fn, List params) ->
     (match2 args) fn;
     let delim = ref "(" in List.iter (fun itm -> output_string args.chan !delim; match2 args itm; delim := ", ") params;
     output_string args.chan ")";
  | Triple (Vhdassociation_element,
                    VhdFormalIndexed,
                    Double (VhdActualExpression,
			    Str src)) -> output_string args.chan src

  | Double (VhdIntPrimary, Num n) -> output_string args.chan (n);
    
  | Triple (Vhdassociation_element,
          VhdFormalIndexed,
          Double (VhdActualExpression,
		  Double (VhdIntPrimary, Num n))) -> output_string args.chan n
  | Double (VhdTargetDotted,
    Triple (VhdNameParametersPrimary, Str result,
     Triple (Vhdassociation_element, VhdFormalIndexed,
      Double (VhdActualExpression, Str i)))) -> output_string args.chan (result^"."^i);

  | Double (VhdSequentialVariableAssignment,
                                   Double (VhdSimpleVariableAssignment,
                                    Quadruple (Vhdsimple_variable_assignment,
					       Str "", dst, expr))) ->
   (match2 args) dst;
   output_string args.chan (" = ");
   (match2 args) expr;
   output_string args.chan (";\n"^args.indent)

  | Double (VhdSequentialSignalAssignment,
          Double (VhdSimpleSignalAssignment,
                  Quintuple
                    (Vhdsimple_signal_assignment_statement,
                     Str "", Str nam, VhdDelayNone,
                     Double (Vhdwaveform_element,
			     Double (VhdAggregatePrimary, (Triple _ as oth)))))) ->
     block_const args nam (lookuprng args nam) [oth]

  | Double (VhdSequentialIf,
                 Quintuple (Vhdif_statement, Str "",
                  cond,
                   thenstmts,
			    VhdElseNone)) ->
     output_string args.chan ("if ");
    (match2 args) cond;
    output_string args.chan ("\n"^args.indent);
    (match2  {args with indent=args.indent^"  "}) (mkblk thenstmts);
    output_string args.chan "\n";
  | Double (VhdSequentialNull, Double (Vhdnull_statement, Str "")) -> output_string args.chan "begin end"
  | Double (VhdSequentialIf,
          Quintuple (Vhdif_statement, Str "",
                     cond,
                     thenstmts,
                     Double (VhdElse,
                             elsestmts))) ->
  output_string args.chan ("if ");
  (match2 args) cond;
    output_string args.chan ("\n"^args.indent);
  (match2  {args with indent=args.indent^"  "}) (mkblk thenstmts);
  output_string args.chan " else\n";
  (match2  {args with indent=args.indent^"  "}) (mkblk elsestmts);

| Double (VhdSequentialIf,
          Quintuple (Vhdif_statement, Str "",
                     cond,
                     thenstmts,
                     Double (VhdElsif,
                             Quintuple (Vhdif_statement, Str "",
					cond',
					thenstmts',
					VhdElseNone)))) ->		
  output_string args.chan ("if ");
  (match2 args) cond;
    output_string args.chan ("\n"^args.indent);
  (match2 {args with indent=args.indent^"  "}) (mkblk thenstmts);
  output_string args.chan (args.indent^"else if "^args.indent);
  (match2 args) cond';
  output_string args.chan ("\n"^args.indent);
  (match2  {args with indent=args.indent^"  "}) (mkblk thenstmts');
   
| Double (VhdSequentialCase,
          Quintuple (Vhdcase_statement, Str "",
                     Double (VhdSelector, sel), VhdOrdinarySelection,
                     List cases)) -> output_string args.chan ("case (");
  (match2 args) sel;
  output_string args.chan ")";
  List.iter (match2 {args with indent=args.indent^"  "}) cases;
  output_string args.chan ("\n"^args.indent^"endcase\n");
  if !cselst = [] then cselst := cases;

| Double (VhdChoiceSimpleExpression,
                               case') ->
   (match2 args) case';


| Triple (Vhdcase_statement_alternative, cse, stmts) ->
  output_string args.chan ("\n"^args.indent);
   (match cse with
   | VhdChoiceOthers -> output_string args.chan "default"
   | List lst -> let delim = ref "" in List.iter (fun itm -> output_string args.chan !delim; match2 args itm; delim := ", ") lst
   | oth -> (match2 args) cse);
  output_string args.chan (":\n"^args.indent);
  match2 args (match stmts with List _ -> stmts | _ -> List [stmts])
     
  |              Double (VhdSequentialIf,
               Quintuple (Vhdif_statement, Str "",
			  cond,
			  then',else')) ->
     output_string args.chan ("if ");
    (match2 args) cond;
    output_string args.chan ("\n  "^args.indent);
    (match2  {args with indent=args.indent^"  "}) (mkblk then');
    output_string args.chan (args.indent^"else\n"^args.indent);
    (match2  {args with indent=args.indent^"  "}) (mkblk else');

  | Double (VhdElse,
                 List
                   lst51) ->
		       List.iter (match2 args) lst51;

| Double (VhdProcessVariableDeclaration,
          Quintuple (Vhdvariable_declaration, Str _false, Str nam,
           Quadruple (Vhdsubtype_indication, Str "", Str kind,
            VhdNoConstraint),
		     VhdNone)) -> (match kind with
		     | "boolean" -> output_string args.chan ("reg "^nam^";\n")
		     | oth ->  output_string args.chan ("reg /*"^oth^" */ "^nam^";\n"))

| Double (VhdProcessVariableDeclaration,
          Quintuple (Vhdvariable_declaration, Str _false, Str nam,
           Quadruple (Vhdsubtype_indication, Str "", Str kind,
            Double (VhdArrayConstraint,
             Triple (Vhdassociation_element, VhdFormalIndexed,
              range))),
		     VhdNone)) ->
   (match kind with
   | "std_ulogic_vector" -> output_string args.chan ("reg "); match2 args range;  output_string args.chan (nam^";\n")
		     | oth ->  output_string args.chan ("reg /*"^oth^" */ "^nam^";\n"))
	   
| Triple (Vhdconditional_waveform,
            Double (Vhdwaveform_element, ch), Double (VhdCondition, VhdNone)) -> (match2 args) ch;

| Triple (Vhdconditional_waveform,
          Double (Vhdwaveform_element, level),
          Double (VhdCondition, cond)) -> 
                match2 args cond;
                output_string args.chan (" ? ");
                match2 args level; 
                output_string args.chan (" : ");
(* *)
|   Double (VhdConcurrentSignalAssignmentStatement,
    Quadruple (Vhdconcurrent_signal_assignment_statement, Str "",
     Str "false",
     Double (VhdConcurrentSimpleSignalAssignment,
      Quintuple (Vhdconcurrent_simple_signal_assignment, Str nam,
       Str "false", VhdDelayNone,
       Double (Vhdwaveform_element,
        Double (VhdAggregatePrimary,
         (Triple (Vhdelement_association, VhdChoiceOthers, _) as oth))))))) ->
(*
 output_string args.chan ("assign1 "^nam^" = "); (match2 args) oth;
*)
     block_const args nam (lookuprng args nam) [oth]
(* *)
       (*
|   Double (VhdConcurrentSignalAssignmentStatement,
    Quadruple (Vhdconcurrent_signal_assignment_statement, Str "",
     Str _false,
     Double (VhdConcurrentSimpleSignalAssignment,
      Quintuple (Vhdconcurrent_simple_signal_assignment, Str nam,
       Str _false', VhdDelayNone,
       Double (Vhdwaveform_element,
        Double (VhdAggregatePrimary, oth)))))) ->

 output_string args.chan ("assign "^nam^" = "); (match2 args) oth;
  (*
     block_const args nam (lookuprng args nam) [oth]
  *)
       *)
| Double (VhdSequentialSignalAssignment,
          Double (VhdSimpleSignalAssignment,
                  Quintuple
                    (Vhdsimple_signal_assignment_statement,
                     Str "", Str dst, VhdDelayNone,
                     Double (Vhdwaveform_element,
			     expr)))) ->
   output_string args.chan (dst^" <= "); (match2 args) expr; output_string args.chan (";\n"^args.indent)
(*
| Double (VhdConcurrentSignalAssignmentStatement,
    Quadruple (Vhdconcurrent_signal_assignment_statement, Str "",
     Str "false",
     Double (VhdConcurrentSimpleSignalAssignment,
      Quintuple (Vhdconcurrent_simple_signal_assignment, Str dst,
       Str "false", VhdDelayNone,
       Double (Vhdwaveform_element,
        Double (VhdAggregatePrimary,
         Triple (Vhdelement_association, VhdChoiceOthers,
		 Double (VhdCharPrimary, Char '0')))))))) ->  output_string args.chan ("assign "^dst^";\n")
*)
| Double (VhdConcurrentSignalAssignmentStatement,
      Quadruple (Vhdconcurrent_signal_assignment_statement, Str "",
       Str _false,
       Double (VhdConcurrentSimpleSignalAssignment,
        Quintuple (Vhdconcurrent_simple_signal_assignment,
         Str dst, Str _false', VhdDelayNone,
         Double (Vhdwaveform_element, expr))))) -> output_string args.chan ("assign "^dst^" = ");
  (match2 args) expr;
    output_string args.chan (";\n"^args.indent);
  
  
| Double (VhdConcurrentProcessStatement,
	   Sextuple (Vhdprocess_statement, Str nam, Str _false,
		     Double (VhdSensitivityExpressionList, Str clk),
		     List decls,
		     Double (VhdSequentialIf,
			     Quintuple (Vhdif_statement, Str "", cond,
					Double (VhdSequentialIf,
						Quintuple (Vhdif_statement, Str "", cond',
							   rststmts,
							   Double (VhdElse,
								   stmts))),
					VhdElseNone)))) ->
   output_string args.chan ("\n"^args.indent^"always @ ( posedge "^clk^")\n");
   output_string args.chan (args.indent^"  if ");
   match2 {args with indent=args.indent^"  "} cond';
   output_string args.chan ("\n");
   match2 {args with indent=args.indent^"  "} (mkblk rststmts);
   output_string args.chan (args.indent^"  else\n");
   match2 args stmts;
| VhdInterfaceModeIn -> output_string args.chan "input wire"
| VhdInterfaceModeOut -> output_string args.chan "output wire"
| Num n -> output_string args.chan ("/* Num */'d"^n)
| Triple (Vhddesign_unit,
    List liblst,
 Double (VhdPrimaryUnit,
   Double (VhdEntityDeclaration,
   Quintuple (Vhdentity_declaration, Str modnam,
      Triple (Vhdentity_header, List [],
       List lst),
   List [], List [])))) ->
   	let delim = ref "(\n" in
	output_string args.chan ("module "^modnam);
	List.iter (fun itm ->
		   output_string args.chan (!delim^"\t");
		   match2 args itm;
		   delim := ",\n") lst;
	output_string args.chan ");\n"
| Triple (Vhddesign_unit, List liblst,
			  Double (VhdSecondaryUnit,
				  Double (VhdArchitectureBody,
					  Quintuple (Vhdarchitecture_body, Str arch, Str design, lst1, lst2)))) ->
   output_string args.chan ("/* design "^design^" */\n");
   output_string args.chan ("/* architecture "^arch^" */\n");
List.iter (match2 args) (match lst1 with List lst -> lst | _ -> [lst1]);
List.iter (match2 args) (match lst2 with List lst -> lst | _ -> [lst2]);
| Triple (Vhddesign_unit,
    List liblst,
      (Double ((VhdPrimaryUnit|VhdSecondaryUnit),
         Double ((VhdEntityDeclaration|VhdPackageDeclaration|VhdArchitectureBody|VhdPackageBody|VhdConfigurationDeclaration) as decl,
            (Quintuple ((Vhdentity_declaration|Vhdpackage_declaration|Vhdconfiguration_declaration), Str nam, _, _, _) |
                Quintuple (Vhdarchitecture_body, _, Str nam, _, _) |
                   Triple (Vhdpackage_body, Str nam, _)))) as u)) ->
   let _ = List.map (function
     | Double (VhdContextLibraryClause, List [Str first]) -> first
     | Double (VhdContextUseClause,
	       List
		 [List
		     [VhdSuffixAll;
		      Double (VhdSuffixSimpleName,
			      Double (VhdSimpleName, Str second));
		      Double (VhdSuffixSimpleName, Double (VhdSimpleName, Str third))]]) -> String.concat "." [second;third]
     | _ -> "unknown") liblst in args.liblst := ((decl,nam), u) :: !(args.liblst)
| Double (VhdBlockSubProgramDeclaration,
      Double (VhdFunctionSpecification,
       Septuple (Vhdfunction_specification,
        Double (VhdDesignatorIdentifier, Str fn),
        arg', List [], List [], Str kind, VhdUnknown))) ->
   output_string args.chan ("/* FunctionSpecification "^fn^":"^kind^" */\n");
   args.fns := (fn, (arg', kind)) :: !(args.fns);
| Double (VhdBlockSubProgramBody,
 Quadruple (Vhdsubprogram_body,
  Double (VhdFunctionSpecification,
   Septuple (Vhdfunction_specification,
    Double (VhdDesignatorIdentifier, Str fn),
    arglst, List [], List [], Str typ, VhdUnknown)),
  decls, stmts)) ->
   if List.mem_assoc fn !(args.fns) then
     begin
       match List.assoc fn !(args.fns) with
       | (fnargs,fnkind) ->
	  if List.mem_assoc fnkind !(args.subtype) then
	    begin
	      match List.assoc typ !(args.subtype) with
	      | "std_ulogic_vector", range ->
		 output_string args.chan ("      function ");
		match2 args range;
		output_string args.chan (" "^fn^";\n");
	      | _ -> output_string args.chan ("    function [???] "^fn^";\n");
	    end
	  else
	    begin
	      output_string args.chan ("    function "^fnkind^" "^fn^";\n");
	    end;
	 List.iter (function
	 | Double (VhdInterfaceObjectDeclaration,
		   Double (VhdInterfaceDefaultDeclaration,
			   Sextuple (Vhdinterface_default_declaration, Str itm,
				     dir,
				     Quadruple (Vhdsubtype_indication, Str "", Str kind,
						VhdNoConstraint),
				     VhdSignalKindDefault, VhdNone))) ->
	    if List.mem_assoc kind !(args.subtype) then
	      begin
		match List.assoc typ !(args.subtype) with
		| "std_ulogic_vector", range ->
		   output_string args.chan ("       input ");
		  match2 args range;
		  output_string args.chan (itm^";\n");
		  args.siglst := (itm,range) :: !(args.siglst);
		| _ -> output_string args.chan ("       input [???] "^itm^";\n");
		  args.siglst := (itm,dummyrng 0 0) :: !(args.siglst);
	      end
	 | Double (VhdInterfaceObjectDeclaration,
		   Double (VhdInterfaceDefaultDeclaration,
			   Sextuple (Vhdinterface_default_declaration, Str itm,
				     dir,
				     Quadruple (Vhdsubtype_indication, Str "", Str kind,
						Double (VhdArrayConstraint,
							Triple (Vhdassociation_element, VhdFormalIndexed, range))),
				     VhdSignalKindDefault, VhdNone))) ->
            output_string args.chan ("       input ");
	   match2 args range;
	   output_string args.chan (itm^";\n");
	   args.siglst := (itm,range) :: !(args.siglst);
	 | oth -> failwith "fnargs") (match fnargs with List lst -> lst | oth -> [oth]);
	 match2 args decls;
	 output_string args.chan ("       begin\n");
	 match2 args stmts;
	 output_string args.chan ("       end\n");
	 output_string args.chan ("    endfunction \n");
     end
   else
       output_string args.chan ("/* VhdBlockSubProgramBody "^fn^": interface not found */\n");
| Double (VhdBlockSignalDeclaration,
 Quintuple (Vhdsignal_declaration, Str signal,
  Quadruple (Vhdsubtype_indication, Str "", Str typ,
   Double (VhdArrayConstraint,
    Triple (Vhdassociation_element, VhdFormalIndexed, range))),
  VhdSignalKindDefault, VhdNone)) ->
   output_string args.chan ("reg ");
   match2 args range;
   output_string args.chan (signal^";\n");
   args.siglst := (signal,range) :: !(args.siglst);
| Double (VhdBlockSignalDeclaration,
	    Quintuple (Vhdsignal_declaration, Str signal,
		       Quadruple (Vhdsubtype_indication, Str "", typ,
				  VhdNoConstraint),
		       VhdSignalKindDefault, VhdNone)) ->
   output_string args.chan ("reg "^signal^";\n");
   args.siglst := (signal,dummyrng 0 0) :: !(args.siglst);
| Double (VhdBlockSignalDeclaration,
 Quintuple (Vhdsignal_declaration, Str signal,
  Quadruple (Vhdsubtype_indication, Str "", Str "natural",
   Double (VhdRangeConstraint,
    Triple (VhdIncreasingRange, Double (VhdIntPrimary, Num num),
     expr))),
  VhdSignalKindDefault, VhdNone)) ->
   let hi = match const_expr args expr with
     | Int hi -> hi
     | oth -> failwith "Invalid natural range" in
   let hi = maxidx hi and lo = int_of_string num in
   output_string args.chan ("reg ["^string_of_int hi^":"^num^"] "^signal^";\n");
   args.siglst := (signal,dummyrng hi lo) :: !(args.siglst);
| Double (VhdBlockSubTypeDeclaration,
	  Triple (Vhdsubtype_declaration, Str typ,
		  Quadruple (Vhdsubtype_indication, Str "", kind,
			     Double (VhdRangeConstraint,
				     Triple (VhdIncreasingRange, lo, hi))))) ->
   output_string args.chan ("/* SubTypeDeclaration_range "^typ^" */\n");
| Triple (VhdEnumerationTypeDefinition, Str typ, List enumlst) ->
   let hi = maxidx (List.length enumlst) and lo = 0 in
   output_string args.chan ("wire ["^string_of_int hi^":0]");
   let delim = ref " " in
   List.iteri (fun ix itm ->
     output_string args.chan !delim;
     match2 args itm;
     output_string args.chan ("="^string_of_int ix);
     delim := ",\n"^args.indent;
     match itm with
     | Str id -> 
	args.siglst := (id,dummyrng hi lo) :: !(args.siglst);
     | _ -> ()) enumlst;
   output_string args.chan ";\n";
| Double (VhdIdentifierEnumeration, Str enum) -> output_string args.chan enum
| Double (VhdRange, Triple (VhdDecreasingRange, hi, lo)) ->
   output_string args.chan ("[");
   match2 args hi;
   output_string args.chan (":");
   match2 args lo;
   output_string args.chan ("] ");
| Triple (VhdDecreasingRange, hi, lo) ->
   match2 args hi;
   output_string args.chan (":");
   match2 args lo;
| Double (VhdBlockConstantDeclaration,
	  Quadruple (Vhdconstant_declaration, Str nam,
		     Quadruple (Vhdsubtype_indication, Str "", kind,
				Double (VhdArrayConstraint,
					Triple (Vhdassociation_element, VhdFormalIndexed,
						range))), Double (VhdAggregatePrimary, List lst))) ->
   block_const args nam range lst
| Triple (Str nam, Str kind, mode) ->  output_string args.chan ("Nam "^kind^":"); match2 args mode
| Double (VhdInterfaceObjectDeclaration,
	  Double (VhdInterfaceDefaultDeclaration,
		  Sextuple (Vhdinterface_default_declaration, Str port,
			    mode,
			    Quadruple (Vhdsubtype_indication, Str "", kind,
				       VhdNoConstraint),
			    VhdSignalKindDefault, VhdNone))) ->
   match2 args mode;
  output_string args.chan ("\t\t"^port);
  args.siglst := (port,dummyrng 0 0) :: !(args.siglst);
| Double (VhdInterfaceObjectDeclaration,
	  Double (VhdInterfaceDefaultDeclaration,
		  Sextuple (Vhdinterface_default_declaration, Str port,
			    mode,
			    Quadruple (Vhdsubtype_indication, Str "", Str kind,
				       Double (VhdArrayConstraint,
					       Triple (Vhdassociation_element, VhdFormalIndexed,
						       range))),
			    VhdSignalKindDefault, VhdNone))) ->
   match2 args mode;
   output_string args.chan ("\t");
   match2 args range;
   output_string args.chan ("\t"^port);
  args.siglst := (port,range) :: !(args.siglst);
(*
| Double (VhdActualExpression, Double (VhdIntPrimary, Num n)) -> output_string args.chan ("/* actual */'d"^n)
 *)
| Double (VhdActualExpression,
    (Triple (VhdNameParametersPrimary, Str "to_unsigned",
     List params) as func)) -> match2 args func
| Double (VhdActualDiscreteRange, range) -> match2 args range
| Double (VhdBlockSubTypeDeclaration,
      Triple (Vhdsubtype_declaration, Str typ,
       Quadruple (Vhdsubtype_indication, Str "", Str kind,
        Double (VhdArrayConstraint,
		Triple (Vhdassociation_element, VhdFormalIndexed, rng))))) ->
   args.subtype := (typ, (kind,rng)) :: !(args.subtype);
   output_string args.chan ("/* SubTypeDeclaration "^typ^":"^kind^":");
   match2 args rng;
   output_string args.chan " */\n"
| Double (VhdBlockConstantDeclaration,
      Quadruple (Vhdconstant_declaration, Str nam,
       Quadruple (Vhdsubtype_indication, Str "", kind,
        Double (VhdArrayConstraint,
         Triple (Vhdassociation_element, VhdFormalIndexed,
          range))), cexpr)) ->
   output_string args.chan ("localparam ");
   match2 args range;
   output_string args.chan (" "^nam^" = ");
   match2 args cexpr;
   output_string args.chan ";\n";
   args.localp := (nam,cexpr) :: !(args.localp)
| Double (VhdBlockConstantDeclaration,
      Quadruple (Vhdconstant_declaration, Str nam,
       Quadruple (Vhdsubtype_indication, Str "", typ,
		  VhdNoConstraint), expr)) ->
     output_string args.chan ("localparam "^nam^" = ");
     match2 args expr;
     output_string args.chan (";\n");
   args.localp := (nam,expr) :: !(args.localp)
| Triple (VhdMultTerm, lft, rght) ->
   match2 args lft;
   output_string args.chan (" * ");
   match2 args rght;
| Triple (VhdDivTerm, lft, rght) ->
   match2 args lft;
   output_string args.chan (" / ");
   match2 args rght;
| Double (VhdNotFactor, arg) ->
   output_string args.chan (" ~ ");
   match2 args arg;
   
| Double (VhdAttributeName,
     Triple (Vhdattribute_name, Double (VhdSuffixSimpleName, Str typ),
	     Str attr)) ->
   (match attr with
    | "length" ->
       if List.mem_assoc typ !(args.subtype) then
	 begin
	   match List.assoc typ !(args.subtype) with
	   | "std_ulogic_vector",
	     Double (VhdActualDiscreteRange,
		     Double (VhdRange,
			     Triple (VhdDecreasingRange, hi, lo))) ->
	      output_string args.chan ("((");
	      match2 args hi;
	      output_string args.chan (")-(");
	      match2 args lo;
	      output_string args.chan (")+1)");
	   | _ -> output_string args.chan ("vhdattr_"^attr^"("^typ^")");
	 end
       else
	 output_string args.chan ("vhdattr_"^attr^"("^typ^")");
    | _ -> output_string args.chan ("vhdattr_"^attr^"("^typ^")"))

|
  Double (VhdSequentialReturn,
   Triple (Vhdreturn_statement, Str "", arg))
   -> output_string args.chan "return ";
match2 args arg;

| Double (VhdSequentialLoop,
   Quadruple (Vhdloop_statement, Str "",
    Double (VhdForLoop,
     Triple (Vhdparameter_specification, Str i, range)), stmt))
-> output_string args.chan ("for ("^i^"= ...)");
match2 args stmt

| Double (VhdSequentialAssertion,
   Triple (Vhdassertion_statement, Str _,
    Quadruple (Vhdassertion, cond, arange, VhdNone)))
   -> output_string args.chan ("/* assert ");
      match2 args cond;
output_string args.chan (" */");
| Double (VhdSubProgramVariableDeclaration,
   Quintuple (Vhdvariable_declaration, Str _false_, Str nam,
    Quadruple (Vhdsubtype_indication, Str _, Str kind,
     Double (VhdArrayConstraint,
      Triple (Vhdassociation_element, VhdFormalIndexed,
       Double (VhdActualExpression,
        Double (VhdAttributeName,
         Triple (Vhdattribute_name,
          Double (VhdSuffixSimpleName, Str _old_crc_), Str _range_)))))),
    VhdNone)) ->
   (match kind with
   | "std_ulogic_vector" -> output_string args.chan ("reg "^_range_^nam^";\n")
		     | oth ->  output_string args.chan ("reg /*"^oth^" */ "^nam^";\n"))

 | Double (VhdSubProgramAliasDeclaration,
    Quintuple (Vhdalias_declaration,
     Double (VhdDesignatorIdentifier, Str vec1),
     Quadruple (Vhdsubtype_indication, Str "", Str "std_ulogic_vector",
      Double (VhdArrayConstraint,
       Triple (Vhdassociation_element, VhdFormalIndexed,
        Double (VhdActualExpression,
         Double (VhdAttributeName,
          Triple (Vhdattribute_name, Double (VhdSuffixSimpleName, Str vec2),
           Str range)))))),
     Str vec3, VhdNone)) -> output_string args.chan ("/* alias "^vec1^" "^vec2^" "^vec3^" "^range^" */\n");

| Double (VhdSubProgramVariableDeclaration,
          Quintuple (Vhdvariable_declaration, Str _false, Str nam,
           Quadruple (Vhdsubtype_indication, Str "", Str kind,
            VhdNoConstraint),
		     VhdNone)) -> (match kind with
		     | "boolean" -> output_string args.chan ("reg "^nam^";\n")
		     | oth ->  output_string args.chan ("reg /*"^oth^" */ "^nam^";\n"))

 |									 
  Double (VhdSubProgramVariableDeclaration,
          Quintuple (Vhdvariable_declaration, Str _false, Str nam,
           Quadruple (Vhdsubtype_indication, Str "", Str kind,
            Double (VhdArrayConstraint,
             Triple (Vhdassociation_element, VhdFormalIndexed,
              range))),
		     VhdNone)) ->
   (match kind with
   | "std_ulogic_vector" -> output_string args.chan ("reg "); match2 args range;  output_string args.chan (nam^";\n")
		     | oth ->  output_string args.chan ("reg /*"^oth^" */ "^nam^";\n"))
| Triple (Vhdassociation_element, VhdFormalIndexed,
 Double (VhdActualExpression,
  Triple (VhdNameParametersPrimary, Str input,
   Triple (Vhdassociation_element, VhdFormalIndexed,
                                   Double (VhdActualExpression, Str i))))) -> output_string args.chan (input^"("^i^")")

| oth -> unmatched := oth :: !unmatched

and match2 args pat =
  args.matched := pat :: !(args.matched);
  match2' args pat

and block_const args nam range lst =
  blocklst := (nam,range,lst) :: !blocklst;
  match const_expr args range with
    | Down(hi, lo) ->
       let delim = ref "" in
       output_string args.chan ("wire ");
       match2 args range;
       output_string args.chan (nam^" = ");
       for i = hi downto lo do
	 let others = ref None and choice = ref None in
	 List.iter (function
		     | Triple (Vhdelement_association, VhdChoiceOthers, Double (VhdCharPrimary, Char ch)) ->
			others := Some (int_of_char ch - int_of_char '0');
		     | Triple (Vhdelement_association,
			       List lst',
			       Double (VhdCharPrimary, Char ch)) ->
			List.iter (function
				    | Double (VhdChoiceSimpleExpression, Double (VhdIntPrimary, Num num)) ->
				       if (i == int_of_string num) then
					 begin
					   if (!debug) then Printf.printf "Choice %d,%s,%c\n" i num ch;
					   choice := Some (int_of_char ch - int_of_char '0');
					 end
				    | oth -> unmatched := oth :: !unmatched) lst';
		     | oth -> blocklst := (nam,oth,[]) :: !blocklst) lst;
	 let chosen = match (!choice,!others) with
	   | Some n, _ -> n
	   | None, Some n -> n
	   | None, None -> failwith "No others choice found in constant init" in
	 output_string args.chan (!delim^"("^string_of_int chosen^"<<"^string_of_int i^")");
	 delim := "|";
       done;
       output_string args.chan (";\n");
   | oth  -> failwith "const_expr_range"

let dump nam lst =
  let args = {chan=open_out nam;
	      indent="";
	      liblst=ref [];
	      subtype=ref [];
	      localp=ref [];
	      matched = ref [];
	      fns = ref [];
	      siglst = ref []} in
  Printexc.record_backtrace true;
  List.iter (match2 args) lst;
  output_string args.chan ("\nendmodule\n");
  close_out args.chan;
  Printexc.print_backtrace stderr;
  args
