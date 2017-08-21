open VhdlTree
let mymap fn (x:vhdintf list) = List.map fn x

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
  | Double (VhdInterfaceObjectDeclaration,
	    Double (VhdInterfaceDefaultDeclaration,
		    Sextuple (Vhdinterface_default_declaration, signal,
			      dir,
			      Quadruple (Vhdsubtype_indication, Str "", kind,
					 VhdNoConstraint),
			      VhdSignalKindDefault, VhdNone))) ->
     Triple(
     abstraction signal,
     abstraction kind,
     abstraction dir)
  | Double (VhdBlockTypeDeclaration,
      Double (VhdFullType,
       Triple (Vhdfull_type_declaration, typ,
        Double (VhdEnumerationTypeDefinition,
         List enumlst)))) ->
     Triple (VhdEnumerationTypeDefinition,
	     abstraction typ,
	     List (List.map abstraction enumlst))
     
  | Double (VhdBlockSignalDeclaration,
	    Quintuple (Vhdsignal_declaration, nam,
		       Quadruple (Vhdsubtype_indication, Str "", typ,
				  VhdNoConstraint),
		       VhdSignalKindDefault, VhdNone)) ->
     Triple(VhdBlockSignalDeclaration,
	    abstraction nam,
	    abstraction typ)
  | Double (VhdBlockSubProgramDeclaration,
      Double (VhdFunctionSpecification,
       Septuple (Vhdfunction_specification,
        Double (VhdDesignatorIdentifier, nam),
        List arglst,
		 List [], List [], typ, VhdUnknown))) ->
     Quadruple (VhdFunctionSpecification,
	     abstraction nam,
	     List (List.map abstraction arglst),
	     abstraction typ)
  | Double (VhdBlockSubProgramBody,
      Quadruple (Vhdsubprogram_body,
		 Double (Vhdfunction_specification,
       Septuple (Vhdfunction_specification,			 
        Double (VhdDesignatorIdentifier, nam),
        List arglst,
		 List [], List [], typ, VhdUnknown)), List [], stmt)) ->
     Quintuple (VhdFunctionSpecification,
	     abstraction nam,
	     List (List.map abstraction arglst),
		abstraction typ,
		abstraction stmt)
  | Double (VhdBlockSubTypeDeclaration,
      Triple (Vhdsubtype_declaration, typnam,
       Quadruple (Vhdsubtype_indication, Str "", kind,
        Double (VhdArrayConstraint,
         Triple (Vhdassociation_element, VhdFormalIndexed,
          Double (VhdActualDiscreteRange, rng)))))) ->
     Quadruple(
     VhdBlockSubTypeDeclaration,
     abstraction typnam,
     abstraction kind,
     abstraction rng)     
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
}

let rec match2 (args:match2_args) = function
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
  | Triple (VhdOrLogicalExpression, lft, rght) ->
      (match2 args) lft; output_string args.chan " || "; (match2 args) rght
  | Double (VhdParenthesedPrimary, x) -> output_string args.chan "("; (match2 args) x; output_string args.chan ")"
  | Triple (VhdAndLogicalExpression, lft, rght) ->
      (match2 args) lft; output_string args.chan " && "; (match2 args) rght
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
                    range)) -> output_string args.chan (src); match2 args range
  | Double (VhdCondition, x) -> output_string args.chan "("; (match2 args) x; output_string args.chan ")"
  | Double (VhdOperatorString, Str v) when digit v.[0] -> output_string args.chan ("'b"^v);
  | Double (VhdCharPrimary, Char ch) -> output_string args.chan (" 1'b"^String.make 1 ch)
  | Triple (VhdSubSimpleExpression, Str src, n) -> output_string args.chan (src^"["); (match2 args) n; output_string args.chan "]";
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
  
  | Double (VhdAttributeName,
                    Triple (Vhdattribute_name,
                            Double (VhdSuffixSimpleName,
                                    Str typ),
                            Str succ)) -> output_string args.chan typ
  | Triple (Vhdassociation_element,
                    VhdFormalIndexed,
                    Double (VhdActualExpression,
			    Str src)) -> output_string args.chan src
  | Double (VhdAggregatePrimary,
            Triple (Vhdelement_association, VhdChoiceOthers,
                    Double (VhdCharPrimary, Char ch))) -> output_string args.chan ("1'b"^String.make 1 ch);
    
  | Double (VhdIntPrimary, Num n) -> output_string args.chan ("'d"^n);
    
  | Triple (Vhdassociation_element,
          VhdFormalIndexed,
          Double (VhdActualExpression,
		  Double (VhdIntPrimary, Num n))) -> output_string args.chan n
  | Double (VhdSequentialVariableAssignment,
                                   Double (VhdSimpleVariableAssignment,
                                    Quadruple (Vhdsimple_variable_assignment,
					       Str "", Str dst, expr))) ->
   output_string args.chan (dst^" <= "); (match2 args) expr; output_string args.chan (";\n"^args.indent)

  | Double (VhdSequentialSignalAssignment,
          Double (VhdSimpleSignalAssignment,
                  Quintuple
                    (Vhdsimple_signal_assignment_statement,
                     Str "", Str dst, VhdDelayNone,
                     Double (Vhdwaveform_element,
			     expr)))) ->
   output_string args.chan (dst^" <= "); (match2 args) expr; output_string args.chan (";\n"^args.indent)

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
  output_string args.chan (": ");
  if true then (match2 args) stmts else output_string args.chan "begin /* something */ end"
     
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

|     Double (VhdConcurrentSignalAssignmentStatement,
      Quadruple (Vhdconcurrent_signal_assignment_statement, Str "",
       Str _false,
       Double (VhdConcurrentConditionalSignalAssignment,
        Quintuple (Vhdconcurrent_conditional_signal_assignment,
         Str dest, Str _false', VhdDelayNone,
         List lst2)))) -> output_string args.chan ("assign "^dest^" = ");
		       List.iter (match2 args) lst2;
    output_string args.chan (";\n"^args.indent);

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
| Num n -> output_string args.chan ("'d"^n)
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
	List.iter (function
	| Triple (Str port, Str kind, mode) ->
	   output_string args.chan (!delim^"\t");
	  match2 args mode;
	  output_string args.chan ("\t\t"^port); delim := ",\n"
	| Quintuple (Str port, Str kind, mode, Num hi, Num lo) ->
	   output_string args.chan (!delim^"\t");
	   match2 args mode;
	  output_string args.chan ("\t["^hi^":"^lo^"]\t"^port); delim := ",\n"
	| oth -> unmatched := oth :: !unmatched; print_endline "???") lst;
	output_string args.chan ");\n"
| Triple (Vhddesign_unit, List liblst,
			  Double (VhdSecondaryUnit,
				  Double (VhdArchitectureBody,
					  Quintuple (Vhdarchitecture_body, Str arch, Str design, List lst1, List lst2)))) ->
   output_string args.chan ("/* design "^design^" */\n");
   output_string args.chan ("/* architecture "^arch^" */\n");
List.iter (match2 args) lst1;
List.iter (match2 args) lst2;
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
| Double (VhdBlockSubProgramBody,
 Quadruple (Vhdsubprogram_body,
  Double (VhdFunctionSpecification,
   Septuple (Vhdfunction_specification,
    Double (VhdDesignatorIdentifier, Str fn),
    arglst, List [], List [], Str typ, VhdUnknown)),
  decls, stmts)) -> ()
| Triple (VhdBlockSignalDeclaration, Str signal, typ) -> ()
| Double (VhdBlockSignalDeclaration,
 Quintuple (Vhdsignal_declaration, Str signal,
  Quadruple (Vhdsubtype_indication, Str "", Str "natural",
   Double (VhdRangeConstraint,
    Triple (VhdIncreasingRange, Double (VhdIntPrimary, num),
     Triple (VhdAddSimpleExpression,
      Double (VhdParenthesedPrimary,
       Triple (VhdAddSimpleExpression, Str nam',
        Double (VhdIntPrimary, hi))),
      Double (VhdIntPrimary, lo))))),
  VhdSignalKindDefault, VhdNone)) -> ()
| Double (VhdBlockSignalDeclaration,
 Quintuple (Vhdsignal_declaration, Str signal,
  Quadruple (Vhdsubtype_indication, Str "", Str typ,
   Double (VhdArrayConstraint,
    Triple (Vhdassociation_element, VhdFormalIndexed, range))),
  VhdSignalKindDefault, VhdNone)) -> ()
| Double (VhdBlockSubTypeDeclaration,
 Triple (Vhdsubtype_declaration, Str typ,
  Quadruple (Vhdsubtype_indication, Str "", kind,
   Double (VhdRangeConstraint,
    Triple (VhdIncreasingRange, Double (VhdIntPrimary, num),
     Triple (VhdAddSimpleExpression,
      Double (VhdParenthesedPrimary,
       Triple (VhdAddSimpleExpression, Str nam,
        Str nam')),
      Double (VhdIntPrimary, num'))))))) -> ()
| Triple (VhdEnumerationTypeDefinition, Str typ, List enumlst) ->
    let maxidx = int_of_float(ceil(log(float_of_int(List.length enumlst))/.log(2.))) - 1 in
    output_string args.chan ("wire ["^string_of_int maxidx^":0]");
    let delim = ref " " in List.iteri (fun ix itm -> output_string args.chan !delim;
				      match2 args itm;
				      output_string args.chan ("="^string_of_int ix);
				      delim := ",\n"^args.indent) enumlst;
    output_string args.chan ";\n"
| Double (VhdIdentifierEnumeration, Str enum) -> output_string args.chan enum
| Double (VhdRange, Triple (VhdDecreasingRange, Double (VhdIntPrimary, hi), Double (VhdIntPrimary, lo))) ->
   output_string args.chan ("[");
   match2 args hi;
   output_string args.chan (":");
   match2 args lo;
   output_string args.chan ("] ");
| Double (VhdBlockConstantDeclaration,
	  Quadruple (Vhdconstant_declaration, Str nam,
		     Quadruple (Vhdsubtype_indication, Str "", kind,
				Double (VhdArrayConstraint,
					Triple (Vhdassociation_element, VhdFormalIndexed,
						range))), Double (VhdAggregatePrimary, List lst))) ->
   output_string args.chan ("wire ");
  match2 args range;
  output_string args.chan (nam^" = ");
  let delim = ref "" in List.iter (function
    | Triple (Vhdelement_association, VhdChoiceOthers, Double (VhdCharPrimary, Char ch)) ->
       output_string args.chan (!delim^"ChoiceOthers "^String.make 1 ch^";\n");
      delim := ",\n"^args.indent
    | Triple (Vhdelement_association,
	      List lst',
	      Double (VhdCharPrimary, Char ch)) ->
       output_string args.chan (!delim^"element_association "^String.make 1 ch); List.iter (match2 args) lst';
      delim := ",\n"^args.indent
    | oth -> output_string args.chan (!delim^"element_other "); match2 args oth) lst
		     
| Triple (Str nam, Str kind, mode) ->  output_string args.chan ("Nam "^kind^":"); match2 args mode
| Double (VhdInterfaceObjectDeclaration,
	  Double (VhdInterfaceDefaultDeclaration,
		  Sextuple (Vhdinterface_default_declaration, Str signal,
			    mode,
			    Quadruple (Vhdsubtype_indication, Str "", Str kind,
				       Double (VhdArrayConstraint,
					       Triple (Vhdassociation_element, VhdFormalIndexed,
						       Double (VhdActualDiscreteRange,
							       Double (VhdRange, range))))),
			    VhdSignalKindDefault, VhdNone))) ->
   output_string args.chan ("Signal "^kind^":"); match2 args mode
| Double (VhdActualExpression, Double (VhdIntPrimary, Num n)) -> output_string args.chan ("'d"^n)
| Double (VhdActualExpression,
    (Triple (VhdNameParametersPrimary, Str "to_unsigned",
     List params) as func)) -> match2 args func
| Double (VhdActualDiscreteRange,
    Double (VhdRange,
     Triple (VhdDecreasingRange, Double (VhdIntPrimary, Num hi),
	     Double (VhdIntPrimary, Num lo)))) ->
   output_string args.chan ("["^hi^":"^lo^"]")
| Quadruple (VhdFunctionSpecification, Str fn, arg', Str kind) -> output_string args.chan ("FunctionSpecification "^fn^":"^kind); match2 args arg'; output_string args.chan ";\n"
| Quadruple (VhdBlockSubTypeDeclaration, Str typ, Str kind, expr) ->
   output_string args.chan ("BlockSubTypeDeclaration "^typ^":"^kind); match2 args expr; output_string args.chan ";\n"
| Double (VhdBlockConstantDeclaration,
      Quadruple (Vhdconstant_declaration, Str nam,
       Quadruple (Vhdsubtype_indication, Str "", kind,
        Double (VhdArrayConstraint,
         Triple (Vhdassociation_element, VhdFormalIndexed,
          range))), Double (VhdOperatorString, op))) ->
   output_string args.chan ("BlockConstantDeclaration operator "^nam^": "); match2 args range; output_string args.chan ";\n"
| Double (VhdRange,
  Triple (VhdDecreasingRange,
   Double (VhdParenthesedPrimary,
    Triple (VhdSubSimpleExpression, Str nam,
     Double (VhdIntPrimary, hi))),
   Double (VhdIntPrimary, lo))) -> ()
| Quadruple (VhdBlockConstantDeclaration, Str nam, Str kind, expr) -> ()
  | Double (VhdBlockConstantDeclaration,
      Quadruple (Vhdconstant_declaration, Str nam,
       Quadruple (Vhdsubtype_indication, Str "", typ,
        VhdNoConstraint), expr)) -> output_string args.chan ("BlockConstantDeclaration no constraint "^nam^": ");
| Triple (VhdSubSimpleExpression,
  Double (VhdParenthesedPrimary,
   Triple (VhdExpFactor, Double (VhdIntPrimary, num),
    Str bits)),
  Double (VhdIntPrimary, num')) -> ()
| oth -> unmatched := oth :: !unmatched

let dump nam lst =    
  let logfile = open_out nam in
  List.iter (match2 {chan=logfile;indent="";liblst=ref []}) lst;
  output_string logfile ("\nendmodule\n");
  close_out logfile
