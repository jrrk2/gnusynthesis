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
     
  | Double (VhdBlockConstantDeclaration,
      Quadruple (Vhdconstant_declaration, nam,
       Quadruple (Vhdsubtype_indication, Str "", kind,
        Double (VhdArrayConstraint,
         Triple (Vhdassociation_element, VhdFormalIndexed,
          Double (VhdActualDiscreteRange,
           Triple (VhdRange, hi, lo))))), expr)) ->
     Sextuple(
       VhdBlockConstantDeclaration,
       abstraction nam,
       abstraction kind,
       abstraction hi,
       abstraction lo,
       abstraction expr)
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
  | Double (VhdBlockConstantDeclaration,
      Quadruple (Vhdconstant_declaration, nam,
       Quadruple (Vhdsubtype_indication, Str "", typ,
        VhdNoConstraint), expr)) ->
     Quadruple(
       VhdBlockConstantDeclaration,
       abstraction nam,
       abstraction typ,
       abstraction expr)     
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
  | Double (VhdInterfaceObjectDeclaration,
       Double (VhdInterfaceDefaultDeclaration,
        Sextuple (Vhdinterface_default_declaration, vector,
         dir,
         Quadruple (Vhdsubtype_indication, Str "", kind,
          Double (VhdArrayConstraint,
           Triple (Vhdassociation_element, VhdFormalIndexed,
            Double (VhdActualDiscreteRange,
             Triple (VhdRange, hi, lo))))),
		  VhdSignalKindDefault, VhdNone))) ->
     Quintuple(
     abstraction vector,
     abstraction kind,
     abstraction dir,
     abstraction hi,
     abstraction lo)
  | Double (VhdRange, Triple (VhdDecreasingRange, Double (VhdIntPrimary, hi),
	    Double (VhdIntPrimary, lo))) ->
     Triple(VhdRange, abstraction hi, abstraction lo)
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
                    Double (VhdActualDiscreteRange,
                            Triple (VhdRange, Num n,
                                    Num n')))) -> output_string args.chan (src^"["^n^":"^n'^"]");
  | Triple (VhdNameParametersPrimary, Str src,
    Triple (Vhdassociation_element, VhdFormalIndexed,
     Double (VhdActualExpression, n))) -> output_string args.chan (src^"["); (match2 args) n; output_string args.chan "]";
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
              Double (VhdActualDiscreteRange,
               Triple (VhdRange, Num hi, Num lo))))),
		     VhdNone)) ->
   (match kind with
   | "std_ulogic_vector" -> output_string args.chan ("reg ["^hi^":"^lo^"] "^nam^";\n")
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
| Triple (Vhddesign_unit, List liblst,
			  Double (VhdSecondaryUnit,
				  Double (VhdArchitectureBody,
					  Quintuple (Vhdarchitecture_body, Str arch, Str design, List lst1, List lst2)))) ->
   output_string args.chan ("/* design "^design^" */\n");
   output_string args.chan ("/* architecture "^arch^" */\n");
List.iter (match2 args) lst1;
List.iter (match2 args) lst2;
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
    Triple (Vhdassociation_element, VhdFormalIndexed,
     Double (VhdActualDiscreteRange, Triple (VhdRange, hi, lo))))),
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
| Sextuple (VhdBlockConstantDeclaration, Str nam, Str kind, hi, lo, Double (VhdAggregatePrimary, List lst)) ->
    output_string args.chan (nam^":"^kind^":");
    let delim = ref "" in List.iter (fun itm -> output_string args.chan !delim; match2 args itm; delim := ",\n"^args.indent) lst
| Triple (Vhdelement_association, VhdChoiceOthers,
				    Double (VhdCharPrimary, Char ch)) -> ()
| Triple (Vhdelement_association,
     List lst,
     Double (VhdCharPrimary, Char ch)) -> ()
| Triple (Str nam, Str kind, mode) -> ()
| Quintuple (Str signal, Str kind, mode, hi, lo) -> ()
| Quadruple (VhdFunctionSpecification, Str fn, arg', Str kind) -> output_string args.chan (fn^":"^kind); match2 args arg'
| Quadruple (VhdBlockSubTypeDeclaration, Str typ, Str kind, expr) -> ()
| Sextuple (VhdBlockConstantDeclaration, Str nam,
 Str kind, hi, lo, Double (VhdOperatorString, op)) -> ()
| Double (VhdRange,
  Triple (VhdDecreasingRange,
   Double (VhdParenthesedPrimary,
    Triple (VhdSubSimpleExpression, Str nam,
     Double (VhdIntPrimary, hi))),
   Double (VhdIntPrimary, lo))) -> ()
| Quadruple (VhdBlockConstantDeclaration, Str nam, Str kind, expr) -> ()
| Triple (VhdSubSimpleExpression,
  Double (VhdParenthesedPrimary,
   Triple (VhdExpFactor, Double (VhdIntPrimary, num),
    Str bits)),
  Double (VhdIntPrimary, num')) -> ()
| oth -> unmatched := oth :: !unmatched

let alamode = function
| VhdInterfaceModeIn -> "input wire"
| VhdInterfaceModeOut -> "output wire"
| _ -> "unknown wire"

let portwrt logfile = function
     | Triple (Str port, Str kind, mode) ->
       output_string logfile ("\t"^alamode mode^"\t\t"^port)
     | Quintuple (Str port, Str kind, mode, Num hi, Num lo) ->
       output_string logfile ("\t"^alamode mode^"\t["^hi^":"^lo^"]\t"^port)
     | _ -> print_endline "???"
     
let verwrt logfile = function
| Triple (Vhddesign_unit,
    List liblst,
 Double (VhdPrimaryUnit,
   Double (VhdEntityDeclaration,
   Quintuple (Vhdentity_declaration, Str modnam,
      Triple (Vhdentity_header, List [],
       List lst),
   List [], List [])))) ->
   	let delim = ref "(\n" in
	output_string logfile ("module "^modnam);
	List.iter (fun itm -> output_string logfile !delim; portwrt logfile itm; delim := ",\n") lst;
	output_string logfile ");\n"
| _ -> print_endline "???"

let dumpv logfile d' =
    match2 {chan=logfile;indent=""} d';
    output_string logfile ("\nendmodule\n")
		      
let dump nam d d' =    
  let logfile = open_out nam in
  verwrt logfile d;
  dumpv logfile d';
  close_out logfile
