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
    output_string args.chan (args.indent^"end\n");
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
  | Double (VhdCharPrimary, Char ch) -> output_string args.chan (" 1'b"^String.make 1 ch^";")
  | Triple (VhdSubSimpleExpression, Str src, n) -> output_string args.chan (src^"["); (match2 args) n; output_string args.chan "]";
  | Triple (VhdNameParametersPrimary, fn, params) ->
     (match2 args) fn;
     output_string args.chan "(";
     (match2 args) params;
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
		  Double (VhdIntPrimary, Num n))) -> output_string args.chan ("/*assoc: "^n^"*/")
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
     output_string args.chan ("if (");
    (match2 args) cond;
    output_string args.chan ") ";
    (match2  {args with indent=args.indent^"  "}) thenstmts;
    output_string args.chan "\n";
  | Double (VhdSequentialNull, Double (Vhdnull_statement, Str "")) -> output_string args.chan "begin end"
  | Double (VhdSequentialIf,
          Quintuple (Vhdif_statement, Str "",
                     cond,
                     thenstmts,
                     Double (VhdElse,
                             elsestmts))) ->
  output_string args.chan ("if (");
  (match2 args) cond;
  output_string args.chan ") ";
  (match2  {args with indent=args.indent^"  "}) thenstmts;
  output_string args.chan " else\n";
  (match2  {args with indent=args.indent^"  "}) elsestmts;

| Double (VhdSequentialIf,
          Quintuple (Vhdif_statement, Str "",
                     cond,
                     thenstmts,
                     Double (VhdElsif,
                             Quintuple (Vhdif_statement, Str "",
					cond',
					thenstmts',
					VhdElseNone)))) ->		
  output_string args.chan ("if (");
  (match2 args) cond;
  output_string args.chan ") ";
  (match2  {args with indent=args.indent^"  "}) thenstmts;
  output_string args.chan " else if (";
  (match2 args) cond';
  output_string args.chan ") ";
  (match2  {args with indent=args.indent^"  "}) thenstmts';
   
| Double (VhdSequentialCase,
          Quintuple (Vhdcase_statement, Str "",
                     Double (VhdSelector, sel), VhdOrdinarySelection,
                     List
                       lst19)) -> output_string args.chan ("switch(");
  (match2 args) sel;
  output_string args.chan ")\nbegin\n";
  List.iter (match2 args) lst19;
  output_string args.chan ")\nend\n";

| Double (VhdChoiceSimpleExpression,
                               case') ->
   (match2 args) case';
  output_string args.chan (": ");


| Triple (Vhdcase_statement_alternative, cse, stmts) ->
   (match cse with
   | VhdChoiceOthers -> output_string args.chan "default: "
   | oth -> (match2 args) cse);
  (match2 args) stmts
     
  |              Double (VhdSequentialIf,
               Quintuple (Vhdif_statement, Str "",
			  cond,
			  then',else')) -> output_string args.chan ("if (");
    (match2 args) cond;
    output_string args.chan ") ";
    (match2  {args with indent=args.indent^"  "}) then';
    output_string args.chan " else ";
    (match2  {args with indent=args.indent^"  "}) else';

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
            Double (Vhdwaveform_element, ch), x) -> output_string args.chan ("/* wave */ "); (match2 args) ch; (match2 args) x

|     Double (VhdConcurrentSignalAssignmentStatement,
      Quadruple (Vhdconcurrent_signal_assignment_statement, Str "",
       Str _false,
       Double (VhdConcurrentConditionalSignalAssignment,
        Quintuple (Vhdconcurrent_conditional_signal_assignment,
         Str dest, Str src, VhdDelayNone,
         List lst2)))) -> output_string args.chan (dest^" := "^src);
		       List.iter (match2 args) lst2;

| Double (VhdConcurrentSignalAssignmentStatement,
      Quadruple (Vhdconcurrent_signal_assignment_statement, Str "",
       Str _false,
       Double (VhdConcurrentSimpleSignalAssignment,
        Quintuple (Vhdconcurrent_simple_signal_assignment,
         Str rx_frame_size_o, Str _false', VhdDelayNone,
         Double (Vhdwaveform_element, expr))))) -> output_string args.chan ("assign "^rx_frame_size_o^" = ");
  (match2 args) expr;
    output_string args.chan (";\n"^args.indent);
  
|     Double (VhdConcurrentProcessStatement,
      Sextuple (Vhdprocess_statement, Str nam, Str _false,
       Double (VhdSensitivityExpressionList, Str clk),
       List lst2,
       Double (VhdSequentialIf,
        Quintuple (Vhdif_statement, Str "", cond,
         Double (VhdSequentialIf,
          Quintuple (Vhdif_statement, Str "", cond',
           List lst3,
           Double (VhdElse,
            List
             lst52))),
         VhdElseNone)))) -> output_string args.chan ("begin:"^nam^"/* "^clk^" */\n");
		       List.iter (match2 args) lst2;
		       List.iter (match2 args) lst3;
		       List.iter (match2 args) lst52;
		       output_string args.chan ("end: /* "^nam^" */");
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
| Double (VhdPrimaryUnit,
   Double (VhdEntityDeclaration,
   Quintuple (Vhdentity_declaration, Str modnam,
      Triple (Vhdentity_header, List [],
       List lst),
   List [], List []))) ->
   	let delim = ref "(\n" in
	output_string logfile ("module "^modnam);
	List.iter (fun itm -> output_string logfile !delim; portwrt logfile itm; delim := ",\n") lst;
	output_string logfile ");\n"
| _ -> print_endline "???"

let junk1 = function
| Double (VhdSecondaryUnit,
 Double (VhdArchitectureBody,
  Quintuple (Vhdarchitecture_body, Str "rtl", Str "framing",
   List lst1, List lst2)))
   -> lst1,lst2
| _ -> [],[]

let dumpv logfile d' =
    let (x,y) = junk1 d' in
    List.iter (match2 {chan=logfile;indent=""}) y

let dump nam d d' =    
  let logfile = open_out nam in
  verwrt logfile d;
  dumpv logfile d';
  close_out logfile
