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
;;  
let rec match2 = function
  | Str str -> print_string str
  | Triple (VhdEqualRelation, lft, rght) ->
     match2 lft; print_string "="; match2 rght
  | Triple (VhdOrLogicalExpression, lft, rght) ->
      match2 lft; print_string "||"; match2 rght
  | Double (VhdParenthesedPrimary, x) -> print_string "("; match2 x; print_string ")"
  | Triple (VhdAndLogicalExpression, lft, rght) ->
      match2 lft; print_string "&&"; match2 rght
  | Double (VhdCondition, VhdNone) -> print_endline ("cond none");
  | Double (VhdCondition,
            Triple (VhdNameParametersPrimary, Str rising_edge,
		    Triple (Vhdassociation_element, VhdFormalIndexed,
			    Double (VhdActualExpression, Str clk)))) ->
     print_string ("@(posedge "^clk)
  | Double (VhdCondition, x) -> print_string("cond "); match2 x;
  | Double (VhdCharPrimary, Char ch) -> print_char ch
  | Triple (Vhdassociation_element,
                                     VhdFormalIndexed,
                                     Double (VhdActualExpression,
					     Str str)) ->
   print_endline ("assoc1:"^str)
| Triple (Vhdassociation_element,
          VhdFormalIndexed,
          Double (VhdActualExpression,
		  Double (VhdIntPrimary, Num n))) -> print_endline ("assoc2:"^n)
|
                                  Double (VhdSequentialVariableAssignment,
                                   Double (VhdSimpleVariableAssignment,
                                    Quadruple (Vhdsimple_variable_assignment,
					       Str "", Str dst, expr))) ->
   print_string (dst^" <= "); match2 expr; print_string ";"

| Double (VhdSequentialSignalAssignment,
                                   Double (VhdSimpleSignalAssignment,
                                    Quintuple
                                     (Vhdsimple_signal_assignment_statement,
                                     Str "", Str dst, VhdDelayNone,
                                     Double (Vhdwaveform_element,
					     Str src)))) -> print_endline (dst^" := "^src^";")
|
                                     Triple (VhdNameParametersPrimary,
                                      Str fn,
                                      List
					lst23) ->
  print_string (fn^"(");
  List.iter match2 lst23;
  print_string (fn^"(")

| Double (VhdSequentialSignalAssignment,
                      Double (VhdSimpleSignalAssignment,
                       Quintuple (Vhdsimple_signal_assignment_statement,
                        Str "", Str dst, VhdDelayNone,
                        Double (Vhdwaveform_element,
				Double (VhdCharPrimary, Char ch))))) -> print_endline (dst^" <= 1'b"^String.make 1 ch^";")
|
                     Double (VhdSequentialIf,
                      Quintuple (Vhdif_statement, Str "",
                       Double (VhdCondition,
                        Triple (VhdEqualRelation, Str mii_rx_frame_i,
                         Double (VhdCharPrimary, Char ch))),
                       List
                        lst15,
                       Double (VhdElse,
                        List
                          lst17))) -> print_endline ("assign:");
		       List.iter match2 lst15;
		       List.iter match2 lst17;
| Double (VhdSequentialCase,
          Quintuple (Vhdcase_statement, Str "",
                     Double (VhdSelector, sel), VhdOrdinarySelection,
                     List
                       lst19)) -> print_string ("switch(");
  match2 sel;
  print_string ")\nbegin\n";
  List.iter match2 lst19;
  print_string ")\nend\n";

| Double (VhdChoiceSimpleExpression,
                               case') ->
   match2 case';
  print_string (": ");


| Triple (Vhdcase_statement_alternative, cse, stmts) ->
   (match cse with
   | VhdChoiceOthers -> print_string "default: "
   | List cselst -> List.iter match2 cselst
   | oth -> match2 cse);
  (match stmts with
  | List lst12 ->
    print_string "\nbegin ";
    List.iter match2 lst12;
    print_string "end\n";
  | oth -> match2 oth)
|                 Double (VhdSequentialIf,
                 Quintuple (Vhdif_statement, Str "",
                  cond,
                  List
                   thenlst,
			    VhdElseNone)) ->
 print_endline ("if (");
    match2 cond;
    print_string ") begin ";
    List.iter match2 thenlst;
    print_string "end;\n";

|
                          Double (VhdSequentialIf,
                           Quintuple (Vhdif_statement, Str "",
                            Double (VhdCondition,
                             Triple (VhdEqualRelation, Str mii_rx_error_i,
                              Double (VhdCharPrimary, Char ch))),
                            Double (VhdSequentialSignalAssignment,
                             Double (VhdSimpleSignalAssignment,
                              Quintuple
                               (Vhdsimple_signal_assignment_statement,
                               Str "", Str rx_state, VhdDelayNone,
                               Double (Vhdwaveform_element, Str _RX_ERROR)))),
				      VhdElseNone)) -> print_endline ("seqif2:");

| Double (VhdSequentialSignalAssignment,
                               Double (VhdSimpleSignalAssignment,
                                Quintuple
                                 (Vhdsimple_signal_assignment_statement,
                                 Str "", Str rx_frame_check_sequence,
                                 VhdDelayNone,
                                 Double (Vhdwaveform_element,
                                  Triple (VhdNameParametersPrimary,
                                   Str update_crc32,
					  List lst8))))) -> print_endline ("segasgn3:");
		       List.iter match2 lst8;
|
                              Double (VhdSequentialIf,
                               Quintuple (Vhdif_statement, Str "",
                                Double (VhdCondition,
                                 Triple (VhdLessRelation,
                                  Str rx_frame_size,
                                  Double (VhdAttributeName,
                                   Triple (Vhdattribute_name,
                                    Double (VhdSuffixSimpleName,
                                     Str t_rx_frame_size),
                                    Str high)))),
                                Double (VhdSequentialSignalAssignment,
                                 Double (VhdSimpleSignalAssignment,
                                  Quintuple
                                   (Vhdsimple_signal_assignment_statement,
                                   Str "", Str rx_frame_size', VhdDelayNone,
                                   Double (Vhdwaveform_element,
                                    Triple (VhdAddSimpleExpression,
                                     Str rx_frame_size'',
                                     Double (VhdIntPrimary, Num n)))))),
					  VhdElseNone)) -> print_endline ("seqif2:");

| Double (VhdSequentialIf,
                                   Quintuple (Vhdif_statement, Str "",
                                    Double (VhdCondition,
                                     Triple (VhdEqualRelation,
                                      Str rx_mac_address_byte,
                                      Double (VhdOperatorString, Str "000"))),
                                    Double (VhdSequentialIf,
                                     Quintuple (Vhdif_statement, Str "",
                                      Double (VhdCondition,
                                       Triple (VhdEqualRelation,
                                        Triple (VhdNameParametersPrimary,
                                         Str mii_rx_data_i,
                                         Triple (Vhdassociation_element,
                                          VhdFormalIndexed,
                                          Double (VhdActualExpression,
                                           Double (VhdIntPrimary, Num n)))),
                                        Double (VhdCharPrimary, Char ch))),
                                      List
                                       lst16,
                                      VhdElseNone)),
                                    Double (VhdElsif,
                                     Quintuple (Vhdif_statement, Str "",
                                      Double (VhdCondition,
                                       Triple (VhdEqualRelation,
                                        Str rx_is_group_address,
                                        Double (VhdCharPrimary, Char ch'))),
                                      Double (VhdSequentialIf,
                                       Quintuple (Vhdif_statement, Str "",
                                        Double (VhdCondition,
                                         Triple (VhdNotEqualRelation,
                                          Str mii_rx_data_i',
                                          Triple (VhdNameParametersPrimary,
                                           Str extract_byte,
                                           List lst6))),
                                        Double
                                         (VhdSequentialSignalAssignment,
                                         Double (VhdSimpleSignalAssignment,
                                          Quintuple
                                           (Vhdsimple_signal_assignment_statement,
                                           Str "", Str rx_state,
                                           VhdDelayNone,
                                           Double (Vhdwaveform_element,
                                            Str _RX_ERROR)))),
                                        VhdElseNone)),
						VhdElseNone)))) -> print_endline ("seqif4:");
		       List.iter match2 lst16;
		       List.iter match2 lst6;

|
                                  Double (VhdSequentialSignalAssignment,
                                   Double (VhdSimpleSignalAssignment,
                                    Quintuple
                                     (Vhdsimple_signal_assignment_statement,
                                     Str "", Str rx_mac_address_byte,
                                     VhdDelayNone,
                                     Double (Vhdwaveform_element,
                                      Triple (VhdAddSimpleExpression,
                                       Str rx_mac_address_byte',
                                       Double (VhdOperatorString, Str "001")))))) -> print_endline ("seqasgn5:");


|
                                        Double (VhdSequentialIf,
                                         Quintuple (Vhdif_statement, 
                                          Str "",
                                          Double (VhdCondition,
                                           Triple (VhdNotEqualRelation,
                                            Str mii_rx_data_i,
                                            Triple (VhdNameParametersPrimary,
                                             Str extract_byte,
                                             List lst7))),
                                          Double
                                           (VhdSequentialSignalAssignment,
                                           Double (VhdSimpleSignalAssignment,
                                            Quintuple
                                             (Vhdsimple_signal_assignment_statement,
                                             Str "", Str rx_state,
                                             VhdDelayNone,
                                             Double (Vhdwaveform_element,
                                              Str _RX_ERROR)))),
						    VhdElseNone)) -> print_endline ("seqif5:");
		       List.iter match2 lst7;

|
                         Double (VhdSequentialIf,
                          Quintuple (Vhdif_statement, Str "",
                           Double (VhdCondition,
                            Triple (VhdOrLogicalExpression,
                             Double (VhdParenthesedPrimary,
                              Triple (VhdOrLogicalExpression,
                               Double (VhdParenthesedPrimary,
                                Triple (VhdOrLogicalExpression,
                                 Triple (VhdEqualRelation,
                                  Str mii_rx_error_i,
                                  Double (VhdCharPrimary, Char ch)),
                                 Triple (VhdNotEqualRelation,
                                  Str rx_frame_check_sequence,
                                  Str _CRC32_POSTINVERT_MAGIC))),
                               Triple (VhdLessRelation, Str rx_frame_size,
                                Triple (VhdAddSimpleExpression,
                                 Str _MIN_FRAME_DATA_BYTES,
                                 Str _CRC32_BYTES)))),
                             Triple (VhdGreaterRelation, Str rx_frame_size',
                              Triple (VhdAddSimpleExpression,
                               Str _MAX_FRAME_DATA_BYTES, Str _CRC32_BYTES')))),
                           Double (VhdSequentialSignalAssignment,
                            Double (VhdSimpleSignalAssignment,
                             Quintuple
                              (Vhdsimple_signal_assignment_statement, 
                              Str "", Str rx_error_o, VhdDelayNone,
                              Double (Vhdwaveform_element,
                               Double (VhdCharPrimary, Char ch'))))),
				     VhdElseNone)) -> print_endline ("seqif6:");

| Double (VhdSequentialSignalAssignment,
                      Double (VhdSimpleSignalAssignment,
                       Quintuple (Vhdsimple_signal_assignment_statement,
                        Str "", Str dst, VhdDelayNone,
                        Double (Vhdwaveform_element,
				Double (VhdOperatorString, Str v))))) when digit v.[0] ->
   print_endline (dst^" <= 'b"^v^";");

|
                     Double (VhdSequentialSignalAssignment,
                      Double (VhdSimpleSignalAssignment,
                       Quintuple (Vhdsimple_signal_assignment_statement,
                        Str "", Str dst, VhdDelayNone,
                        Double (Vhdwaveform_element,
				Double (VhdIntPrimary, Num n))))) -> print_endline (dst^" <= "^n^";");
|
                     Double (VhdSequentialSignalAssignment,
                      Double (VhdSimpleSignalAssignment,
                       Quintuple (Vhdsimple_signal_assignment_statement,
                        Str "", Str dst, VhdDelayNone,
                        Double (Vhdwaveform_element,
                         Double (VhdAggregatePrimary,
                          Triple (Vhdelement_association, VhdChoiceOthers,
                           Double (VhdCharPrimary, Char ch))))))) -> print_endline (dst^" <= "^String.make 1 ch^";");

| Double (VhdSequentialIf,
                          Quintuple (Vhdif_statement, Str "",
                           Double (VhdCondition,
                            Triple (VhdEqualRelation,
                             Str mii_rx_byte_received_i,
                             Double (VhdCharPrimary, Char ch))),
                           Double (VhdSequentialCase,
                            Quintuple (Vhdcase_statement, Str "",
                             Double (VhdSelector, Str mii_rx_data_i),
                             VhdOrdinarySelection,
                             List
                              lst9)),
				     VhdElseNone)) -> print_endline ("seqif8:");
		       List.iter match2 lst9;
| Double (VhdSequentialSignalAssignment,
                               Double (VhdSimpleSignalAssignment,
                                Quintuple
                                 (Vhdsimple_signal_assignment_statement,
                                 Str "", Str dst, VhdDelayNone,
                                 Double (Vhdwaveform_element,
                                  Triple (VhdNameParametersPrimary,
                                   Double (VhdAttributeName,
                                    Triple (Vhdattribute_name,
                                     Double (VhdSuffixSimpleName,
                                      Str t_tx_state),
                                     Str succ)),
                                   Triple (Vhdassociation_element,
                                    VhdFormalIndexed,
                                    Double (VhdActualExpression,
                                     Str src))))))) -> print_endline (dst^" <=  "^src^";");


|                              Double (VhdSequentialIf,
                               Quintuple (Vhdif_statement, Str "",
                                Double (VhdCondition,
                                 Triple (VhdEqualRelation,
                                  Str tx_interpacket_gap_counter,
                                  Triple (VhdSubSimpleExpression,
                                   Str _INTERPACKET_GAP_BYTES,
                                   Double (VhdIntPrimary, Num n)))),
                                List
                                 lst33,
                                Double (VhdElse,
                                 Double (VhdSequentialSignalAssignment,
                                  Double (VhdSimpleSignalAssignment,
                                   Quintuple
                                    (Vhdsimple_signal_assignment_statement,
                                    Str "", Str tx_interpacket_gap_counter',
                                    VhdDelayNone,
                                    Double (Vhdwaveform_element,
                                     Triple (VhdAddSimpleExpression,
                                      Str tx_interpacket_gap_counter'',
                                      Double (VhdIntPrimary, Num n'))))))))) -> print_endline ("seqif':");
		       List.iter match2 lst33;

|                                         Triple (VhdNameParametersPrimary,
                                         Str mac_address_i,
                                         Triple (Vhdassociation_element,
                                          VhdFormalIndexed,
                                          Double (VhdActualDiscreteRange,
                                           Triple (VhdRange, Num n,
                                            Num n')))) -> print_endline ("varasgn:"^n^","^n');

|                                     Double (VhdSequentialSignalAssignment,
                                      Double (VhdSimpleSignalAssignment,
                                       Quintuple
                                        (Vhdsimple_signal_assignment_statement,
                                        Str "", Str tx_mac_address_byte,
                                        VhdDelayNone,
                                        Double (Vhdwaveform_element,
                                         Double (VhdOperatorString,
                                          Str "001"))))) -> print_endline ("seqsigasgn1:");
     
  |              Double (VhdSequentialIf,
               Quintuple (Vhdif_statement, Str "",
			  cond,
			  then',else')) -> print_endline ("if (");
    match2 cond;
    print_string ") ";
    match2 then';
    print_string " else ";
    match2 else';

|                          Double (VhdAggregatePrimary,
                           Triple (Vhdelement_association, VhdChoiceOthers,
                            Double (VhdCharPrimary, Char ch))) -> print_endline ("1'b"^String.make 1 ch^";");


  | Double (VhdElse,
                 List
                   lst51) ->
		       List.iter match2 lst51;

| Double (VhdProcessVariableDeclaration,
          Quintuple (Vhdvariable_declaration, Str _false, Str nam,
           Quadruple (Vhdsubtype_indication, Str "", Str kind,
            VhdNoConstraint),
		     VhdNone)) -> (match kind with
		     | "boolean" -> print_endline ("reg "^nam^";")
		     | oth ->  print_endline ("reg /*"^oth^" */ "^nam^";"))

| Double (VhdProcessVariableDeclaration,
          Quintuple (Vhdvariable_declaration, Str _false, Str nam,
           Quadruple (Vhdsubtype_indication, Str "", Str kind,
            Double (VhdArrayConstraint,
             Triple (Vhdassociation_element, VhdFormalIndexed,
              Double (VhdActualDiscreteRange,
               Triple (VhdRange, Num hi, Num lo))))),
		     VhdNone)) ->
   (match kind with
   | "std_ulogic_vector" -> print_endline ("reg ["^hi^":"^lo^"] "^nam^";")
		     | oth ->  print_endline ("reg /*"^oth^" */ "^nam^";"))

| Triple (Vhdconditional_waveform,
            Double (Vhdwaveform_element, ch), x) -> print_endline ("wave1:"); match2 x

| Double (VhdConcurrentSignalAssignmentStatement,
      Quadruple (Vhdconcurrent_signal_assignment_statement, Str "",
       Str _false,
       Double (VhdConcurrentSimpleSignalAssignment,
        Quintuple (Vhdconcurrent_simple_signal_assignment,
         Str rx_frame_size_o, Str _false', VhdDelayNone,
         Double (Vhdwaveform_element,
          Triple (VhdNameParametersPrimary, Str std_logic_vector,
           Triple (Vhdassociation_element, VhdFormalIndexed,
            Double (VhdActualExpression,
             Triple (VhdNameParametersPrimary, Str to_unsigned,
              List
		lst53))))))))) -> print_endline ("concurrent signal assignmnt: "^rx_frame_size_o);
  List.iter match2 lst53

|     Double (VhdConcurrentSignalAssignmentStatement,
      Quadruple (Vhdconcurrent_signal_assignment_statement, Str "",
       Str _false,
       Double (VhdConcurrentConditionalSignalAssignment,
        Quintuple (Vhdconcurrent_conditional_signal_assignment,
         Str dest, Str src, VhdDelayNone,
         List lst2)))) -> print_endline (dest^" := "^src);
		       List.iter match2 lst2;

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
         VhdElseNone)))) -> print_endline ("begin:"^nam^"/* "^clk^" */");
		       List.iter match2 lst2;
		       List.iter match2 lst3;
		       List.iter match2 lst52;
		       print_endline ("end: /* "^nam^" */");
| oth -> unmatched := oth :: !unmatched
     
