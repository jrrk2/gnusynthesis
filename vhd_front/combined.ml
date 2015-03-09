
open VhdlTree

let rec match_combined = function
| Double(VhdBlockComponentDeclaration,Quadruple(Vhdcomponent_declaration,str0,lst0,
	lst1)) -> Quadruple(Str "0",str0,match_combined lst0,match_combined lst1)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Double(VhdAggregatePrimary,lst2))) -> Quintuple(Str "1",str0,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Double(VhdNamePrimary,Double(VhdOperatorString,str1)))) -> Quintuple(Str "2",
	str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdAggregatePrimary,
	lst1))) -> Quadruple(Str "3",str0,match_combined lst0,match_combined lst1)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdIntPrimary,
	num0))) -> Quadruple(Str "4",str0,num0,match_combined lst0)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),VhdSignalKindDefault,Double(VhdNamePrimary,Double(VhdOperatorString,str1)))) -> Quintuple(Str "5",
	str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),VhdSignalKindDefault,Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Quadruple(Str "6",
	str0,match_combined lst0,match_combined lst1)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdRangeConstraint,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1)))),
	VhdSignalKindDefault,Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Quintuple(Str "7",
	str0,num0,num1,match_combined lst0)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdRangeConstraint,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))),VhdSignalKindDefault,Double(VhdIntPrimary,num1))) -> Sextuple(Str "8",str0,
	str1,num0,num1,match_combined lst0)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,
	Double(VhdAggregatePrimary,lst1))) -> Quadruple(Str "9",str0,match_combined lst0,
	match_combined lst1)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,
	Double(VhdCharPrimary,chr0))) -> Quadruple(Str "10",str0,chr0,match_combined lst0)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,
	Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Triple(Str "11",str0,match_combined lst0)
| Double(VhdBlockSubProgramBody,Quadruple(Vhdsubprogram_body,Double(VhdFunctionSpecification,
	Septuple(Vhdfunction_specification,Double(VhdDesignatorIdentifier,str0),lst0,lst1,
	lst2,Double(VhdSimpleName,str1),VhdUnknown)),lst3,lst4)) -> Octuple(Str "307",str0,
	str1,match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3,
	match_combined lst4)
| Double(VhdBlockSubTypeDeclaration,Triple(Vhdsubtype_declaration,str0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str1),Double(VhdArrayConstraint,
	lst0)))) -> Quadruple(Str "13",str0,str1,match_combined lst0)
| Double(VhdBlockSubTypeDeclaration,Triple(Vhdsubtype_declaration,str0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str1),Double(VhdRangeConstraint,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1)))))) -> Quintuple(Str "14",
	str0,str1,num0,num1)
| Double(VhdBlockTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdArrayTypeDefinition,Double(VhdConstrainedArray,Triple(Vhdconstrained_array_definition,
	lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdArrayConstraint,lst1)))))))) -> Quintuple(Str "15",str0,str1,match_combined lst0,
	match_combined lst1)
| Double(VhdBlockTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdArrayTypeDefinition,Double(VhdConstrainedArray,Triple(Vhdconstrained_array_definition,
	lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint))))))) -> Quadruple(Str "16",str0,str1,match_combined lst0)
| Double(VhdBlockTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdArrayTypeDefinition,Double(VhdUnboundedArray,Triple(Vhdunbounded_array_definition,
	lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint))))))) -> Quadruple(Str "17",str0,str1,match_combined lst0)
| Double(VhdBlockTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdEnumerationTypeDefinition,lst0)))) -> Triple(Str "18",str0,match_combined lst0)
| Double(VhdBlockTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdRecordTypeDefinition,Triple(Vhdrecord_type_definition,lst0,Str ""))))) -> Triple(Str "19",
	str0,match_combined lst0)
| Double(VhdCharLiteralEnumeration,chr0) -> Double(Str "20",chr0)
| Double(VhdCharPrimary,chr0) -> Double(Str "21",chr0)
| Double(VhdChoiceSimpleExpression,Double(VhdCharPrimary,chr0)) -> Double(Str "22",
	chr0)
| Double(VhdChoiceSimpleExpression,Double(VhdIntPrimary,num0)) -> Double(Str "23",
	num0)
| Double(VhdChoiceSimpleExpression,Double(VhdNamePrimary,Double(VhdOperatorString,
	str0))) -> Double(Str "24",str0)
| Double(VhdChoiceSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0))) -> Double(Str "25",
	str0)
| Double(VhdConcatSimpleExpression,lst0) -> Double(Str "26",match_combined lst0)
| Double(VhdConcurrentBlockStatement,unknown) -> (Str "27")
| Double(VhdConcurrentComponentInstantiationStatement,Quintuple(Vhdcomponent_instantiation_statement,
	str0,Double(VhdInstantiatedComponent,Double(VhdSimpleName,str1)),lst0,lst1)) -> Quintuple(Str "28",
	str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdConcurrentComponentInstantiationStatement,Quintuple(Vhdcomponent_instantiation_statement,
	str0,Triple(VhdInstantiatedEntityArchitecture,Double(VhdSimpleName,str1),Str ""),
	lst0,lst1)) -> Quintuple(Str "29",str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdConcurrentGenerateStatement,Double(VhdForGenerateStatement,Sextuple(Vhdfor_generate_statement,
	str0,Str "",Triple(Vhdparameter_specification,str1,Double(VhdRange,Triple(VhdIncreasingRange,
	Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,str2))))),lst0,
	lst1))) -> Septuple(Str "30",str0,str1,str2,num0,match_combined lst0,match_combined lst1)
| Double(VhdConcurrentGenerateStatement,Double(VhdForGenerateStatement,Sextuple(Vhdfor_generate_statement,
	str0,Str "",Triple(Vhdparameter_specification,str1,Double(VhdRange,Triple(VhdIncreasingRange,
	Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str2)),Double(VhdIntPrimary,num1))))),lst0,lst1))) -> Octuple(Str "31",str0,str1,
	str2,num0,num1,match_combined lst0,match_combined lst1)
| Double(VhdConcurrentGenerateStatement,Double(VhdIfGenerateStatement,Septuple(Vhdif_generate_statement,
	str0,Str "",Double(VhdCondition,exp0),lst0,lst1,VhdGenerateElseNone))) -> Quintuple(Str "32",
	str0,match_combined lst0,match_combined lst1,match_combined exp0)
| Double(VhdConcurrentProcessStatement,Sextuple(Vhdprocess_statement,Str "",str0,Double(VhdSensitivityExpressionList,
	lst0),lst1,lst2)) -> Quintuple(Str "33",str0,match_combined lst0,match_combined lst1,
	match_combined lst2)
| Double(VhdConcurrentProcessStatement,Sextuple(Vhdprocess_statement,str0,str1,Double(VhdSensitivityExpressionList,
	lst0),lst1,lst2)) -> Sextuple(Str "34",str0,str1,match_combined lst0,match_combined lst1,
	match_combined lst2)
| Double(VhdConcurrentSignalAssignmentStatement,Quadruple(Vhdconcurrent_signal_assignment_statement,
	Str "",str0,Double(VhdConcurrentConditionalSignalAssignment,Quintuple(Vhdconcurrent_conditional_signal_assignment,
	Double(VhdTargetName,Double(VhdSimpleName,str1)),str2,VhdDelayNone,lst0)))) -> Quintuple(Str "35",
	str0,str1,str2,match_combined lst0)
| Double(VhdConcurrentSignalAssignmentStatement,Quadruple(Vhdconcurrent_signal_assignment_statement,
	Str "",str0,Double(VhdConcurrentSimpleSignalAssignment,Quintuple(Vhdconcurrent_simple_signal_assignment,
	Double(VhdTargetName,Double(VhdSimpleName,str1)),str2,VhdDelayNone,lst0)))) -> Quintuple(Str "36",
	str0,str1,str2,match_combined lst0)
| Double(VhdConcurrentSignalAssignmentStatement,Quadruple(Vhdconcurrent_signal_assignment_statement,
	Str "",str0,Double(VhdConcurrentSimpleSignalAssignment,Quintuple(Vhdconcurrent_simple_signal_assignment,
	Triple(VhdTargetNameParameters,Double(VhdSimpleName,str1),lst0),str2,VhdDelayNone,
	lst1)))) -> Sextuple(Str "37",str0,str1,str2,match_combined lst0,match_combined lst1)
| Double(VhdEntityConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdDivTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2))))) -> Quintuple(Str "38",str0,str1,str2,match_combined lst0)
| Double(VhdEntityConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdMultTerm,
	Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))))) -> Quintuple(Str "39",
	str0,str1,num0,match_combined lst0)
| Double(VhdEntityConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1))) -> Quintuple(Str "40",str0,str1,match_combined lst0,
	match_combined lst1)
| Double(VhdIdentifierEnumeration,str0) -> Double(Str "41",str0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),Double(VhdArrayConstraint,lst1)),VhdSignalKindDefault,
	Double(VhdAggregatePrimary,lst2)))) -> Quintuple(Str "308",str0,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),Double(VhdArrayConstraint,lst1)),VhdSignalKindDefault,
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))))) -> Quadruple(Str "309",str0,
	match_combined lst0,match_combined lst1)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdCharPrimary,
	chr0)))) -> Quadruple(Str "310",str0,chr0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdIntPrimary,
	num0)))) -> Quadruple(Str "311",str0,num0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))))) -> Triple(Str "312",str0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))))) -> Quadruple(Str "313",str0,str1,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdPhysicalPrimary,
	Triple(VhdPhysicalInteger,num0,str1))))) -> Quintuple(Str "314",str0,str1,num0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeInOut,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),Double(VhdArrayConstraint,lst1)),VhdSignalKindDefault,
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))))) -> Quadruple(Str "315",str0,
	match_combined lst0,match_combined lst1)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeInOut,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))))) -> Triple(Str "316",str0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeOut,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),Double(VhdArrayConstraint,lst1)),VhdSignalKindDefault,
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))))) -> Quadruple(Str "317",str0,
	match_combined lst0,match_combined lst1)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeOut,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))))) -> Triple(Str "318",str0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceSignalDeclaration,Sextuple(Vhdinterface_signal_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))))) -> Triple(Str "319",str0,match_combined lst0)
| Double(VhdNamePrimary,Double(VhdAttributeName,exp0)) -> Double(Str "53",match_combined exp0)
| Double(VhdNamePrimary,Double(VhdOperatorString,str0)) -> Double(Str "54",str0)
| Double(VhdNamePrimary,Double(VhdSimpleName,Str "")) -> (Str "55")
| Double(VhdNamePrimary,Double(VhdSimpleName,str0)) -> Double(Str "56",str0)
| Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str0))) -> Double(Str "57",
	str0)
| Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0)) -> Triple(Str "58",
	str0,match_combined lst0)
| Double(VhdPackageBodyConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str0),VhdNoConstraint),Double(VhdAggregatePrimary,lst1))) -> Quadruple(Str "320",
	str0,match_combined lst0,match_combined lst1)
| Double(VhdPackageBodySubProgramBody,Quadruple(Vhdsubprogram_body,Double(VhdFunctionSpecification,
	Septuple(Vhdfunction_specification,Double(VhdDesignatorIdentifier,str0),lst0,lst1,
	lst2,Double(VhdSimpleName,str1),VhdUnknown)),lst3,lst4)) -> Octuple(Str "321",str0,
	str1,match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3,
	match_combined lst4)
| Double(VhdPackageBodySubProgramBody,Quadruple(Vhdsubprogram_body,Double(VhdFunctionSpecification,
	Septuple(Vhdfunction_specification,Double(VhdDesignatorOperator,str0),lst0,lst1,lst2,
	Double(VhdSimpleName,str1),VhdUnknown)),lst3,lst4)) -> Octuple(Str "322",str0,str1,
	match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3,match_combined lst4)
| Double(VhdPackageBodyTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdArrayTypeDefinition,Double(VhdConstrainedArray,Triple(Vhdconstrained_array_definition,
	lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint))))))) -> Quadruple(Str "62",str0,str1,match_combined lst0)
| Double(VhdPackageComponentDeclaration,Quadruple(Vhdcomponent_declaration,str0,lst0,
	lst1)) -> Quadruple(Str "63",str0,match_combined lst0,match_combined lst1)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Double(VhdAggregatePrimary,lst2))) -> Quintuple(Str "323",str0,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdAggregatePrimary,
	lst1))) -> Quadruple(Str "324",str0,match_combined lst0,match_combined lst1)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdIntPrimary,
	num0))) -> Quadruple(Str "325",str0,num0,match_combined lst0)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdNamePrimary,
	Double(VhdOperatorString,str1)))) -> Quadruple(Str "326",str0,str1,match_combined lst0)
| Double(VhdPackageSubProgramDeclaration,Double(VhdFunctionSpecification,Septuple(Vhdfunction_specification,
	Double(VhdDesignatorIdentifier,str0),lst0,lst1,lst2,Double(VhdSimpleName,str1),VhdUnknown))) -> Sextuple(Str "327",
	str0,str1,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdPackageSubProgramDeclaration,Double(VhdFunctionSpecification,Septuple(Vhdfunction_specification,
	Double(VhdDesignatorOperator,str0),lst0,lst1,lst2,Double(VhdSimpleName,str1),VhdUnknown))) -> Sextuple(Str "328",
	str0,str1,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdPackageSubTypeDeclaration,Triple(Vhdsubtype_declaration,str0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str1),Double(VhdArrayConstraint,
	lst0)))) -> Quadruple(Str "70",str0,str1,match_combined lst0)
| Double(VhdPackageSubTypeDeclaration,Triple(Vhdsubtype_declaration,str0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str1),Double(VhdRangeConstraint,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str2)),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1)))))) -> Sextuple(Str "71",
	str0,str1,str2,num0,num1)
| Double(VhdPackageSubTypeDeclaration,Triple(Vhdsubtype_declaration,str0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str1),VhdNoConstraint))) -> Triple(Str "72",
	str0,str1)
| Double(VhdPackageSubTypeDeclaration,Triple(Vhdsubtype_declaration,str0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,str1),Double(VhdSimpleName,str2),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,
	Double(VhdCharPrimary,chr0),Double(VhdCharPrimary,chr1)))))) -> Sextuple(Str "73",
	str0,str1,str2,chr0,chr1)
| Double(VhdPackageSubTypeDeclaration,Triple(Vhdsubtype_declaration,str0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,str1),Double(VhdSimpleName,str2),VhdNoConstraint))) -> Quadruple(Str "74",
	str0,str1,str2)
| Double(VhdPackageTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdArrayTypeDefinition,Double(VhdUnboundedArray,Triple(Vhdunbounded_array_definition,
	lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint))))))) -> Quadruple(Str "75",str0,str1,match_combined lst0)
| Double(VhdPackageTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdEnumerationTypeDefinition,lst0)))) -> Triple(Str "76",str0,match_combined lst0)
| Double(VhdParenthesedPrimary,exp0) -> Double(Str "77",match_combined exp0)
| Double(VhdPrimaryUnit,Double(VhdConfigurationDeclaration,Quintuple(Vhdconfiguration_declaration,
	str0,Double(VhdSimpleName,str1),lst0,Quadruple(Vhdblock_configuration,Double(VhdBlockSpecificationName,
	Double(VhdSimpleName,str2)),lst1,lst2)))) -> Septuple(Str "304",str0,str1,str2,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdPrimaryUnit,Double(VhdEntityDeclaration,Quintuple(Vhdentity_declaration,
	str0,Triple(Vhdentity_header,lst0,lst1),lst2,lst3))) -> Sextuple(Str "78",str0,match_combined lst0,
	match_combined lst1,match_combined lst2,match_combined lst3)
| Double(VhdPrimaryUnit,Double(VhdPackageDeclaration,Quintuple(Vhdpackage_declaration,
	str0,lst0,lst1,lst2))) -> Quintuple(Str "79",str0,match_combined lst0,match_combined lst1,
	match_combined lst2)
| Double(VhdProcessFileDeclaration,Quintuple(Vhdfile_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str "")),Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Triple(Str "80",
	str0,match_combined lst0)
| Double(VhdProcessTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdFileTypeDefinition,Double(VhdSimpleName,str1))))) -> Triple(Str "81",
	str0,str1)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdArrayConstraint,lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str "")))) -> Quintuple(Str "82",str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdRangeConstraint,Double(VhdAttrNameRange,Triple(Vhdattribute_name,
	Double(VhdSuffixSimpleName,Double(VhdSimpleName,str2)),str3)))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str "")))) -> Sextuple(Str "83",str0,str1,str2,str3,match_combined lst0)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),
	Double(VhdNamePrimary,Double(VhdSimpleName,str2))))),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str "")))) -> Sextuple(Str "84",str0,str1,str2,num0,match_combined lst0)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Quadruple(Str "85",
	str0,str1,match_combined lst0)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Double(VhdNamePrimary,Double(VhdSimpleName,str2)))) -> Quintuple(Str "86",
	str0,str1,str2,match_combined lst0)
| Double(VhdSecondaryUnit,Double(VhdArchitectureBody,Quintuple(Vhdarchitecture_body,
	str0,str1,lst0,lst1))) -> Quintuple(Str "87",str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdSecondaryUnit,Double(VhdPackageBody,Triple(Vhdpackage_body,str0,lst0))) -> Triple(Str "88",
	str0,match_combined lst0)
| Double(VhdSequentialAssertion,Triple(Vhdassertion_statement,Str "",Quadruple(Vhdassertion,
	Double(VhdCondition,exp0),Double(VhdNamePrimary,Double(VhdOperatorString,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))))) -> Quadruple(Str "89",str0,str1,match_combined exp0)
| Double(VhdSequentialCase,Quintuple(Vhdcase_statement,Str "",Double(VhdSelector,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))),VhdOrdinarySelection,lst0)) -> Triple(Str "90",str0,
	match_combined lst0)
| Double(VhdSequentialCase,Quintuple(Vhdcase_statement,Str "",Double(VhdSelector,Triple(VhdLdotted,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)))),VhdOrdinarySelection,lst1)) -> Quintuple(Str "91",str0,
	str1,match_combined lst0,match_combined lst1)
| Double(VhdSequentialCase,Quintuple(Vhdcase_statement,Str "",Double(VhdSelector,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)),VhdOrdinarySelection,lst1)) -> Quadruple(Str "92",
	str0,match_combined lst0,match_combined lst1)
| Double(VhdSequentialIf,Quintuple(Vhdif_statement,Str "",Double(VhdCondition,exp0),
	lst0,Double(VhdElse,lst1))) -> Quadruple(Str "93",match_combined lst0,match_combined lst1,
	match_combined exp0)
| Double(VhdSequentialIf,Quintuple(Vhdif_statement,Str "",Double(VhdCondition,exp0),
	lst0,Double(VhdElsif,exp1))) -> Quadruple(Str "94",match_combined lst0,match_combined exp0,
	match_combined exp1)
| Double(VhdSequentialIf,Quintuple(Vhdif_statement,Str "",Double(VhdCondition,exp0),
	lst0,VhdElseNone)) -> Triple(Str "95",match_combined lst0,match_combined exp0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdDecreasingRange,Double(VhdIntPrimary,num0),Double(VhdIntPrimary,
	num1))))),lst0)) -> Quintuple(Str "96",str0,num0,num1,match_combined lst0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdDecreasingRange,Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1)))))),lst0)) -> Quintuple(Str "97",
	str0,str1,match_combined lst0,match_combined exp0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,
	Double(VhdAttributeName,exp0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),
	Double(VhdIntPrimary,num0))))),lst0)) -> Sextuple(Str "98",str0,str1,num0,match_combined lst0,
	match_combined exp0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdIntPrimary,
	num1))))),lst0)) -> Quintuple(Str "99",str0,num0,num1,match_combined lst0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,
	Double(VhdAttributeName,exp0)))))),lst0)) -> Quintuple(Str "100",str0,num0,match_combined lst0,
	match_combined exp0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)))))),lst0)) -> Quintuple(Str "101",str0,str1,num0,match_combined lst0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdParenthesedPrimary,
	exp0))))),lst0)) -> Quintuple(Str "102",str0,num0,match_combined lst0,match_combined exp0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdSubTypeRange,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,
	Str ""),Double(VhdAttributeName,exp0),VhdNoConstraint)))),lst0)) -> Quadruple(Str "103",
	str0,match_combined lst0,match_combined exp0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdWhileLoop,
	Double(VhdCondition,exp0)),lst0)) -> Triple(Str "104",match_combined lst0,match_combined exp0)
| Double(VhdSequentialNull,Double(Vhdnull_statement,Str "")) -> (Str "105")
| Double(VhdSequentialProcedureCall,Triple(Vhdprocedure_call_statement,Str "",Triple(Vhdprocedure_call,
	Double(VhdSimpleName,str0),lst0))) -> Triple(Str "106",str0,match_combined lst0)
| Double(VhdSequentialReport,Quadruple(Vhdreport_statement,Str "",Double(VhdNamePrimary,
	Double(VhdOperatorString,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1)))) -> Triple(Str "107",
	str0,str1)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Double(VhdCharPrimary,
	chr0))) -> Double(Str "108",chr0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Double(VhdIntPrimary,
	num0))) -> Double(Str "109",num0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)))) -> Double(Str "110",str0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Double(VhdParenthesedPrimary,
	exp0))) -> Double(Str "111",match_combined exp0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "112",str0,str1)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0))) -> Triple(Str "113",str0,match_combined lst0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "114",str0,str1)
| Double(VhdSequentialSignalAssignment,Double(VhdSimpleSignalAssignment,Quintuple(Vhdsimple_signal_assignment_statement,
	Str "",Double(VhdTargetName,Double(VhdSimpleName,str0)),VhdDelayNone,lst0))) -> Triple(Str "115",
	str0,match_combined lst0)
| Double(VhdSequentialSignalAssignment,Double(VhdSimpleSignalAssignment,Quintuple(Vhdsimple_signal_assignment_statement,
	Str "",Double(VhdTargetName,Double(VhdSimpleName,str0)),VhdDelayTransport,lst0))) -> Triple(Str "305",
	str0,match_combined lst0)
| Double(VhdSequentialSignalAssignment,Double(VhdSimpleSignalAssignment,Quintuple(Vhdsimple_signal_assignment_statement,
	Str "",Quadruple(VhdSelectTargetNameParameters,Double(VhdSimpleName,str0),lst0,Double(VhdSuffixSimpleName,
	Double(VhdSimpleName,str1))),VhdDelayNone,lst1))) -> Quintuple(Str "329",str0,str1,
	match_combined lst0,match_combined lst1)
| Double(VhdSequentialSignalAssignment,Double(VhdSimpleSignalAssignment,Quintuple(Vhdsimple_signal_assignment_statement,
	Str "",Triple(VhdTargetNameParameters,Double(VhdSimpleName,str0),lst0),VhdDelayNone,
	lst1))) -> Quadruple(Str "117",str0,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetName,Double(VhdSimpleName,str0)),Double(VhdAggregatePrimary,
	lst0)))) -> Triple(Str "118",str0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetName,Double(VhdSimpleName,str0)),Double(VhdCharPrimary,chr0)))) -> Triple(Str "119",
	str0,chr0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetName,Double(VhdSimpleName,str0)),Double(VhdConcatSimpleExpression,
	lst0)))) -> Triple(Str "120",str0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetName,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "121",
	str0,num0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetName,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "122",str0,str1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetName,Double(VhdSimpleName,str0)),Double(VhdNotFactor,Double(VhdParenthesedPrimary,
	exp0))))) -> Triple(Str "123",str0,match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetName,Double(VhdSimpleName,str0)),Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0))))) -> Quadruple(Str "124",
	str0,str1,num0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetName,Double(VhdSimpleName,str0)),Triple(VhdAddSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str2),lst1))))) -> Sextuple(Str "125",str0,str1,str2,match_combined lst0,
	match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetName,Double(VhdSimpleName,str0)),Triple(VhdDivTerm,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0),Double(VhdIntPrimary,num0))))) -> Quintuple(Str "126",
	str0,str1,num0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetName,Double(VhdSimpleName,str0)),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0)))) -> Quadruple(Str "127",str0,str1,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetName,Double(VhdSimpleName,str0)),Triple(VhdXorLogicalExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdOperatorString,
	str2)))))) -> Quadruple(Str "128",str0,str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Triple(VhdTargetNameParameters,Double(VhdSimpleName,str0),lst0),Double(VhdCharPrimary,
	chr0)))) -> Quadruple(Str "129",str0,chr0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Triple(VhdTargetNameParameters,Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))))) -> Quadruple(Str "130",str0,str1,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Triple(VhdTargetNameParameters,Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1)))) -> Quintuple(Str "131",str0,str1,match_combined lst0,
	match_combined lst1)
| Double(VhdSequentialWait,Quintuple(Vhdwait_statement,Str "",lst0,Double(VhdCondition,
	exp0),Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Triple(Str "132",match_combined lst0,
	match_combined exp0)
| Double(VhdSequentialWait,Quintuple(Vhdwait_statement,Str "",lst0,Double(VhdCondition,
	exp0),Double(VhdPhysicalPrimary,Triple(VhdPhysicalInteger,num0,str0)))) -> Quintuple(Str "306",
	str0,num0,match_combined lst0,match_combined exp0)
| Double(VhdSimpleName,str0) -> Double(Str "133",str0)
| Double(VhdSubProgramAliasDeclaration,Quintuple(Vhdalias_declaration,Double(VhdDesignatorIdentifier,
	str0),Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdArrayConstraint,lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2)),VhdNone)) -> Quintuple(Str "134",str0,str1,str2,match_combined lst0)
| Double(VhdSubProgramConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdAddSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "330",
	str0,num0,match_combined lst0,match_combined exp0)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdArrayConstraint,lst1)),Double(VhdAggregatePrimary,lst2))) -> Sextuple(Str "136",
	str0,str1,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdArrayConstraint,lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str "")))) -> Quintuple(Str "137",str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Double(VhdCharPrimary,chr0))) -> Quintuple(Str "138",str0,
	str1,chr0,match_combined lst0)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Quadruple(Str "139",
	str0,str1,match_combined lst0)
| Quintuple(Vhdif_statement,Str "",Double(VhdCondition,exp0),lst0,Double(VhdElse,lst1)) -> Quadruple(Str "331",
	match_combined lst0,match_combined lst1,match_combined exp0)
| Quintuple(Vhdif_statement,Str "",Double(VhdCondition,exp0),lst0,Double(VhdElsif,
	exp1)) -> Quadruple(Str "141",match_combined lst0,match_combined exp0,match_combined exp1)
| Quintuple(Vhdif_statement,Str "",Double(VhdCondition,exp0),lst0,VhdElseNone) -> Triple(Str "142",
	match_combined lst0,match_combined exp0)
| Triple(VhdAddSimpleExpression,Double(VhdIntPrimary,num0),Triple(VhdMultTerm,Double(VhdIntPrimary,
	num1),Double(VhdNamePrimary,Double(VhdSimpleName,str0)))) -> Quadruple(Str "143",
	str0,num0,num1)
| Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdIntPrimary,num0)) -> Triple(Str "144",str0,num0)
| Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Triple(Str "145",str0,str1)
| Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Triple(VhdMultTerm,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)))) -> Quadruple(Str "146",str0,str1,num0)
| Triple(VhdAddSimpleExpression,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)),Double(VhdIntPrimary,num0)) -> Quadruple(Str "147",
	str0,num0,match_combined lst0)
| Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,
	num0)) -> Triple(Str "148",num0,match_combined exp0)
| Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdIntPrimary,num0)) -> Quadruple(Str "149",str0,num0,match_combined lst0)
| Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1)) -> Quintuple(Str "150",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),
	Double(VhdParenthesedPrimary,exp1)) -> Triple(Str "151",match_combined exp0,match_combined exp1)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),
	Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdCharPrimary,
	chr0))) -> Quadruple(Str "152",str0,chr0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "153",
	str0,match_combined lst0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdParenthesedPrimary,
	exp1)) -> Triple(Str "154",match_combined exp0,match_combined exp1)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdCharPrimary,chr0))) -> Quadruple(Str "155",
	str0,chr0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdLessOrEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdAttributeName,
	exp1)))) -> Quadruple(Str "156",str0,match_combined exp0,match_combined exp1)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdNotEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1))) -> Sextuple(Str "157",str0,str1,match_combined lst0,
	match_combined lst1,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0))) -> Quadruple(Str "158",str0,chr0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdCharPrimary,chr1))) -> Quintuple(Str "159",
	str0,str1,chr0,chr1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,str2)))) -> Quintuple(Str "160",
	str0,str1,str2,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0),Double(VhdCharPrimary,chr1))) -> Sextuple(Str "161",
	str0,str1,chr0,chr1,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdGreaterOrEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdAttributeName,exp0)))) -> Quintuple(Str "162",
	str0,str1,chr0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdNotEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0))) -> Quintuple(Str "163",str0,
	str1,num0,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdCharPrimary,chr0))) -> Quintuple(Str "164",
	str0,str1,str2,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst0),Double(VhdCharPrimary,
	chr0))) -> Sextuple(Str "165",str0,str1,str2,chr0,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str3),lst1))) -> Septuple(Str "166",str0,str1,str2,str3,match_combined lst0,
	match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdNotEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str3),lst1))) -> Septuple(Str "167",str0,str1,str2,str3,match_combined lst0,
	match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),
	Double(VhdNamePrimary,Double(VhdSimpleName,str2))),Triple(VhdEqualRelation,Triple(VhdLdotted,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str3),lst1),Double(VhdNamePrimary,
	Double(VhdSimpleName,str4))),Double(VhdNamePrimary,Double(VhdSimpleName,str5)))) -> Nonuple(Str "332",
	str0,str1,str2,str3,str4,str5,match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1)) -> Quintuple(Str "169",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "170",num0,match_combined exp0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdCharPrimary,
	chr0)) -> Triple(Str "171",str0,chr0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "172",str0,num0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdOperatorString,str1))) -> Triple(Str "173",str0,str1)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Triple(Str "174",str0,str1)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdLdotted,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2)))) -> Quintuple(Str "175",str0,str1,str2,match_combined lst0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0)) -> Quadruple(Str "176",str0,str1,match_combined lst0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0))) -> Quadruple(Str "177",
	str0,str1,num0)
| Triple(VhdEqualRelation,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdCharPrimary,
	chr0)) -> Quintuple(Str "178",str0,str1,chr0,match_combined lst0)
| Triple(VhdEqualRelation,Triple(VhdModTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1)) -> Quadruple(Str "179",
	str0,num0,num1)
| Triple(VhdEqualRelation,Triple(VhdModTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdIntPrimary,num0)) -> Quadruple(Str "180",
	str0,str1,num0)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Double(VhdCharPrimary,chr0)) -> Quadruple(Str "181",str0,chr0,match_combined lst0)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Double(VhdNamePrimary,Double(VhdOperatorString,str1))) -> Quadruple(Str "182",
	str0,str1,match_combined lst0)
| Triple(VhdGreaterOrEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdAttributeName,exp0))) -> Triple(Str "183",str0,match_combined exp0)
| Triple(VhdGreaterOrEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Triple(Str "184",str0,str1)
| Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "185",str0,num0)
| Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Triple(Str "186",str0,str1)
| Triple(VhdGreaterRelation,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,
	num0)) -> Triple(Str "187",num0,match_combined exp0)
| Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "188",str0,str1,
	match_combined lst0)
| Triple(VhdLessOrEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdAttributeName,exp0))) -> Triple(Str "189",str0,match_combined exp0)
| Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "190",str0,num0)
| Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0) -> Triple(Str "191",
	str0,match_combined lst0)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),
	Double(VhdNamePrimary,Double(VhdAttributeName,exp1))) -> Triple(Str "192",match_combined exp0,
	match_combined exp1)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdCharPrimary,
	chr0)) -> Triple(Str "193",str0,chr0)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "194",str0,num0)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Triple(Str "195",str0,str1)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0))) -> Quadruple(Str "196",
	str0,str1,num0)
| Triple(VhdNotEqualRelation,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2))) -> Quintuple(Str "197",str0,str1,str2,match_combined lst0)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdParenthesedPrimary,
	exp1)) -> Triple(Str "198",match_combined exp0,match_combined exp1)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0))) -> Quadruple(Str "199",
	str0,num0,match_combined exp0)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)))) -> Quadruple(Str "200",str0,str1,match_combined exp0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdCharPrimary,chr1))) -> Quintuple(Str "201",
	str0,str1,chr0,chr1)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1))) -> Quintuple(Str "202",str0,
	str1,num0,num1)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str3)))) -> Quintuple(Str "203",str0,str1,str2,str3)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1),Double(VhdCharPrimary,
	chr1))) -> Septuple(Str "204",str0,str1,chr0,chr1,match_combined lst0,match_combined lst1)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),
	Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdCharPrimary,
	chr0))) -> Sextuple(Str "333",str0,str1,str2,chr0,match_combined lst0)
| Triple(VhdOrLogicalExpression,Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdAttributeName,exp0))),Triple(VhdGreaterRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdAttributeName,
	exp1)))) -> Quintuple(Str "206",str0,str1,match_combined exp0,match_combined exp1)
| Triple(VhdOrLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "207",
	str0,str1,match_combined lst0)
| Triple(VhdSubSimpleExpression,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))) -> Triple(Str "208",str0,num0)
| Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str0))) -> Triple(Str "209",str0,match_combined exp0)
| Triple(VhdXorLogicalExpression,Double(VhdConcatSimpleExpression,lst0),Double(VhdConcatSimpleExpression,
	lst1)) -> Triple(Str "210",match_combined lst0,match_combined lst1)
| Triple(VhdXorLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdConcatSimpleExpression,
	lst0)) -> Triple(Str "211",match_combined lst0,match_combined exp0)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Double(VhdCharPrimary,chr0))) -> Triple(Str "212",
	str0,chr0)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Double(VhdIntPrimary,num0))) -> Triple(Str "213",
	str0,num0)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)))) -> Triple(Str "214",
	str0,str1)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,str2))))) -> Quadruple(Str "215",
	str0,str1,str2)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0))) -> Quadruple(Str "216",str0,str1,match_combined lst0)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0)))) -> Quadruple(Str "217",
	str0,str1,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1))))) -> Triple(Str "218",
	num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdIntPrimary,num0),Triple(VhdExpFactor,Double(VhdIntPrimary,
	num1),Double(VhdNamePrimary,Double(VhdSimpleName,str0))))))) -> Quadruple(Str "219",
	str0,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0))))) -> Triple(Str "220",str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdIntPrimary,num0),
	Triple(VhdExpFactor,Double(VhdIntPrimary,num1),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)))),Double(VhdIntPrimary,num2))))) -> Quintuple(Str "221",str0,num0,num1,num2)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1))))) -> Quadruple(Str "222",
	num0,num1,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1))))) -> Quadruple(Str "223",
	str0,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1))))) -> Quadruple(Str "224",
	num0,num1,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,str0)))))) -> Quadruple(Str "225",
	str0,num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Triple(VhdMultTerm,Double(VhdIntPrimary,
	num0),Double(VhdParenthesedPrimary,exp0)),Double(VhdIntPrimary,num1)),Triple(VhdMultTerm,
	Double(VhdIntPrimary,num2),Double(VhdNamePrimary,Double(VhdSimpleName,str0))))))) -> Sextuple(Str "226",
	str0,num0,num1,num2,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1))))) -> Triple(Str "227",
	num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)))))) -> Triple(Str "228",num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)))))) -> Triple(Str "229",str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdNamePrimary,
	Double(VhdAttributeName,exp1)))))) -> Triple(Str "230",match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdAttributeName,exp1)),Double(VhdIntPrimary,num0)))))) -> Quadruple(Str "231",
	num0,match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdAttributeName,
	exp1)))))) -> Quadruple(Str "232",num0,match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdSubTypeRange,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str0),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),
	Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,
	num1)))))))) -> Quintuple(Str "233",str0,num0,num1,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdSubTypeRange,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str0),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)),Double(VhdNamePrimary,Double(VhdAttributeName,exp1)))))))) -> Quadruple(Str "234",
	str0,match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdSubTypeRange,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str0),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)),Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,
	num0)))))))) -> Quintuple(Str "235",str0,str1,num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdAbsFactor,
	Double(VhdParenthesedPrimary,exp0)))) -> Double(Str "236",match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdIntPrimary,
	num0))) -> Double(Str "237",num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdNamePrimary,
	Double(VhdAttributeName,exp0)))) -> Double(Str "238",match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdNamePrimary,
	Double(VhdOperatorString,str0)))) -> Double(Str "239",str0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)))) -> Double(Str "240",str0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdNotFactor,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0))))) -> Double(Str "241",str0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "242",
	str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "243",str0,str1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdParenthesedPrimary,exp0)))) -> Triple(Str "244",
	str0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)))) -> Quadruple(Str "245",str0,match_combined lst0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdIntPrimary,
	num0)))) -> Quadruple(Str "246",str0,num0,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1)))) -> Quintuple(Str "247",str0,str1,match_combined lst0,
	match_combined lst1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdDivTerm,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))))) -> Quadruple(Str "248",str0,str1,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdLdotted,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))))) -> Quadruple(Str "249",str0,str1,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdMultTerm,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1)))) -> Quintuple(Str "250",str0,str1,match_combined lst0,
	match_combined lst1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0))) -> Triple(Str "251",str0,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "252",
	str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdSubSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))))) -> Triple(Str "253",
	str0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdSubSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdIntPrimary,
	num0)))) -> Quadruple(Str "254",str0,num0,match_combined lst0)
| Triple(Vhdattribute_name,Double(VhdSuffixSimpleName,Double(VhdSimpleName,str0)),
	str1) -> Triple(Str "255",str0,str1)
| Triple(Vhdcase_statement_alternative,lst0,lst1) -> Triple(Str "256",match_combined lst0,
	match_combined lst1)
| Triple(Vhdconditional_waveform,lst0,Double(VhdCondition,exp0)) -> Triple(Str "257",
	match_combined lst0,match_combined exp0)
| Triple(Vhdelement_association,lst0,Double(VhdAggregatePrimary,lst1)) -> Triple(Str "258",
	match_combined lst0,match_combined lst1)
| Triple(Vhdelement_association,lst0,Double(VhdCharPrimary,chr0)) -> Triple(Str "259",
	chr0,match_combined lst0)
| Triple(Vhdelement_association,lst0,Double(VhdNamePrimary,Double(VhdOperatorString,
	str0))) -> Triple(Str "260",str0,match_combined lst0)
| Triple(Vhdelement_association,lst0,Double(VhdNamePrimary,Double(VhdSimpleName,str0))) -> Triple(Str "334",
	str0,match_combined lst0)
| Triple(Vhdelement_association,lst0,Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)))) -> Triple(Str "335",str0,match_combined lst0)
| Triple(Vhdelement_association,lst0,Double(VhdNotFactor,Double(VhdParenthesedPrimary,
	exp0))) -> Triple(Str "263",match_combined lst0,match_combined exp0)
| Triple(Vhdelement_association,lst0,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst1))) -> Quadruple(Str "264",str0,match_combined lst0,
	match_combined lst1)
| Triple(Vhdelement_association,lst0,Double(VhdParenthesedPrimary,exp0)) -> Triple(Str "265",
	match_combined lst0,match_combined exp0)
| Triple(Vhdelement_association,lst0,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst1)) -> Quadruple(Str "266",str0,match_combined lst0,match_combined lst1)
| Triple(Vhdelement_declaration,lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,
	Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,lst1))) -> Quadruple(Str "267",
	str0,match_combined lst0,match_combined lst1)
| Triple(Vhdelement_declaration,lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,
	Str ""),Double(VhdSimpleName,str0),VhdNoConstraint)) -> Triple(Str "268",str0,match_combined lst0)
| Triple(Vhdwaveform_element,Double(VhdAggregatePrimary,lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Double(Str "269",match_combined lst0)
| Triple(Vhdwaveform_element,Double(VhdCharPrimary,chr0),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Double(Str "270",chr0)
| Triple(Vhdwaveform_element,Double(VhdCharPrimary,chr0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))) -> Triple(Str "271",str0,chr0)
| Triple(Vhdwaveform_element,Double(VhdCharPrimary,chr0),Double(VhdPhysicalPrimary,
	Triple(VhdPhysicalInteger,num0,str0))) -> Quadruple(Str "272",str0,num0,chr0)
| Triple(Vhdwaveform_element,Double(VhdConcatSimpleExpression,lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Double(Str "273",match_combined lst0)
| Triple(Vhdwaveform_element,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Double(Str "274",num0)
| Triple(Vhdwaveform_element,Double(VhdNamePrimary,Double(VhdOperatorString,str0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Double(Str "275",str0)
| Triple(Vhdwaveform_element,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Double(Str "276",str0)
| Triple(Vhdwaveform_element,Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Double(Str "277",
	str0)
| Triple(Vhdwaveform_element,Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdPhysicalPrimary,Triple(VhdPhysicalInteger,num0,str1))) -> Quadruple(Str "278",
	str0,str1,num0)
| Triple(Vhdwaveform_element,Double(VhdNotFactor,Double(VhdParenthesedPrimary,exp0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Double(Str "279",match_combined exp0)
| Triple(Vhdwaveform_element,Double(VhdNotFactor,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1)))),
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quadruple(Str "336",str0,
	str1,match_combined lst0)
| Triple(Vhdwaveform_element,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "281",
	str0,match_combined lst0)
| Triple(Vhdwaveform_element,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Double(Str "282",match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "283",
	str0,num0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "284",
	num0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "285",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)))),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "286",
	str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdParenthesedPrimary,exp0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "287",str0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quadruple(Str "288",
	str0,str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quadruple(Str "337",str0,
	str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quadruple(Str "338",str0,
	str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Quadruple(Str "291",str0,str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "292",
	str0,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "293",
	str0,str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdNorLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "294",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "295",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)),Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1))),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quintuple(Str "339",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quintuple(Str "297",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(Vhdwaveform_element,Triple(VhdXorLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "298",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdXorLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdParenthesedPrimary,exp0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "299",str0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdXorLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quadruple(Str "300",
	str0,str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdXorLogicalExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quintuple(Str "301",
	str0,str1,match_combined lst0,match_combined lst1)
| VhdChoiceOthers -> (Str "302")
| List [itm] -> match_combined itm
| List lst -> List (List.map match_combined lst)
| others -> others
