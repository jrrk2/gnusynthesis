
open VhdlTree

let rec match_combined = function
| Double(VhdBlockAttributeDeclaration,Triple(Vhdattribute_declaration,str0,Double(VhdSimpleName,
	str1))) -> Triple(Str "309",str0,str1)
| Double(VhdBlockAttributeSpecification,Quadruple(Vhdattribute_specification,str0,
	Triple(Vhdentity_specification,lst0,VhdClassArchitecture),Double(VhdNamePrimary,Double(VhdOperatorString,
	str1)))) -> Quadruple(Str "310",str0,str1,match_combined lst0)
| Double(VhdBlockAttributeSpecification,Quadruple(Vhdattribute_specification,str0,
	Triple(Vhdentity_specification,lst0,VhdClassLabel),Double(VhdNamePrimary,Double(VhdOperatorString,
	str1)))) -> Quadruple(Str "311",str0,str1,match_combined lst0)
| Double(VhdBlockAttributeSpecification,Quadruple(Vhdattribute_specification,str0,
	Triple(Vhdentity_specification,lst0,VhdClassSignal),Double(VhdNamePrimary,Double(VhdOperatorString,
	str1)))) -> Quadruple(Str "312",str0,str1,match_combined lst0)
| Double(VhdBlockComponentDeclaration,Quadruple(Vhdcomponent_declaration,str0,lst0,
	lst1)) -> Quadruple(Str "0",str0,match_combined lst0,match_combined lst1)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Double(VhdAggregatePrimary,lst2))) -> Quintuple(Str "1",str0,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Double(VhdConcatSimpleExpression,lst2))) -> Quintuple(Str "313",str0,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Double(VhdNamePrimary,Double(VhdOperatorString,str1)))) -> Quintuple(Str "2",
	str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,str1)))) -> Quintuple(Str "314",
	str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Double(VhdParenthesedPrimary,exp0))) -> Quintuple(Str "315",str0,match_combined lst0,
	match_combined lst1,match_combined exp0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst2))) -> Sextuple(Str "316",
	str0,str1,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdRangeConstraint,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1)))),
	Double(VhdIntPrimary,num2))) -> Sextuple(Str "317",str0,num0,num1,num2,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdRangeConstraint,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1)))),
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1))) -> Septuple(Str "318",
	str0,str1,num0,num1,match_combined lst0,match_combined lst1)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdAggregatePrimary,
	lst1))) -> Quadruple(Str "3",str0,match_combined lst0,match_combined lst1)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdCharPrimary,
	chr0))) -> Quadruple(Str "319",str0,chr0,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdIntPrimary,
	num0))) -> Quadruple(Str "4",str0,num0,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdNamePrimary,
	Double(VhdAttributeName,exp0)))) -> Quadruple(Str "320",str0,match_combined lst0,
	match_combined exp0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdNamePrimary,
	Double(VhdOperatorString,str1)))) -> Quadruple(Str "321",str0,str1,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)))) -> Quadruple(Str "322",str0,str1,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdNotFactor,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))))) -> Quadruple(Str "323",str0,
	str1,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdParenthesedPrimary,
	exp0))) -> Quadruple(Str "324",str0,match_combined lst0,match_combined exp0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdPhysicalPrimary,
	Triple(VhdPhysicalInteger,num0,str1)))) -> Quintuple(Str "325",str0,str1,num0,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdAddSimpleExpression,
	Double(VhdIntPrimary,num0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),
	lst1)))) -> Sextuple(Str "326",str0,str1,num0,match_combined lst0,match_combined lst1)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "327",
	str0,str1,num0,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2))))) -> Quintuple(Str "328",str0,str1,str2,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str2),lst1)))) -> Sextuple(Str "329",str0,str1,str2,match_combined lst0,
	match_combined lst1)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdAddSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1),Double(VhdIntPrimary,
	num0)))) -> Sextuple(Str "330",str0,str1,num0,match_combined lst0,match_combined lst1)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdAddSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str2),lst2)))) -> Septuple(Str "331",str0,str1,str2,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdDivTerm,
	Double(VhdFloatPrimary,real0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst1)))) -> Sextuple(Str "332",str0,str1,real0,match_combined lst0,match_combined lst1)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdDivTerm,
	Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "333",
	str0,num0,match_combined lst0,match_combined exp0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdDivTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "334",
	str0,str1,num0,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdDivTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2))))) -> Quintuple(Str "335",str0,str1,str2,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdDivTerm,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "336",
	str0,num0,match_combined lst0,match_combined exp0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "337",
	str0,str1,num0,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdExpFactor,
	Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))))) -> Quintuple(Str "338",
	str0,str1,num0,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdExpFactor,
	Double(VhdIntPrimary,num0),Double(VhdParenthesedPrimary,exp0)))) -> Quintuple(Str "339",
	str0,num0,match_combined lst0,match_combined exp0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdMultTerm,
	Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))))) -> Quintuple(Str "340",
	str0,str1,num0,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdMultTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2))))) -> Quintuple(Str "341",str0,str1,str2,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1))) -> Quintuple(Str "342",str0,str1,match_combined lst0,
	match_combined lst1)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdNotEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "343",
	str0,str1,num0,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdOrLogicalExpression,
	Double(VhdNamePrimary,Double(VhdOperatorString,str1)),Double(VhdNamePrimary,Double(VhdOperatorString,
	str2))))) -> Quintuple(Str "344",str0,str1,str2,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdOrLogicalExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdOperatorString,
	str2))))) -> Quintuple(Str "345",str0,str1,str2,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "346",
	str0,str1,num0,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2))))) -> Quintuple(Str "347",str0,str1,str2,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Triple(VhdDivTerm,Double(VhdNamePrimary,
	Double(VhdSimpleName,str2)),Double(VhdIntPrimary,num0))))) -> Sextuple(Str "348",
	str0,str1,str2,num0,match_combined lst0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdSubSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "349",
	str0,num0,match_combined lst0,match_combined exp0)
| Double(VhdBlockConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdSubSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1),Double(VhdIntPrimary,
	num0)))) -> Sextuple(Str "350",str0,str1,num0,match_combined lst0,match_combined lst1)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),VhdSignalKindDefault,Double(VhdAggregatePrimary,lst2))) -> Quintuple(Str "351",
	str0,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),VhdSignalKindDefault,Double(VhdNamePrimary,Double(VhdOperatorString,str1)))) -> Quintuple(Str "5",
	str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),VhdSignalKindDefault,Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Quadruple(Str "6",
	str0,match_combined lst0,match_combined lst1)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),VhdSignalKindDefault,Double(VhdNamePrimary,Double(VhdSimpleName,str1)))) -> Quintuple(Str "352",
	str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),VhdSignalKindDefault,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst2))) -> Sextuple(Str "353",str0,str1,match_combined lst0,match_combined lst1,
	match_combined lst2)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdRangeConstraint,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1)))),
	VhdSignalKindDefault,Double(VhdIntPrimary,num2))) -> Sextuple(Str "354",str0,num0,
	num1,num2,match_combined lst0)
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
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdRangeConstraint,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))),VhdSignalKindDefault,Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Quintuple(Str "355",
	str0,str1,num0,match_combined lst0)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdRangeConstraint,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1))))),
	VhdSignalKindDefault,Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Sextuple(Str "356",
	str0,str1,num0,num1,match_combined lst0)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,
	Double(VhdAggregatePrimary,lst1))) -> Quadruple(Str "9",str0,match_combined lst0,
	match_combined lst1)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,
	Double(VhdCharPrimary,chr0))) -> Quadruple(Str "10",str0,chr0,match_combined lst0)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,
	Double(VhdIntPrimary,num0))) -> Quadruple(Str "357",str0,num0,match_combined lst0)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,
	Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Triple(Str "11",str0,match_combined lst0)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)))) -> Quadruple(Str "358",str0,str1,
	match_combined lst0)
| Double(VhdBlockSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1))) -> Quintuple(Str "359",
	str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdBlockSubProgramBody,Quadruple(Vhdsubprogram_body,Double(VhdFunctionSpecification,
	Septuple(Vhdfunction_specification,Double(VhdDesignatorIdentifier,str0),lst0,lst1,
	lst2,Double(VhdSimpleName,str1),VhdImpure)),lst3,lst4)) -> Octuple(Str "360",str0,
	str1,match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3,
	match_combined lst4)
| Double(VhdBlockSubProgramBody,Quadruple(Vhdsubprogram_body,Double(VhdFunctionSpecification,
	Septuple(Vhdfunction_specification,Double(VhdDesignatorIdentifier,str0),lst0,lst1,
	lst2,Double(VhdSimpleName,str1),VhdUnknown)),lst3,lst4)) -> Octuple(Str "307",str0,
	str1,match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3,
	match_combined lst4)
| Double(VhdBlockSubProgramBody,Quadruple(Vhdsubprogram_body,Double(VhdProcedureSpecification,
	Quintuple(Vhdprocedure_specification,Double(VhdDesignatorIdentifier,str0),lst0,lst1,
	lst2)),lst3,lst4)) -> Septuple(Str "361",str0,match_combined lst0,match_combined lst1,
	match_combined lst2,match_combined lst3,match_combined lst4)
| Double(VhdBlockSubTypeDeclaration,Triple(Vhdsubtype_declaration,str0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str1),Double(VhdArrayConstraint,
	lst0)))) -> Quadruple(Str "13",str0,str1,match_combined lst0)
| Double(VhdBlockSubTypeDeclaration,Triple(Vhdsubtype_declaration,str0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str1),Double(VhdRangeConstraint,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1)))))) -> Quintuple(Str "14",
	str0,str1,num0,num1)
| Double(VhdBlockTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdAccessTypeDefinition,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,
	Str ""),Double(VhdSimpleName,str1),VhdNoConstraint))))) -> Triple(Str "362",str0,
	str1)
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
	str1),Double(VhdArrayConstraint,lst1)))))))) -> Quintuple(Str "363",str0,str1,match_combined lst0,
	match_combined lst1)
| Double(VhdBlockTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdArrayTypeDefinition,Double(VhdUnboundedArray,Triple(Vhdunbounded_array_definition,
	lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint))))))) -> Quadruple(Str "17",str0,str1,match_combined lst0)
| Double(VhdBlockTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdEnumerationTypeDefinition,lst0)))) -> Triple(Str "18",str0,match_combined lst0)
| Double(VhdBlockTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdRecordTypeDefinition,Triple(Vhdrecord_type_definition,lst0,Str ""))))) -> Triple(Str "19",
	str0,match_combined lst0)
| Double(VhdBlockTypeDeclaration,Double(VhdIncompleteType,Double(Vhdincomplete_type_declaration,
	str0))) -> Double(Str "364",str0)
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
| Double(VhdComponentConfiguration,Quadruple(Vhdcomponent_configuration,Triple(Vhdcomponent_specification,
	lst0,Double(VhdSimpleName,str0)),Quadruple(Vhdbinding_indication,Triple(VhdBindingEntityArchitecture,
	Double(VhdSelectedName,lst1),str1),lst2,lst3),Quadruple(Vhdblock_configuration,Double(VhdBlockSpecificationName,
	Double(VhdSimpleName,str2)),lst4,lst5))) -> Decuple(Str "365",str0,str1,str2,match_combined lst0,
	match_combined lst1,match_combined lst2,match_combined lst3,match_combined lst4,match_combined lst5)
| Double(VhdComponentConfiguration,Quadruple(Vhdcomponent_configuration,Triple(Vhdcomponent_specification,
	lst0,Double(VhdSimpleName,str0)),Quadruple(Vhdbinding_indication,Triple(VhdBindingEntityArchitecture,
	Double(VhdSelectedName,lst1),str1),lst2,lst3),Quadruple(Vhdblock_configuration,VhdBlockSpecificationEmpty,
	lst4,lst5))) -> Nonuple(Str "366",str0,str1,match_combined lst0,match_combined lst1,
	match_combined lst2,match_combined lst3,match_combined lst4,match_combined lst5)
| Double(VhdConcatSimpleExpression,lst0) -> Double(Str "26",match_combined lst0)
| Double(VhdConcurrentAssertionStatement,Quadruple(Vhdconcurrent_assertion_statement,
	Str "",str0,Quadruple(Vhdassertion,Double(VhdCondition,exp0),Double(VhdConcatSimpleExpression,
	lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))))) -> Quintuple(Str "367",
	str0,str1,match_combined lst0,match_combined exp0)
| Double(VhdConcurrentAssertionStatement,Quadruple(Vhdconcurrent_assertion_statement,
	Str "",str0,Quadruple(Vhdassertion,Double(VhdCondition,exp0),Double(VhdNamePrimary,
	Double(VhdOperatorString,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,str2))))) -> Quintuple(Str "368",
	str0,str1,str2,match_combined exp0)
| Double(VhdConcurrentBlockStatement,unknown) -> (Str "27")
| Double(VhdConcurrentComponentInstantiationStatement,Quintuple(Vhdcomponent_instantiation_statement,
	str0,Double(VhdInstantiatedComponent,Double(VhdSimpleName,str1)),lst0,lst1)) -> Quintuple(Str "28",
	str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdConcurrentComponentInstantiationStatement,Quintuple(Vhdcomponent_instantiation_statement,
	str0,Triple(VhdInstantiatedEntityArchitecture,Double(VhdSelectedName,lst0),Str ""),
	lst1,lst2)) -> Quintuple(Str "369",str0,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdConcurrentComponentInstantiationStatement,Quintuple(Vhdcomponent_instantiation_statement,
	str0,Triple(VhdInstantiatedEntityArchitecture,Double(VhdSimpleName,str1),Str ""),
	lst0,lst1)) -> Quintuple(Str "29",str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdConcurrentGenerateStatement,Double(VhdForGenerateStatement,Sextuple(Vhdfor_generate_statement,
	str0,Str "",Triple(Vhdparameter_specification,str1,Double(VhdRange,Triple(VhdDecreasingRange,
	Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1)))),lst0,lst1))) -> Septuple(Str "370",
	str0,str1,num0,num1,match_combined lst0,match_combined lst1)
| Double(VhdConcurrentGenerateStatement,Double(VhdForGenerateStatement,Sextuple(Vhdfor_generate_statement,
	str0,Str "",Triple(Vhdparameter_specification,str1,Double(VhdRange,Triple(VhdDecreasingRange,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0)))),lst0,lst1))) -> Septuple(Str "371",
	str0,str1,num0,match_combined lst0,match_combined lst1,match_combined exp0)
| Double(VhdConcurrentGenerateStatement,Double(VhdForGenerateStatement,Sextuple(Vhdfor_generate_statement,
	str0,Str "",Triple(Vhdparameter_specification,str1,Double(VhdRange,Triple(VhdDecreasingRange,
	Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdIntPrimary,
	num0)),Double(VhdIntPrimary,num1)))),lst0,lst1))) -> Octuple(Str "372",str0,str1,
	str2,num0,num1,match_combined lst0,match_combined lst1)
| Double(VhdConcurrentGenerateStatement,Double(VhdForGenerateStatement,Sextuple(Vhdfor_generate_statement,
	str0,Str "",Triple(Vhdparameter_specification,str1,Double(VhdRange,Triple(VhdDecreasingRange,
	Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdIntPrimary,
	num0)),Double(VhdNamePrimary,Double(VhdSimpleName,str3))))),lst0,lst1))) -> Octuple(Str "373",
	str0,str1,str2,str3,num0,match_combined lst0,match_combined lst1)
| Double(VhdConcurrentGenerateStatement,Double(VhdForGenerateStatement,Sextuple(Vhdfor_generate_statement,
	str0,Str "",Triple(Vhdparameter_specification,str1,Double(VhdRange,Triple(VhdDecreasingRange,
	Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,
	num0)),Double(VhdIntPrimary,num1)))),lst0,lst1))) -> Octuple(Str "374",str0,str1,
	num0,num1,match_combined lst0,match_combined lst1,match_combined exp0)
| Double(VhdConcurrentGenerateStatement,Double(VhdForGenerateStatement,Sextuple(Vhdfor_generate_statement,
	str0,Str "",Triple(Vhdparameter_specification,str1,Double(VhdRange,Triple(VhdDecreasingRange,
	Triple(VhdSubSimpleExpression,Triple(VhdDivTerm,Double(VhdParenthesedPrimary,exp0),
	Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1)),Double(VhdIntPrimary,num2)))),
	lst0,lst1))) -> Nonuple(Str "375",str0,str1,num0,num1,num2,match_combined lst0,match_combined lst1,
	match_combined exp0)
| Double(VhdConcurrentGenerateStatement,Double(VhdForGenerateStatement,Sextuple(Vhdfor_generate_statement,
	str0,Str "",Triple(Vhdparameter_specification,str1,Double(VhdRange,Triple(VhdIncreasingRange,
	Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1)))),lst0,lst1))) -> Septuple(Str "376",
	str0,str1,num0,num1,match_combined lst0,match_combined lst1)
| Double(VhdConcurrentGenerateStatement,Double(VhdForGenerateStatement,Sextuple(Vhdfor_generate_statement,
	str0,Str "",Triple(Vhdparameter_specification,str1,Double(VhdRange,Triple(VhdIncreasingRange,
	Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,str2))))),lst0,
	lst1))) -> Septuple(Str "30",str0,str1,str2,num0,match_combined lst0,match_combined lst1)
| Double(VhdConcurrentGenerateStatement,Double(VhdForGenerateStatement,Sextuple(Vhdfor_generate_statement,
	str0,Str "",Triple(Vhdparameter_specification,str1,Double(VhdRange,Triple(VhdIncreasingRange,
	Double(VhdIntPrimary,num0),Double(VhdParenthesedPrimary,exp0)))),lst0,lst1))) -> Septuple(Str "377",
	str0,str1,num0,match_combined lst0,match_combined lst1,match_combined exp0)
| Double(VhdConcurrentGenerateStatement,Double(VhdForGenerateStatement,Sextuple(Vhdfor_generate_statement,
	str0,Str "",Triple(Vhdparameter_specification,str1,Double(VhdRange,Triple(VhdIncreasingRange,
	Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str2)),Double(VhdIntPrimary,num1))))),lst0,lst1))) -> Octuple(Str "31",str0,str1,
	str2,num0,num1,match_combined lst0,match_combined lst1)
| Double(VhdConcurrentGenerateStatement,Double(VhdForGenerateStatement,Sextuple(Vhdfor_generate_statement,
	str0,Str "",Triple(Vhdparameter_specification,str1,Double(VhdRange,Triple(VhdIncreasingRange,
	Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str2),lst0),Double(VhdIntPrimary,num1))))),lst1,lst2))) -> Nonuple(Str "378",
	str0,str1,str2,num0,num1,match_combined lst0,match_combined lst1,match_combined lst2)
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
	Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str1))),str2,VhdDelayNone,
	lst0)))) -> Quintuple(Str "379",str0,str1,str2,match_combined lst0)
| Double(VhdConcurrentSignalAssignmentStatement,Quadruple(Vhdconcurrent_signal_assignment_statement,
	Str "",str0,Double(VhdConcurrentConditionalSignalAssignment,Quintuple(Vhdconcurrent_conditional_signal_assignment,
	Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),
	lst0)),str2,VhdDelayNone,lst1)))) -> Sextuple(Str "380",str0,str1,str2,match_combined lst0,
	match_combined lst1)
| Double(VhdConcurrentSignalAssignmentStatement,Quadruple(Vhdconcurrent_signal_assignment_statement,
	Str "",str0,Double(VhdConcurrentConditionalSignalAssignment,Quintuple(Vhdconcurrent_conditional_signal_assignment,
	Double(VhdTargetName,Double(VhdSimpleName,str1)),str2,VhdDelayNone,lst0)))) -> Quintuple(Str "35",
	str0,str1,str2,match_combined lst0)
| Double(VhdConcurrentSignalAssignmentStatement,Quadruple(Vhdconcurrent_signal_assignment_statement,
	Str "",str0,Double(VhdConcurrentSelectedSignalAssignment,Septuple(Vhdconcurrent_selected_signal_assignment,
	Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str1))),str2,VhdDelayNone,
	Double(VhdSelector,Double(VhdNamePrimary,Double(VhdSimpleName,str3))),VhdOrdinarySelection,
	lst0)))) -> Sextuple(Str "381",str0,str1,str2,str3,match_combined lst0)
| Double(VhdConcurrentSignalAssignmentStatement,Quadruple(Vhdconcurrent_signal_assignment_statement,
	Str "",str0,Double(VhdConcurrentSimpleSignalAssignment,Quintuple(Vhdconcurrent_simple_signal_assignment,
	Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),str1,
	VhdDelayNone,lst1)))) -> Quintuple(Str "382",str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdConcurrentSignalAssignmentStatement,Quadruple(Vhdconcurrent_signal_assignment_statement,
	Str "",str0,Double(VhdConcurrentSimpleSignalAssignment,Quintuple(Vhdconcurrent_simple_signal_assignment,
	Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str1))),str2,VhdDelayNone,
	lst0)))) -> Quintuple(Str "383",str0,str1,str2,match_combined lst0)
| Double(VhdConcurrentSignalAssignmentStatement,Quadruple(Vhdconcurrent_signal_assignment_statement,
	Str "",str0,Double(VhdConcurrentSimpleSignalAssignment,Quintuple(Vhdconcurrent_simple_signal_assignment,
	Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),
	lst0)),str2,VhdDelayNone,lst1)))) -> Sextuple(Str "384",str0,str1,str2,match_combined lst0,
	match_combined lst1)
| Double(VhdConcurrentSignalAssignmentStatement,Quadruple(Vhdconcurrent_signal_assignment_statement,
	Str "",str0,Double(VhdConcurrentSimpleSignalAssignment,Quintuple(Vhdconcurrent_simple_signal_assignment,
	Double(VhdTargetName,Double(VhdSimpleName,str1)),str2,VhdDelayNone,lst0)))) -> Quintuple(Str "36",
	str0,str1,str2,match_combined lst0)
| Double(VhdConcurrentSignalAssignmentStatement,Quadruple(Vhdconcurrent_signal_assignment_statement,
	Str "",str0,Double(VhdConcurrentSimpleSignalAssignment,Quintuple(Vhdconcurrent_simple_signal_assignment,
	Triple(VhdTargetNameParameters,Double(VhdSimpleName,str1),lst0),str2,VhdDelayNone,
	lst1)))) -> Sextuple(Str "37",str0,str1,str2,match_combined lst0,match_combined lst1)
| Double(VhdEntityAttributeDeclaration,Triple(Vhdattribute_declaration,str0,Double(VhdSimpleName,
	str1))) -> Triple(Str "385",str0,str1)
| Double(VhdEntityAttributeSpecification,Quadruple(Vhdattribute_specification,str0,
	Triple(Vhdentity_specification,lst0,VhdClassConstant),Double(VhdIntPrimary,num0))) -> Quadruple(Str "386",
	str0,num0,match_combined lst0)
| Double(VhdEntityAttributeSpecification,Quadruple(Vhdattribute_specification,str0,
	Triple(Vhdentity_specification,lst0,VhdClassSignal),Double(VhdNamePrimary,Double(VhdOperatorString,
	str1)))) -> Quadruple(Str "387",str0,str1,match_combined lst0)
| Double(VhdEntityConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdPhysicalPrimary,
	Triple(VhdPhysicalInteger,num0,str1)))) -> Quintuple(Str "388",str0,str1,num0,match_combined lst0)
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
| Double(VhdIntPrimary,num0) -> Double(Str "389",num0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeBuffer,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,
	Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))))) -> Triple(Str "390",str0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),Double(VhdArrayConstraint,lst1)),VhdSignalKindDefault,
	Double(VhdAggregatePrimary,lst2)))) -> Quintuple(Str "308",str0,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),Double(VhdArrayConstraint,lst1)),VhdSignalKindDefault,
	Double(VhdConcatSimpleExpression,lst2)))) -> Quintuple(Str "391",str0,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),Double(VhdArrayConstraint,lst1)),VhdSignalKindDefault,
	Double(VhdNamePrimary,Double(VhdOperatorString,str1))))) -> Quintuple(Str "392",str0,
	str1,match_combined lst0,match_combined lst1)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),Double(VhdArrayConstraint,lst1)),VhdSignalKindDefault,
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))))) -> Quadruple(Str "309",str0,
	match_combined lst0,match_combined lst1)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,Double(VhdIntPrimary,
	num0),Double(VhdIntPrimary,num1)))),VhdSignalKindDefault,Double(VhdIntPrimary,num2)))) -> Sextuple(Str "393",
	str0,num0,num1,num2,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,Double(VhdIntPrimary,
	num0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))))),VhdSignalKindDefault,Double(VhdIntPrimary,
	num1)))) -> Sextuple(Str "394",str0,str1,num0,num1,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdAggregatePrimary,
	lst1)))) -> Quadruple(Str "395",str0,match_combined lst0,match_combined lst1)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdCharPrimary,
	chr0)))) -> Quadruple(Str "310",str0,chr0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdFloatPrimary,
	real0)))) -> Quadruple(Str "396",str0,real0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdIntPrimary,
	num0)))) -> Quadruple(Str "311",str0,num0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdNamePrimary,
	Double(VhdOperatorString,Str ""))))) -> Triple(Str "397",str0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdNamePrimary,
	Double(VhdOperatorString,str1))))) -> Quadruple(Str "398",str0,str1,match_combined lst0)
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
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdCharPrimary,
	chr0)))) -> Quadruple(Str "399",str0,chr0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeInOut,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))))) -> Triple(Str "316",str0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeOut,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),Double(VhdArrayConstraint,lst1)),VhdSignalKindDefault,
	Double(VhdAggregatePrimary,lst2)))) -> Quintuple(Str "400",str0,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeOut,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),Double(VhdArrayConstraint,lst1)),VhdSignalKindDefault,
	Double(VhdNamePrimary,Double(VhdOperatorString,str1))))) -> Quintuple(Str "401",str0,
	str1,match_combined lst0,match_combined lst1)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeOut,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),Double(VhdArrayConstraint,lst1)),VhdSignalKindDefault,
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))))) -> Quadruple(Str "317",str0,
	match_combined lst0,match_combined lst1)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeOut,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdCharPrimary,
	chr0)))) -> Quadruple(Str "402",str0,chr0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeOut,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))))) -> Triple(Str "318",str0,match_combined lst0)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceDefaultDeclaration,Sextuple(Vhdinterface_default_declaration,
	lst0,VhdInterfaceModeOut,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1)))) -> Quintuple(Str "403",str0,str1,match_combined lst0,
	match_combined lst1)
| Double(VhdInterfaceObjectDeclaration,Double(VhdInterfaceSignalDeclaration,Sextuple(Vhdinterface_signal_declaration,
	lst0,VhdInterfaceModeIn,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),
	Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))))) -> Triple(Str "319",str0,match_combined lst0)
| Double(VhdNamePrimary,Double(VhdAttributeName,exp0)) -> Double(Str "53",match_combined exp0)
| Double(VhdNamePrimary,Double(VhdOperatorString,str0)) -> Double(Str "54",str0)
| Double(VhdNamePrimary,Double(VhdSelectedName,lst0)) -> Double(Str "404",match_combined lst0)
| Double(VhdNamePrimary,Double(VhdSimpleName,Str "")) -> (Str "55")
| Double(VhdNamePrimary,Double(VhdSimpleName,str0)) -> Double(Str "56",str0)
| Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))) -> Double(Str "405",
	match_combined lst0)
| Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str0))) -> Double(Str "57",
	str0)
| Double(VhdNotFactor,Double(VhdParenthesedPrimary,exp0)) -> Double(Str "406",match_combined exp0)
| Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),
	lst1)) -> Triple(Str "407",match_combined lst0,match_combined lst1)
| Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0)) -> Triple(Str "58",
	str0,match_combined lst0)
| Double(VhdPackageAttributeDeclaration,Triple(Vhdattribute_declaration,str0,Double(VhdSimpleName,
	str1))) -> Triple(Str "408",str0,str1)
| Double(VhdPackageAttributeSpecification,Quadruple(Vhdattribute_specification,str0,
	Triple(Vhdentity_specification,lst0,VhdClassComponent),Double(VhdNamePrimary,Double(VhdOperatorString,
	str1)))) -> Quadruple(Str "409",str0,str1,match_combined lst0)
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
| Double(VhdPackageBodySubProgramBody,Quadruple(Vhdsubprogram_body,Double(VhdProcedureSpecification,
	Quintuple(Vhdprocedure_specification,Double(VhdDesignatorIdentifier,str0),lst0,lst1,
	lst2)),lst3,lst4)) -> Septuple(Str "410",str0,match_combined lst0,match_combined lst1,
	match_combined lst2,match_combined lst3,match_combined lst4)
| Double(VhdPackageBodySubTypeDeclaration,Triple(Vhdsubtype_declaration,str0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str1),Double(VhdArrayConstraint,
	lst0)))) -> Quadruple(Str "411",str0,str1,match_combined lst0)
| Double(VhdPackageBodyTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdArrayTypeDefinition,Double(VhdConstrainedArray,Triple(Vhdconstrained_array_definition,
	lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint))))))) -> Quadruple(Str "62",str0,str1,match_combined lst0)
| Double(VhdPackageBodyTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdEnumerationTypeDefinition,lst0)))) -> Triple(Str "412",str0,match_combined lst0)
| Double(VhdPackageComponentDeclaration,Quadruple(Vhdcomponent_declaration,str0,lst0,
	lst1)) -> Quadruple(Str "63",str0,match_combined lst0,match_combined lst1)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Double(VhdAggregatePrimary,lst2))) -> Quintuple(Str "323",str0,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Double(VhdConcatSimpleExpression,lst2))) -> Quintuple(Str "413",str0,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Double(VhdNamePrimary,Double(VhdOperatorString,str1)))) -> Quintuple(Str "414",
	str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst2))) -> Sextuple(Str "415",
	str0,str1,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdAggregatePrimary,
	lst1))) -> Quadruple(Str "324",str0,match_combined lst0,match_combined lst1)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdCharPrimary,
	chr0))) -> Quadruple(Str "416",str0,chr0,match_combined lst0)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdIntPrimary,
	num0))) -> Quadruple(Str "325",str0,num0,match_combined lst0)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdNamePrimary,
	Double(VhdOperatorString,str1)))) -> Quadruple(Str "326",str0,str1,match_combined lst0)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)))) -> Quadruple(Str "417",str0,str1,match_combined lst0)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdParenthesedPrimary,
	exp0))) -> Quadruple(Str "418",str0,match_combined lst0,match_combined exp0)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdAddSimpleExpression,
	Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1)))) -> Quintuple(Str "419",str0,
	num0,num1,match_combined lst0)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "420",
	str0,str1,num0,match_combined lst0)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2))))) -> Quintuple(Str "421",str0,str1,str2,match_combined lst0)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdAddSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))))) -> Quintuple(Str "422",
	str0,str1,match_combined lst0,match_combined exp0)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdAddSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1),Double(VhdIntPrimary,
	num0)))) -> Sextuple(Str "423",str0,str1,num0,match_combined lst0,match_combined lst1)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdDivTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "424",
	str0,str1,num0,match_combined lst0)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdDivTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2))))) -> Quintuple(Str "425",str0,str1,str2,match_combined lst0)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1))) -> Quintuple(Str "426",str0,str1,match_combined lst0,
	match_combined lst1)
| Double(VhdPackageConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "427",
	str0,str1,num0,match_combined lst0)
| Double(VhdPackageSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),VhdSignalKindDefault,Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Quadruple(Str "428",
	str0,match_combined lst0,match_combined lst1)
| Double(VhdPackageSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,
	Double(VhdCharPrimary,chr0))) -> Quadruple(Str "429",str0,chr0,match_combined lst0)
| Double(VhdPackageSignalDeclaration,Quintuple(Vhdsignal_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),VhdSignalKindDefault,
	Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Triple(Str "430",str0,match_combined lst0)
| Double(VhdPackageSubProgramDeclaration,Double(VhdFunctionSpecification,Septuple(Vhdfunction_specification,
	Double(VhdDesignatorIdentifier,str0),lst0,lst1,lst2,Double(VhdSimpleName,str1),VhdUnknown))) -> Sextuple(Str "327",
	str0,str1,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdPackageSubProgramDeclaration,Double(VhdFunctionSpecification,Septuple(Vhdfunction_specification,
	Double(VhdDesignatorOperator,str0),lst0,lst1,lst2,Double(VhdSimpleName,str1),VhdUnknown))) -> Sextuple(Str "328",
	str0,str1,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdPackageSubProgramDeclaration,Double(VhdProcedureSpecification,Quintuple(Vhdprocedure_specification,
	Double(VhdDesignatorIdentifier,str0),lst0,lst1,lst2))) -> Quintuple(Str "431",str0,
	match_combined lst0,match_combined lst1,match_combined lst2)
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
	str0,Double(VhdArrayTypeDefinition,Double(VhdConstrainedArray,Triple(Vhdconstrained_array_definition,
	lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdArrayConstraint,lst1)))))))) -> Quintuple(Str "432",str0,str1,match_combined lst0,
	match_combined lst1)
| Double(VhdPackageTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdArrayTypeDefinition,Double(VhdConstrainedArray,Triple(Vhdconstrained_array_definition,
	lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint))))))) -> Quadruple(Str "433",str0,str1,match_combined lst0)
| Double(VhdPackageTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdArrayTypeDefinition,Double(VhdUnboundedArray,Triple(Vhdunbounded_array_definition,
	lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdArrayConstraint,lst1)))))))) -> Quintuple(Str "434",str0,str1,match_combined lst0,
	match_combined lst1)
| Double(VhdPackageTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdArrayTypeDefinition,Double(VhdUnboundedArray,Triple(Vhdunbounded_array_definition,
	lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint))))))) -> Quadruple(Str "75",str0,str1,match_combined lst0)
| Double(VhdPackageTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdEnumerationTypeDefinition,lst0)))) -> Triple(Str "76",str0,match_combined lst0)
| Double(VhdPackageTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdRangeTypeDefinition,Triple(VhdIncreasingRange,Double(VhdIntPrimary,
	num0),Double(VhdIntPrimary,num1)))))) -> Quadruple(Str "435",str0,num0,num1)
| Double(VhdPackageTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdRecordTypeDefinition,Triple(Vhdrecord_type_definition,lst0,Str ""))))) -> Triple(Str "436",
	str0,match_combined lst0)
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
| Double(VhdProcessConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Double(VhdAggregatePrimary,lst2))) -> Quintuple(Str "437",str0,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdProcessConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Double(VhdNamePrimary,Double(VhdOperatorString,str1)))) -> Quintuple(Str "438",
	str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdProcessConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)))) -> Quadruple(Str "439",str0,str1,match_combined lst0)
| Double(VhdProcessFileDeclaration,Quintuple(Vhdfile_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str "")),Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Triple(Str "80",
	str0,match_combined lst0)
| Double(VhdProcessSubProgramBody,Quadruple(Vhdsubprogram_body,Double(VhdFunctionSpecification,
	Septuple(Vhdfunction_specification,Double(VhdDesignatorIdentifier,str0),lst0,lst1,
	lst2,Double(VhdSimpleName,str1),VhdUnknown)),lst3,lst4)) -> Octuple(Str "440",str0,
	str1,match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3,
	match_combined lst4)
| Double(VhdProcessSubProgramBody,Quadruple(Vhdsubprogram_body,Double(VhdProcedureSpecification,
	Quintuple(Vhdprocedure_specification,Double(VhdDesignatorIdentifier,str0),lst0,lst1,
	lst2)),lst3,lst4)) -> Septuple(Str "441",str0,match_combined lst0,match_combined lst1,
	match_combined lst2,match_combined lst3,match_combined lst4)
| Double(VhdProcessTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdEnumerationTypeDefinition,lst0)))) -> Triple(Str "442",str0,match_combined lst0)
| Double(VhdProcessTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdFileTypeDefinition,Double(VhdSimpleName,str1))))) -> Triple(Str "81",
	str0,str1)
| Double(VhdProcessUseClause,lst0) -> Double(Str "443",match_combined lst0)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdArrayConstraint,lst1)),Double(VhdAggregatePrimary,lst2))) -> Sextuple(Str "444",
	str0,str1,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdArrayConstraint,lst1)),Double(VhdConcatSimpleExpression,lst2))) -> Sextuple(Str "445",
	str0,str1,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdArrayConstraint,lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str "")))) -> Quintuple(Str "82",str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdArrayConstraint,lst1)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str2),lst2))) -> Septuple(Str "446",str0,str1,str2,match_combined lst0,match_combined lst1,
	match_combined lst2)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdRangeConstraint,Double(VhdAttrNameRange,Triple(Vhdattribute_name,
	Double(VhdSuffixSimpleName,Double(VhdSimpleName,str2)),str3)))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str "")))) -> Sextuple(Str "83",str0,str1,str2,str3,match_combined lst0)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),
	Double(VhdIntPrimary,num1)))),Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Sextuple(Str "447",
	str0,str1,num0,num1,match_combined lst0)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),
	Double(VhdNamePrimary,Double(VhdSimpleName,str2))))),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str "")))) -> Sextuple(Str "84",str0,str1,str2,num0,match_combined lst0)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),
	Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdIntPrimary,
	num1))))),Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Septuple(Str "448",
	str0,str1,str2,num0,num1,match_combined lst0)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Double(VhdIntPrimary,num0))) -> Quintuple(Str "449",str0,str1,
	num0,match_combined lst0)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Quadruple(Str "85",
	str0,str1,match_combined lst0)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Double(VhdNamePrimary,Double(VhdSimpleName,str2)))) -> Quintuple(Str "86",
	str0,str1,str2,match_combined lst0)
| Double(VhdProcessVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),
	lst1))) -> Sextuple(Str "450",str0,str1,str2,match_combined lst0,match_combined lst1)
| Double(VhdQualifiedExpressionPrimary,Triple(VhdQualifiedExpression,Double(VhdSuffixSimpleName,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdOperatorString,str1)))) -> Triple(Str "451",
	str0,str1)
| Double(VhdSecondaryUnit,Double(VhdArchitectureBody,Quintuple(Vhdarchitecture_body,
	str0,str1,lst0,lst1))) -> Quintuple(Str "87",str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdSecondaryUnit,Double(VhdPackageBody,Triple(Vhdpackage_body,str0,lst0))) -> Triple(Str "88",
	str0,match_combined lst0)
| Double(VhdSequentialAssertion,Triple(Vhdassertion_statement,Str "",Quadruple(Vhdassertion,
	Double(VhdCondition,exp0),Double(VhdConcatSimpleExpression,lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))))) -> Quadruple(Str "452",str0,match_combined lst0,match_combined exp0)
| Double(VhdSequentialAssertion,Triple(Vhdassertion_statement,Str "",Quadruple(Vhdassertion,
	Double(VhdCondition,exp0),Double(VhdNamePrimary,Double(VhdOperatorString,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))))) -> Quadruple(Str "89",str0,str1,match_combined exp0)
| Double(VhdSequentialAssertion,Triple(Vhdassertion_statement,Str "",Quadruple(Vhdassertion,
	Double(VhdCondition,exp0),Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))))) -> Quadruple(Str "453",str0,match_combined lst0,match_combined exp0)
| Double(VhdSequentialCase,Quintuple(Vhdcase_statement,Str "",Double(VhdSelector,Double(VhdNamePrimary,
	Double(VhdSelectedName,lst0))),VhdOrdinarySelection,lst1)) -> Triple(Str "454",match_combined lst0,
	match_combined lst1)
| Double(VhdSequentialCase,Quintuple(Vhdcase_statement,Str "",Double(VhdSelector,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))),VhdOrdinarySelection,lst0)) -> Triple(Str "90",str0,
	match_combined lst0)
| Double(VhdSequentialCase,Quintuple(Vhdcase_statement,Str "",Double(VhdSelector,Double(VhdParenthesedPrimary,
	exp0)),VhdOrdinarySelection,lst0)) -> Triple(Str "455",match_combined lst0,match_combined exp0)
| Double(VhdSequentialCase,Quintuple(Vhdcase_statement,Str "",Double(VhdSelector,Triple(VhdLdotted,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)))),VhdOrdinarySelection,lst1)) -> Quintuple(Str "91",str0,
	str1,match_combined lst0,match_combined lst1)
| Double(VhdSequentialCase,Quintuple(Vhdcase_statement,Str "",Double(VhdSelector,Triple(VhdNameParametersPrimary,
	Double(VhdSelectedName,lst0),lst1)),VhdOrdinarySelection,lst2)) -> Quadruple(Str "456",
	match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdSequentialCase,Quintuple(Vhdcase_statement,Str "",Double(VhdSelector,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)),VhdOrdinarySelection,lst1)) -> Quadruple(Str "92",
	str0,match_combined lst0,match_combined lst1)
| Double(VhdSequentialExit,Quadruple(Vhdexit_statement,Str "",Str "",Double(VhdCondition,
	exp0))) -> Double(Str "457",match_combined exp0)
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
	exp0)),Double(VhdIntPrimary,num0))))),lst0)) -> Quintuple(Str "458",str0,num0,match_combined lst0,
	match_combined exp0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdDecreasingRange,Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)),Double(VhdNamePrimary,Double(VhdAttributeName,exp1)))))),lst0)) -> Quintuple(Str "459",
	str0,match_combined lst0,match_combined exp0,match_combined exp1)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdDecreasingRange,Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1)))))),lst0)) -> Quintuple(Str "97",
	str0,str1,match_combined lst0,match_combined exp0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdDecreasingRange,Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)),Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,exp1),Double(VhdIntPrimary,
	num0)))))),lst0)) -> Sextuple(Str "460",str0,num0,match_combined lst0,match_combined exp0,
	match_combined exp1)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdDecreasingRange,Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)),Double(VhdIntPrimary,num0))))),lst0)) -> Quintuple(Str "461",str0,str1,num0,
	match_combined lst0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdDecreasingRange,Double(VhdParenthesedPrimary,exp0),
	Double(VhdIntPrimary,num0))))),lst0)) -> Quintuple(Str "462",str0,num0,match_combined lst0,
	match_combined exp0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,
	Double(VhdAttributeName,exp0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),
	Double(VhdIntPrimary,num0))))),lst0)) -> Sextuple(Str "98",str0,str1,num0,match_combined lst0,
	match_combined exp0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1))))),
	lst0)) -> Sextuple(Str "463",str0,str1,num0,num1,match_combined lst0)
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
	str0,Double(VhdRange,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdIntPrimary,num1)))))),
	lst0)) -> Sextuple(Str "464",str0,num0,num1,match_combined lst0,match_combined exp0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1)))))),
	lst0)) -> Sextuple(Str "465",str0,str1,num0,num1,match_combined lst0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num1)))))),lst0)) -> Sextuple(Str "466",
	str0,num0,num1,match_combined lst0,match_combined exp0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdIncreasingRange,Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)),Double(VhdNamePrimary,Double(VhdAttributeName,exp1)))))),lst0)) -> Quintuple(Str "467",
	str0,match_combined lst0,match_combined exp0,match_combined exp1)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdRange,Triple(VhdIncreasingRange,Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)),Double(VhdNamePrimary,Double(VhdSimpleName,str2)))))),lst0)) -> Quintuple(Str "468",
	str0,str1,str2,match_combined lst0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdForLoop,Triple(Vhdparameter_specification,
	str0,Double(VhdSubTypeRange,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,
	Str ""),Double(VhdAttributeName,exp0),VhdNoConstraint)))),lst0)) -> Quadruple(Str "103",
	str0,match_combined lst0,match_combined exp0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",Double(VhdWhileLoop,
	Double(VhdCondition,exp0)),lst0)) -> Triple(Str "104",match_combined lst0,match_combined exp0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,Str "",VhdAlwaysLoop,lst0)) -> Double(Str "469",
	match_combined lst0)
| Double(VhdSequentialLoop,Quadruple(Vhdloop_statement,str0,Double(VhdForLoop,Triple(Vhdparameter_specification,
	str1,Double(VhdRange,Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str2)),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1))))),
	lst0)) -> Septuple(Str "470",str0,str1,str2,num0,num1,match_combined lst0)
| Double(VhdSequentialNull,Double(Vhdnull_statement,Str "")) -> (Str "105")
| Double(VhdSequentialProcedureCall,Triple(Vhdprocedure_call_statement,Str "",Triple(Vhdprocedure_call,
	Double(VhdSimpleName,str0),lst0))) -> Triple(Str "106",str0,match_combined lst0)
| Double(VhdSequentialReport,Quadruple(Vhdreport_statement,Str "",Double(VhdConcatSimpleExpression,
	lst0),Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Double(Str "471",match_combined lst0)
| Double(VhdSequentialReport,Quadruple(Vhdreport_statement,Str "",Double(VhdNamePrimary,
	Double(VhdOperatorString,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1)))) -> Triple(Str "107",
	str0,str1)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Double(VhdCharPrimary,
	chr0))) -> Double(Str "108",chr0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Double(VhdConcatSimpleExpression,
	lst0))) -> Double(Str "472",match_combined lst0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Double(VhdIntPrimary,
	num0))) -> Double(Str "109",num0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Double(VhdNamePrimary,
	Double(VhdAttributeName,exp0)))) -> Double(Str "473",match_combined exp0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Double(VhdNamePrimary,
	Double(VhdOperatorString,str0)))) -> Double(Str "474",str0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)))) -> Double(Str "110",str0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Double(VhdNegSimpleExpression,
	Double(VhdIntPrimary,num0)))) -> Double(Str "475",num0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Double(VhdNotFactor,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0))))) -> Double(Str "476",str0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Double(VhdParenthesedPrimary,
	exp0))) -> Double(Str "111",match_combined exp0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "112",str0,str1)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Triple(VhdAddSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))))) -> Triple(Str "477",
	str0,match_combined exp0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Triple(VhdAndLogicalExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdParenthesedPrimary,exp1)))) -> Triple(Str "478",
	match_combined exp0,match_combined exp1)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Triple(VhdDivTerm,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "479",str0,
	num0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Triple(VhdDivTerm,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdParenthesedPrimary,exp0)))) -> Quadruple(Str "480",
	str0,match_combined lst0,match_combined exp0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "481",str0,str1)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))))) -> Quadruple(Str "482",str0,str1,match_combined lst0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Triple(VhdExpFactor,
	Double(VhdIntPrimary,num0),Double(VhdParenthesedPrimary,exp0)))) -> Triple(Str "483",
	num0,match_combined exp0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0))) -> Triple(Str "113",str0,match_combined lst0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "484",
	str0,num0)
| Double(VhdSequentialReturn,Triple(Vhdreturn_statement,Str "",Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "114",str0,str1)
| Double(VhdSequentialSignalAssignment,Double(VhdSimpleSignalAssignment,Quintuple(Vhdsimple_signal_assignment_statement,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	VhdDelayNone,lst1))) -> Triple(Str "485",match_combined lst0,match_combined lst1)
| Double(VhdSequentialSignalAssignment,Double(VhdSimpleSignalAssignment,Quintuple(Vhdsimple_signal_assignment_statement,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	VhdDelayNone,lst0))) -> Triple(Str "486",str0,match_combined lst0)
| Double(VhdSequentialSignalAssignment,Double(VhdSimpleSignalAssignment,Quintuple(Vhdsimple_signal_assignment_statement,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),VhdDelayNone,lst2))) -> Quadruple(Str "487",match_combined lst0,match_combined lst1,
	match_combined lst2)
| Double(VhdSequentialSignalAssignment,Double(VhdSimpleSignalAssignment,Quintuple(Vhdsimple_signal_assignment_statement,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),VhdDelayNone,lst1))) -> Quadruple(Str "488",str0,match_combined lst0,
	match_combined lst1)
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
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Double(VhdAggregatePrimary,lst1)))) -> Triple(Str "489",match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Double(VhdCharPrimary,chr0)))) -> Triple(Str "490",chr0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Double(VhdConcatSimpleExpression,lst1)))) -> Triple(Str "491",match_combined lst0,
	match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Double(VhdIntPrimary,num0)))) -> Triple(Str "492",num0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Double(VhdNamePrimary,Double(VhdOperatorString,str0))))) -> Triple(Str "493",str0,
	match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Double(VhdNamePrimary,Double(VhdSelectedName,lst1))))) -> Triple(Str "494",match_combined lst0,
	match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Double(VhdNamePrimary,Double(VhdSimpleName,str0))))) -> Triple(Str "495",str0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSelectedName,lst1)))))) -> Triple(Str "496",
	match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst1))))) -> Quadruple(Str "497",
	str0,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst1)),
	Double(VhdIntPrimary,num0))))) -> Quadruple(Str "498",num0,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst1)),
	Double(VhdNamePrimary,Double(VhdSelectedName,lst2)))))) -> Quadruple(Str "499",match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst1),lst2),Double(VhdIntPrimary,num0))))) -> Quintuple(Str "500",num0,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst1)),
	Double(VhdNamePrimary,Double(VhdSelectedName,lst2)))))) -> Quadruple(Str "501",match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst1)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)))))) -> Quadruple(Str "502",str0,
	match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst1)),
	Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSelectedName,lst2))))))) -> Quadruple(Str "503",
	match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst1)),
	Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str0))))))) -> Quadruple(Str "504",
	str0,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst1)),
	Double(VhdParenthesedPrimary,exp0))))) -> Quadruple(Str "505",match_combined lst0,
	match_combined lst1,match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst1))))) -> Quintuple(Str "506",str0,match_combined lst0,
	match_combined lst1,match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst1),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)))))) -> Quintuple(Str "507",str0,
	str1,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst1),lst2)))) -> Quadruple(Str "508",
	match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst1)))) -> Quadruple(Str "509",
	str0,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst1)),
	Double(VhdNamePrimary,Double(VhdSelectedName,lst2)))))) -> Quadruple(Str "510",match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst1)),
	Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst2),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))))))) -> Sextuple(Str "511",str0,
	str1,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdOrLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst1),lst2),Double(VhdNamePrimary,Double(VhdSelectedName,lst3)))))) -> Quintuple(Str "512",
	match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst1)),
	Double(VhdIntPrimary,num0))))) -> Quadruple(Str "513",num0,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	Triple(VhdSubSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst1),Double(VhdIntPrimary,num0))))) -> Quintuple(Str "514",str0,num0,match_combined lst0,
	match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSelectedName,lst0))),
	VhdSequentialNull))) -> Double(Str "515",match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdAbsFactor,Double(VhdParenthesedPrimary,exp0))))) -> Triple(Str "516",str0,
	match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdAggregatePrimary,lst0)))) -> Triple(Str "517",str0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdCharPrimary,chr0)))) -> Triple(Str "518",str0,chr0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdConcatSimpleExpression,lst0)))) -> Triple(Str "519",str0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdIntPrimary,num0)))) -> Triple(Str "520",str0,num0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdNamePrimary,Double(VhdOperatorString,str1))))) -> Triple(Str "521",str0,
	str1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdNamePrimary,Double(VhdSelectedName,lst0))))) -> Triple(Str "522",str0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))))) -> Triple(Str "523",str0,str1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdNegSimpleExpression,Double(VhdIntPrimary,num0))))) -> Triple(Str "524",
	str0,num0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdNegSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)))))) -> Triple(Str "525",
	str0,str1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdNegSimpleExpression,Triple(VhdMultTerm,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))))))) -> Quadruple(Str "526",str0,str1,num0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdNewFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str1)))))) -> Triple(Str "527",
	str0,str1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdNewFactor,Double(VhdQualifiedExpressionPrimary,Triple(VhdQualifiedExpression,
	Double(VhdSuffixSimpleName,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2)))))))) -> Quadruple(Str "528",str0,str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)))))) -> Triple(Str "529",
	str0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str1)))))) -> Triple(Str "530",
	str0,str1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdNotFactor,Double(VhdParenthesedPrimary,exp0))))) -> Triple(Str "531",str0,
	match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0))))) -> Quadruple(Str "532",
	str0,str1,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Double(VhdParenthesedPrimary,exp0)))) -> Triple(Str "533",str0,match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),
	Double(VhdIntPrimary,num0))))) -> Quadruple(Str "534",str0,num0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,
	num0))))) -> Quadruple(Str "535",str0,str1,num0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2)))))) -> Quadruple(Str "536",str0,str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str2),lst0))))) -> Quintuple(Str "537",str0,str1,str2,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAddSimpleExpression,Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,str2)))))) -> Quintuple(Str "538",
	str0,str1,str2,num0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0),Double(VhdCharPrimary,chr0))))) -> Quintuple(Str "539",str0,str1,chr0,
	match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0),Double(VhdParenthesedPrimary,exp0))))) -> Quintuple(Str "540",str0,str1,
	match_combined lst0,match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst1))))) -> Sextuple(Str "541",
	str0,str1,str2,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),
	Double(VhdNamePrimary,Double(VhdSelectedName,lst1)))))) -> Quadruple(Str "542",str0,
	match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),
	Double(VhdNamePrimary,Double(VhdSelectedName,lst0)))))) -> Quadruple(Str "543",str0,
	str1,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)))))) -> Quadruple(Str "544",str0,
	str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),
	Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str2))))))) -> Quadruple(Str "545",
	str0,str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),
	Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),
	lst1)))))) -> Quintuple(Str "546",str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))),Double(VhdNamePrimary,Double(VhdSimpleName,str2)))))) -> Quadruple(Str "547",
	str0,str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))),Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str2))))))) -> Quadruple(Str "548",
	str0,str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNotFactor,Double(VhdParenthesedPrimary,exp0)))))) -> Quintuple(Str "549",
	str0,match_combined lst0,match_combined lst1,match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0),Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str2),lst1)))))) -> Sextuple(Str "550",str0,str1,str2,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdDivTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,
	num0))))) -> Quadruple(Str "551",str0,str1,num0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdDivTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2)))))) -> Quadruple(Str "552",str0,str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdDivTerm,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0))))) -> Quadruple(Str "553",
	str0,num0,match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),lst1),
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst2))))) -> Sextuple(Str "554",
	str0,str1,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdModTerm,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)))))) -> Quadruple(Str "555",str0,str1,match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,
	num0))))) -> Quadruple(Str "556",str0,str1,num0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2)))))) -> Quadruple(Str "557",str0,str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdMultTerm,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0))))) -> Quadruple(Str "558",
	str0,num0,match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdMultTerm,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0),
	Double(VhdIntPrimary,num0))))) -> Quintuple(Str "559",str0,str1,num0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdMultTerm,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0),
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst1))))) -> Sextuple(Str "560",
	str0,str1,str2,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),lst1)))) -> Quadruple(Str "561",
	str0,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0)))) -> Quadruple(Str "562",
	str0,str1,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,
	Double(VhdSelectedName,lst0)))))) -> Quadruple(Str "563",str0,str1,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2)))))) -> Quadruple(Str "564",str0,str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNotFactor,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2))))))) -> Quadruple(Str "565",str0,
	str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdParenthesedPrimary,
	exp0))))) -> Quadruple(Str "566",str0,str1,match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Triple(VhdLdotted,
	Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),lst1),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2))))))) -> Sextuple(Str "567",str0,str1,str2,match_combined lst0,
	match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str2),lst0))))) -> Quintuple(Str "568",str0,str1,str2,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdOrLogicalExpression,Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))),Double(VhdNamePrimary,Double(VhdSimpleName,str2)))))) -> Quadruple(Str "569",
	str0,str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdParenthesedPrimary,
	exp1))))) -> Quadruple(Str "570",str0,match_combined exp0,match_combined exp1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0))))) -> Quintuple(Str "571",str0,str1,match_combined lst0,
	match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdOrLogicalExpression,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str2))),Triple(VhdLdotted,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str3),lst1),Double(VhdNamePrimary,
	Double(VhdSimpleName,str4))))))) -> Octuple(Str "572",str0,str1,str2,str3,str4,match_combined lst0,
	match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdRotateLeftExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)))))) -> Quadruple(Str "573",str0,
	str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,
	num0))))) -> Quadruple(Str "574",str0,str1,num0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,
	Double(VhdOperatorString,str2)))))) -> Quadruple(Str "575",str0,str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str2),lst0))))) -> Quintuple(Str "576",str0,str1,str2,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)))))) -> Quadruple(Str "577",str0,str1,match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdSubSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0),Double(VhdCharPrimary,chr0))))) -> Quintuple(Str "578",str0,str1,chr0,
	match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdSubSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0),Double(VhdParenthesedPrimary,exp0))))) -> Quintuple(Str "579",str0,str1,
	match_combined lst0,match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdSubSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst1))))) -> Sextuple(Str "580",
	str0,str1,str2,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdXnorLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst1))))) -> Sextuple(Str "581",
	str0,str1,str2,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdXorLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),
	Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst1),lst2))))) -> Quintuple(Str "582",
	str0,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdXorLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)))))) -> Quadruple(Str "583",str0,
	str1,str2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdXorLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst0))))) -> Quintuple(Str "584",
	str0,str1,str2,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdXorLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0))))) -> Quintuple(Str "585",str0,str1,match_combined lst0,
	match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdXorLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst2),lst3))))) -> Sextuple(Str "586",
	str0,match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdXorLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str2)))))) -> Quintuple(Str "587",
	str0,str1,str2,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	Triple(VhdXorLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst1))))) -> Sextuple(Str "588",
	str0,str1,str2,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Double(VhdNamePrimary,Double(VhdSimpleName,str0))),
	VhdSequentialNull))) -> Double(Str "589",str0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNamePrimary,Double(VhdSimpleName,str0)))),Triple(VhdLdotted,
	Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst2),lst3),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)))))) -> Septuple(Str "590",str0,str1,match_combined lst0,
	match_combined lst1,match_combined lst2,match_combined lst3)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNamePrimary,Double(VhdSimpleName,str0)))),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst2)))) -> Sextuple(Str "591",str0,str1,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNamePrimary,Double(VhdSimpleName,str0)))),Triple(VhdSubSimpleExpression,
	Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst2),lst3),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdIntPrimary,num0))))) -> Octuple(Str "592",
	str0,str1,num0,match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst2))),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))))) -> Sextuple(Str "593",str0,str1,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Double(VhdAggregatePrimary,lst2)))) -> Quadruple(Str "594",match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Double(VhdCharPrimary,chr0)))) -> Quadruple(Str "595",chr0,match_combined lst0,
	match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Double(VhdConcatSimpleExpression,lst2)))) -> Quadruple(Str "596",match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Double(VhdNamePrimary,Double(VhdOperatorString,str0))))) -> Quadruple(Str "597",
	str0,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Double(VhdNamePrimary,Double(VhdSelectedName,lst2))))) -> Quadruple(Str "598",
	match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,str0))))) -> Quadruple(Str "599",
	str0,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst2),lst3))))) -> Quintuple(Str "600",match_combined lst0,match_combined lst1,match_combined lst2,
	match_combined lst3)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst2)),Double(VhdIntPrimary,num0))))) -> Quintuple(Str "601",num0,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2))))))) -> Sextuple(Str "602",str0,str1,str2,match_combined lst0,
	match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst2),lst3),Double(VhdNamePrimary,Double(VhdSelectedName,lst4)))))) -> Sextuple(Str "603",
	match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3,match_combined lst4)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst2),lst3),Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst4),lst5))))) -> Septuple(Str "604",
	match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3,match_combined lst4,
	match_combined lst5)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst2),lst3),Double(VhdNamePrimary,Double(VhdSimpleName,str0)))))) -> Sextuple(Str "605",
	str0,match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst2),lst3)))) -> Quintuple(Str "606",
	match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst2)))) -> Quintuple(Str "607",
	str0,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Double(VhdAggregatePrimary,lst1)))) -> Quadruple(Str "608",str0,match_combined lst0,
	match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Double(VhdCharPrimary,chr0)))) -> Quadruple(Str "609",str0,chr0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Double(VhdConcatSimpleExpression,lst1)))) -> Quadruple(Str "610",str0,
	match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Double(VhdIntPrimary,num0)))) -> Quadruple(Str "611",str0,num0,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Double(VhdNamePrimary,Double(VhdOperatorString,str1))))) -> Quadruple(Str "612",
	str0,str1,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Double(VhdNamePrimary,Double(VhdSelectedName,lst1))))) -> Quadruple(Str "613",
	str0,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))))) -> Quadruple(Str "614",
	str0,str1,match_combined lst0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Double(VhdNotFactor,Double(VhdParenthesedPrimary,exp0))))) -> Quadruple(Str "615",
	str0,match_combined lst0,match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst1),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst2))))) -> Septuple(Str "616",
	str0,str1,str2,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst1),lst2),Double(VhdNamePrimary,Double(VhdSimpleName,str1)))))) -> Sextuple(Str "617",
	str0,str1,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst1),lst2)))) -> Quintuple(Str "618",
	str0,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1)))) -> Quintuple(Str "619",
	str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Triple(VhdOrLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst1),Double(VhdNamePrimary,Double(VhdSimpleName,str2)))))) -> Sextuple(Str "620",
	str0,str1,str2,match_combined lst0,match_combined lst1)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Triple(VhdXorLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1))))) -> Sextuple(Str "621",str0,str1,match_combined lst0,
	match_combined lst1,match_combined exp0)
| Double(VhdSequentialVariableAssignment,Double(VhdSimpleVariableAssignment,Quadruple(Vhdsimple_variable_assignment,
	Str "",Double(VhdTargetDotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Triple(VhdXorLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst1),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst2))))) -> Septuple(Str "622",
	str0,str1,str2,match_combined lst0,match_combined lst1,match_combined lst2)
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
	exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0)))) -> Quadruple(Str "623",
	str0,match_combined lst0,match_combined exp0)
| Double(VhdSequentialWait,Quintuple(Vhdwait_statement,Str "",lst0,Double(VhdCondition,
	exp0),Double(VhdPhysicalPrimary,Triple(VhdPhysicalInteger,num0,str0)))) -> Quintuple(Str "306",
	str0,num0,match_combined lst0,match_combined exp0)
| Double(VhdSimpleName,str0) -> Double(Str "133",str0)
| Double(VhdSubProgramAliasDeclaration,Quintuple(Vhdalias_declaration,Double(VhdDesignatorIdentifier,
	str0),Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdArrayConstraint,lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2)),VhdNone)) -> Quintuple(Str "134",str0,str1,str2,match_combined lst0)
| Double(VhdSubProgramConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),Double(VhdArrayConstraint,
	lst1)),Double(VhdAggregatePrimary,lst2))) -> Quintuple(Str "624",str0,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdSubProgramConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdAggregatePrimary,
	lst1))) -> Quadruple(Str "625",str0,match_combined lst0,match_combined lst1)
| Double(VhdSubProgramConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdCharPrimary,
	chr0))) -> Quadruple(Str "626",str0,chr0,match_combined lst0)
| Double(VhdSubProgramConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdIntPrimary,
	num0))) -> Quadruple(Str "627",str0,num0,match_combined lst0)
| Double(VhdSubProgramConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdNamePrimary,
	Double(VhdAttributeName,exp0)))) -> Quadruple(Str "628",str0,match_combined lst0,
	match_combined exp0)
| Double(VhdSubProgramConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdAddSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "330",
	str0,num0,match_combined lst0,match_combined exp0)
| Double(VhdSubProgramConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdDivTerm,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "629",
	str0,num0,match_combined lst0,match_combined exp0)
| Double(VhdSubProgramConstantDeclaration,Quadruple(Vhdconstant_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Triple(VhdMultTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0)))) -> Quintuple(Str "630",
	str0,str1,num0,match_combined lst0)
| Double(VhdSubProgramFileDeclaration,Quintuple(Vhdfile_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str "")),Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Triple(Str "631",
	str0,match_combined lst0)
| Double(VhdSubProgramFileDeclaration,Quintuple(Vhdfile_declaration,lst0,Quadruple(Vhdsubtype_indication,
	Double(VhdSimpleName,Str ""),Double(VhdSimpleName,str0),VhdNoConstraint),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,str2)))) -> Quintuple(Str "632",
	str0,str1,str2,match_combined lst0)
| Double(VhdSubProgramTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdArrayTypeDefinition,Double(VhdConstrainedArray,Triple(Vhdconstrained_array_definition,
	lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdArrayConstraint,lst1)))))))) -> Quintuple(Str "633",str0,str1,match_combined lst0,
	match_combined lst1)
| Double(VhdSubProgramTypeDeclaration,Double(VhdFullType,Triple(Vhdfull_type_declaration,
	str0,Double(VhdArrayTypeDefinition,Double(VhdConstrainedArray,Triple(Vhdconstrained_array_definition,
	lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint))))))) -> Quadruple(Str "634",str0,str1,match_combined lst0)
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
	str1),Double(VhdArrayConstraint,lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2)))) -> Sextuple(Str "635",str0,str1,str2,match_combined lst0,match_combined lst1)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),
	Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str2),lst1),Double(VhdIntPrimary,num1))))),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str3),lst2))) -> Decuple(Str "636",str0,str1,str2,str3,num0,num1,match_combined lst0,
	match_combined lst1,match_combined lst2)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,Double(VhdNegSimpleExpression,
	Double(VhdIntPrimary,num0)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str2),lst1)))),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str3),lst2))) -> Nonuple(Str "637",
	str0,str1,str2,str3,num0,match_combined lst0,match_combined lst1,match_combined lst2)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Double(VhdAggregatePrimary,lst1))) -> Quintuple(Str "638",
	str0,str1,match_combined lst0,match_combined lst1)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Double(VhdCharPrimary,chr0))) -> Quintuple(Str "138",str0,
	str1,chr0,match_combined lst0)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Double(VhdIntPrimary,num0))) -> Quintuple(Str "639",str0,str1,
	num0,match_combined lst0)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Double(VhdNamePrimary,Double(VhdAttributeName,exp0)))) -> Quintuple(Str "640",
	str0,str1,match_combined lst0,match_combined exp0)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Double(VhdNamePrimary,Double(VhdSimpleName,Str "")))) -> Quadruple(Str "139",
	str0,str1,match_combined lst0)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Double(VhdNamePrimary,Double(VhdSimpleName,str2)))) -> Quintuple(Str "641",
	str0,str1,str2,match_combined lst0)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Double(VhdNegSimpleExpression,Double(VhdIntPrimary,num0)))) -> Quintuple(Str "642",
	str0,str1,num0,match_combined lst0)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)),Double(VhdIntPrimary,num0)))) -> Sextuple(Str "643",str0,str1,num0,match_combined lst0,
	match_combined exp0)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),
	lst1))) -> Sextuple(Str "644",str0,str1,str2,match_combined lst0,match_combined lst1)
| Double(VhdSubProgramVariableDeclaration,Quintuple(Vhdvariable_declaration,str0,lst0,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str1),VhdNoConstraint),VhdSequentialNull)) -> Quadruple(Str "645",str0,str1,match_combined lst0)
| Double(VhdSuffixSimpleName,Double(VhdSimpleName,str0)) -> Double(Str "646",str0)
| Quintuple(Vhdif_statement,Str "",Double(VhdCondition,exp0),lst0,Double(VhdElse,lst1)) -> Quadruple(Str "331",
	match_combined lst0,match_combined lst1,match_combined exp0)
| Quintuple(Vhdif_statement,Str "",Double(VhdCondition,exp0),lst0,Double(VhdElsif,
	exp1)) -> Quadruple(Str "141",match_combined lst0,match_combined exp0,match_combined exp1)
| Quintuple(Vhdif_statement,Str "",Double(VhdCondition,exp0),lst0,VhdElseNone) -> Triple(Str "142",
	match_combined lst0,match_combined exp0)
| Triple(VhdAddSimpleExpression,Double(VhdIntPrimary,num0),Double(VhdParenthesedPrimary,
	exp0)) -> Triple(Str "647",num0,match_combined exp0)
| Triple(VhdAddSimpleExpression,Double(VhdIntPrimary,num0),Triple(VhdMultTerm,Double(VhdIntPrimary,
	num1),Double(VhdNamePrimary,Double(VhdSimpleName,str0)))) -> Quadruple(Str "143",
	str0,num0,num1)
| Triple(VhdAddSimpleExpression,Double(VhdIntPrimary,num0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)) -> Quadruple(Str "648",str0,num0,match_combined lst0)
| Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),
	Double(VhdIntPrimary,num0)) -> Triple(Str "649",num0,match_combined exp0)
| Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),
	Double(VhdIntPrimary,num0)) -> Triple(Str "650",num0,match_combined lst0)
| Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdIntPrimary,num0)) -> Triple(Str "144",str0,num0)
| Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdOperatorString,str1))) -> Triple(Str "651",str0,str1)
| Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Triple(Str "145",str0,str1)
| Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Triple(VhdMultTerm,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)))) -> Quadruple(Str "146",str0,str1,num0)
| Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdParenthesedPrimary,
	exp0))) -> Quadruple(Str "652",str0,str1,match_combined exp0)
| Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Triple(VhdMultTerm,Double(VhdParenthesedPrimary,exp0),Double(VhdParenthesedPrimary,
	exp1))) -> Quadruple(Str "653",str0,match_combined exp0,match_combined exp1)
| Triple(VhdAddSimpleExpression,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)),Double(VhdIntPrimary,num0)) -> Quadruple(Str "147",
	str0,num0,match_combined lst0)
| Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,
	num0)) -> Triple(Str "148",num0,match_combined exp0)
| Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "654",str0,match_combined exp0)
| Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdParenthesedPrimary,
	exp1)) -> Triple(Str "655",match_combined exp0,match_combined exp1)
| Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdMultTerm,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdIntPrimary,
	num0))) -> Quintuple(Str "656",str0,num0,match_combined lst0,match_combined exp0)
| Triple(VhdAddSimpleExpression,Triple(VhdDivTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "657",
	str0,num0,match_combined exp0)
| Triple(VhdAddSimpleExpression,Triple(VhdDivTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdDivTerm,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num1))) -> Quintuple(Str "658",str0,num0,num1,match_combined exp0)
| Triple(VhdAddSimpleExpression,Triple(VhdDivTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)),Double(VhdParenthesedPrimary,exp0))) -> Quintuple(Str "659",str0,str1,num0,
	match_combined exp0)
| Triple(VhdAddSimpleExpression,Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2))) -> Quadruple(Str "660",str0,str1,str2)
| Triple(VhdAddSimpleExpression,Triple(VhdMultTerm,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdIntPrimary,num0)),Triple(VhdMultTerm,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1),Double(VhdIntPrimary,num1))) -> Septuple(Str "661",
	str0,str1,num0,num1,match_combined lst0,match_combined lst1)
| Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdCharPrimary,chr0)) -> Quadruple(Str "662",str0,chr0,match_combined lst0)
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
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),
	Double(VhdNamePrimary,Double(VhdSelectedName,lst1))) -> Triple(Str "663",match_combined lst0,
	match_combined lst1)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str0))) -> Triple(Str "664",str0,match_combined lst0)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),
	Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSelectedName,lst1)))) -> Triple(Str "665",
	match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),
	Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst1),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)))) -> Quintuple(Str "666",str0,str1,
	match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),
	Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst1),lst2)) -> Quadruple(Str "667",
	match_combined lst0,match_combined lst1,match_combined lst2)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdSelectedName,lst0))) -> Triple(Str "668",str0,match_combined lst0)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Triple(Str "669",str0,str1)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)))) -> Triple(Str "670",
	str0,match_combined lst0)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str1)))) -> Triple(Str "671",
	str0,str1)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNotFactor,Double(VhdParenthesedPrimary,exp0))) -> Triple(Str "672",str0,
	match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),
	lst1))) -> Quadruple(Str "673",str0,match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdParenthesedPrimary,exp0)) -> Triple(Str "674",str0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),lst1)) -> Quadruple(Str "675",
	str0,match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0)) -> Quadruple(Str "676",
	str0,str1,match_combined lst0)
| Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2)))) -> Quadruple(Str "677",str0,str1,str2)
| Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Triple(Str "678",str0,
	str1)
| Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str1)))) -> Triple(Str "679",
	str0,str1)
| Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Double(VhdParenthesedPrimary,
	exp0)),Double(VhdNamePrimary,Double(VhdSimpleName,str0))) -> Triple(Str "680",str0,
	match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Double(VhdParenthesedPrimary,
	exp0)),Double(VhdParenthesedPrimary,exp1)) -> Triple(Str "681",match_combined exp0,
	match_combined exp1)
| Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Double(VhdParenthesedPrimary,
	exp0)),Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)))) -> Quadruple(Str "682",str0,str1,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Double(VhdParenthesedPrimary,
	exp0)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0)) -> Quadruple(Str "683",
	str0,match_combined lst0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSelectedName,lst0),lst1)),Double(VhdNamePrimary,Double(VhdSelectedName,
	lst2))) -> Quadruple(Str "684",match_combined lst0,match_combined lst1,match_combined lst2)
| Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSelectedName,lst0),lst1)),Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst2),lst3)) -> Quintuple(Str "685",match_combined lst0,match_combined lst1,match_combined lst2,
	match_combined lst3)
| Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "686",
	str0,str1,match_combined lst0)
| Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "153",
	str0,match_combined lst0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSelectedName,lst0))) -> Triple(Str "687",match_combined lst0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "688",str0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdNotFactor,
	Double(VhdNamePrimary,Double(VhdSelectedName,lst0)))) -> Triple(Str "689",match_combined lst0,
	match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdNotFactor,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)))) -> Triple(Str "690",str0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdNotFactor,
	Double(VhdParenthesedPrimary,exp1))) -> Triple(Str "691",match_combined exp0,match_combined exp1)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdParenthesedPrimary,
	exp1)) -> Triple(Str "154",match_combined exp0,match_combined exp1)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)))) -> Quadruple(Str "692",str0,match_combined lst0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdCharPrimary,chr0))) -> Quadruple(Str "155",
	str0,chr0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0))) -> Quadruple(Str "693",
	str0,num0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdOperatorString,
	str1)))) -> Quadruple(Str "694",str0,str1,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)))) -> Quadruple(Str "695",str0,str1,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdParenthesedPrimary,exp1),Double(VhdCharPrimary,chr0))) -> Quadruple(Str "696",
	chr0,match_combined exp0,match_combined exp1)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdCharPrimary,
	chr0))) -> Quintuple(Str "697",str0,chr0,match_combined lst0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,
	Double(VhdOperatorString,str1)))) -> Quintuple(Str "698",str0,str1,match_combined lst0,
	match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdGreaterOrEqualRelation,
	Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdNamePrimary,Double(VhdSelectedName,
	lst1)))) -> Quadruple(Str "699",match_combined lst0,match_combined lst1,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdGreaterOrEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)))) -> Quintuple(Str "700",str0,str1,match_combined lst0,
	match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdLessOrEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0))) -> Quadruple(Str "701",
	str0,num0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdLessOrEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdAttributeName,
	exp1)))) -> Quadruple(Str "156",str0,match_combined exp0,match_combined exp1)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)) -> Quadruple(Str "702",str0,match_combined lst0,
	match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdNotEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdCharPrimary,chr0))) -> Quadruple(Str "703",
	str0,chr0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdNotEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0))) -> Quadruple(Str "704",
	str0,num0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdNotEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdOperatorString,
	str1)))) -> Quadruple(Str "705",str0,str1,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdNotEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)))) -> Quadruple(Str "706",str0,str1,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdNotEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdCharPrimary,
	chr0))) -> Quintuple(Str "707",str0,chr0,match_combined lst0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdNotEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1))) -> Sextuple(Str "157",str0,str1,match_combined lst0,
	match_combined lst1,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)),Double(VhdNamePrimary,Double(VhdAttributeName,exp1))),Double(VhdParenthesedPrimary,
	exp2)) -> Quadruple(Str "708",match_combined exp0,match_combined exp1,match_combined exp2)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdOperatorString,
	str0)),Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),lst1)),Double(VhdParenthesedPrimary,
	exp0)) -> Quintuple(Str "709",str0,match_combined lst0,match_combined lst1,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)),Double(VhdCharPrimary,chr0)),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "710",
	chr0,match_combined lst0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSelectedName,lst1)),Double(VhdCharPrimary,chr1))) -> Quintuple(Str "711",
	chr0,chr1,match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSelectedName,lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,str0)))) -> Quintuple(Str "712",
	str0,chr0,match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdCharPrimary,chr1))) -> Quintuple(Str "713",
	str0,chr0,chr1,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst1),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),
	Double(VhdCharPrimary,chr1))) -> Septuple(Str "714",str0,str1,chr0,chr1,match_combined lst0,
	match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSelectedName,lst1),lst2),Double(VhdCharPrimary,chr1))) -> Sextuple(Str "715",
	chr0,chr1,match_combined lst0,match_combined lst1,match_combined lst2)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSelectedName,lst1),lst2),Double(VhdNamePrimary,Double(VhdOperatorString,
	str0)))) -> Sextuple(Str "716",str0,chr0,match_combined lst0,match_combined lst1,
	match_combined lst2)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)),Double(VhdIntPrimary,num0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSelectedName,lst1)),Double(VhdCharPrimary,chr0))) -> Quintuple(Str "717",
	num0,chr0,match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSelectedName,lst1)),Double(VhdCharPrimary,chr0))) -> Quintuple(Str "718",
	str0,chr0,match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSelectedName,lst1)),Double(VhdIntPrimary,num0))) -> Quintuple(Str "719",
	str0,num0,match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0))) -> Quadruple(Str "158",str0,chr0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "720",
	str0,chr0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSelectedName,lst0)),Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst1),lst2))) -> Sextuple(Str "721",str0,chr0,match_combined lst0,match_combined lst1,
	match_combined lst2)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdCharPrimary,chr1))) -> Quintuple(Str "159",
	str0,str1,chr0,chr1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0))) -> Quintuple(Str "722",str0,
	str1,num0,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdOperatorString,str2)))) -> Quintuple(Str "723",
	str0,str1,str2,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,str2)))) -> Quintuple(Str "160",
	str0,str1,str2,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str2))),
	Double(VhdCharPrimary,chr1))) -> Septuple(Str "724",str0,str1,str2,chr0,chr1,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0),Double(VhdCharPrimary,chr1))) -> Sextuple(Str "161",
	str0,str1,chr0,chr1,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0),Double(VhdNamePrimary,Double(VhdOperatorString,str2)))) -> Sextuple(Str "725",
	str0,str1,str2,chr0,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdGreaterOrEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdAttributeName,exp0)))) -> Quintuple(Str "162",
	str0,str1,chr0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdLessRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdOperatorString,str2)))) -> Quintuple(Str "726",
	str0,str1,str2,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdNotEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdCharPrimary,chr1))) -> Quintuple(Str "727",
	str0,str1,chr0,chr1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdNotEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0))) -> Quintuple(Str "163",str0,
	str1,num0,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdNotEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdOperatorString,str2)))) -> Quintuple(Str "728",
	str0,str1,str2,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdNotEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,str2)))) -> Quintuple(Str "729",
	str0,str1,str2,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdNotEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0),Double(VhdNamePrimary,Double(VhdOperatorString,str2)))) -> Sextuple(Str "730",
	str0,str1,str2,chr0,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0))) -> Quadruple(Str "731",str0,num0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "732",
	str0,str1,num0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "733",
	str0,num0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdCharPrimary,chr0))) -> Quintuple(Str "734",
	str0,str1,num0,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1))) -> Quintuple(Str "735",str0,
	str1,num0,num1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdLessOrEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1))) -> Quintuple(Str "736",str0,
	str1,num0,num1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)),Double(VhdIntPrimary,num1))) -> Quintuple(Str "737",str0,str1,num0,num1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),Double(VhdParenthesedPrimary,
	exp0)) -> Quadruple(Str "738",str0,str1,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdCharPrimary,chr0))) -> Quintuple(Str "739",
	str0,str1,str2,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdIntPrimary,num0))) -> Quintuple(Str "740",
	str0,str1,str2,num0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst0),Double(VhdCharPrimary,
	chr0))) -> Sextuple(Str "741",str0,str1,str2,chr0,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),Triple(VhdNotEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str3)))) -> Quintuple(Str "742",str0,str1,str2,str3)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdParenthesedPrimary,
	exp0)) -> Quadruple(Str "743",str0,str1,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdCharPrimary,chr0))) -> Quintuple(Str "164",
	str0,str1,str2,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdNamePrimary,Double(VhdOperatorString,
	str3)))) -> Quintuple(Str "744",str0,str1,str2,str3)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst0),Double(VhdCharPrimary,
	chr0))) -> Sextuple(Str "165",str0,str1,str2,chr0,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst0),Double(VhdNamePrimary,
	Double(VhdOperatorString,str3)))) -> Sextuple(Str "745",str0,str1,str2,str3,match_combined lst0)
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
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Double(VhdParenthesedPrimary,
	exp0),Double(VhdCharPrimary,chr0)),Double(VhdParenthesedPrimary,exp1)) -> Quadruple(Str "746",
	chr0,match_combined exp0,match_combined exp1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),
	Double(VhdNamePrimary,Double(VhdSimpleName,str2))),Triple(VhdEqualRelation,Triple(VhdLdotted,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str3),lst1),Double(VhdNamePrimary,
	Double(VhdSimpleName,str4))),Double(VhdNamePrimary,Double(VhdSimpleName,str5)))) -> Nonuple(Str "332",
	str0,str1,str2,str3,str4,str5,match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Triple(VhdModTerm,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdIntPrimary,
	num0)),Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str2)),
	Double(VhdIntPrimary,num1))) -> Sextuple(Str "747",str0,str1,str2,num0,num1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSelectedName,lst0),lst1),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst2),lst3),Double(VhdCharPrimary,
	chr1))) -> Septuple(Str "748",chr0,chr1,match_combined lst0,match_combined lst1,match_combined lst2,
	match_combined lst3)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSelectedName,lst0),lst1),Double(VhdNamePrimary,Double(VhdOperatorString,
	str0))),Triple(VhdEqualRelation,Double(VhdParenthesedPrimary,exp0),Double(VhdCharPrimary,
	chr0))) -> Sextuple(Str "749",str0,chr0,match_combined lst0,match_combined lst1,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdCharPrimary,chr0)),Double(VhdParenthesedPrimary,
	exp0)) -> Quintuple(Str "750",str0,chr0,match_combined lst0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdCharPrimary,chr1))) -> Sextuple(Str "751",
	str0,str1,chr0,chr1,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1),Double(VhdCharPrimary,
	chr1))) -> Septuple(Str "752",str0,str1,chr0,chr1,match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdIntPrimary,num0)),Double(VhdParenthesedPrimary,
	exp0)) -> Quintuple(Str "753",str0,num0,match_combined lst0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),
	Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),
	lst1),Double(VhdCharPrimary,chr0))) -> Septuple(Str "754",str0,str1,str2,chr0,match_combined lst0,
	match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),
	Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),
	lst1),Double(VhdCharPrimary,chr0))) -> Septuple(Str "755",str0,str1,str2,chr0,match_combined lst0,
	match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdGreaterOrEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdLessRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdParenthesedPrimary,exp0))) -> Quintuple(Str "756",
	str0,str1,str2,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdGreaterOrEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdIntPrimary,num0)),Triple(VhdLessRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1),Triple(VhdAddSimpleExpression,
	Double(VhdIntPrimary,num1),Triple(VhdMultTerm,Double(VhdIntPrimary,num2),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2)))))) -> Nonuple(Str "757",str0,str1,str2,num0,num1,num2,
	match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "758",
	str0,num0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1))) -> Quintuple(Str "759",str0,
	str1,num0,num1)
| Triple(VhdAndLogicalExpression,Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdLessOrEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1))) -> Quintuple(Str "760",str0,
	str1,num0,num1)
| Triple(VhdAndLogicalExpression,Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdNotEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1))) -> Quintuple(Str "761",str0,
	str1,num0,num1)
| Triple(VhdAndLogicalExpression,Triple(VhdLessOrEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdIntPrimary,num0)),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdCharPrimary,chr0))) -> Sextuple(Str "762",
	str0,str1,num0,chr0,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "763",
	str0,num0,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1))) -> Quintuple(Str "764",str0,
	str1,num0,num1)
| Triple(VhdAndLogicalExpression,Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)),Double(VhdIntPrimary,num1))) -> Quintuple(Str "765",str0,str1,num0,num1)
| Triple(VhdAndLogicalExpression,Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str2),lst0))) -> Quintuple(Str "766",str0,str1,str2,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNamePrimary,Double(VhdSelectedName,lst2))) -> Quadruple(Str "767",
	match_combined lst0,match_combined lst1,match_combined lst2)
| Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSelectedName,lst2)))) -> Quadruple(Str "768",
	match_combined lst0,match_combined lst1,match_combined lst2)
| Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst2),lst3))) -> Quintuple(Str "769",match_combined lst0,match_combined lst1,match_combined lst2,
	match_combined lst3)
| Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "770",match_combined lst0,
	match_combined lst1,match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "771",
	str0,str1,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str1)))) -> Quadruple(Str "772",
	str0,str1,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "773",str0,match_combined lst0,
	match_combined exp0)
| Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1)) -> Quintuple(Str "169",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(VhdAndLogicalExpression,Triple(VhdNotEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdCharPrimary,chr0))) -> Quintuple(Str "774",
	str0,str1,num0,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdNotEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)),Triple(VhdNotEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdCharPrimary,chr0))) -> Quintuple(Str "775",
	str0,str1,num0,chr0)
| Triple(VhdAndLogicalExpression,Triple(VhdNotEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),
	Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdNamePrimary,
	Double(VhdOperatorString,str3)))) -> Quintuple(Str "776",str0,str1,str2,str3)
| Triple(VhdAndLogicalExpression,Triple(VhdNotEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdNotEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str3)))) -> Quintuple(Str "777",str0,str1,str2,str3)
| Triple(VhdAndLogicalExpression,Triple(VhdNotEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdNotEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst0),Double(VhdIntPrimary,
	num0))) -> Sextuple(Str "778",str0,str1,str2,num0,match_combined lst0)
| Triple(VhdAndLogicalExpression,Triple(VhdNotEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),VhdSequentialNull),Triple(VhdGreaterRelation,Double(VhdNamePrimary,
	Double(VhdAttributeName,exp0)),Double(VhdIntPrimary,num0))) -> Quadruple(Str "779",
	str0,num0,match_combined exp0)
| Triple(VhdDivTerm,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))) -> Triple(Str "780",str0,num0)
| Triple(VhdDivTerm,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "781",num0,match_combined exp0)
| Triple(VhdDivTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "782",str0,num0)
| Triple(VhdDivTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Triple(Str "783",str0,str1)
| Triple(VhdDivTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdParenthesedPrimary,
	exp0)) -> Triple(Str "784",str0,match_combined exp0)
| Triple(VhdDivTerm,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0)) -> Triple(Str "785",
	num0,match_combined exp0)
| Triple(VhdDivTerm,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))) -> Triple(Str "786",str0,match_combined exp0)
| Triple(VhdDivTerm,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "787",str0,str1,
	match_combined lst0)
| Triple(VhdDivTerm,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),
	Triple(VhdExpFactor,Double(VhdIntPrimary,num0),Double(VhdParenthesedPrimary,exp0))) -> Quintuple(Str "788",
	str0,num0,match_combined lst0,match_combined exp0)
| Triple(VhdDivTerm,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1)) -> Quintuple(Str "789",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "170",num0,match_combined exp0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdOperatorString,str0)),Double(VhdNamePrimary,
	Double(VhdSelectedName,lst0))) -> Triple(Str "790",str0,match_combined lst0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdOperatorString,str0)),Triple(VhdNameParametersPrimary,
	Double(VhdSelectedName,lst0),lst1)) -> Quadruple(Str "791",str0,match_combined lst0,
	match_combined lst1)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdCharPrimary,
	chr0)) -> Triple(Str "792",chr0,match_combined lst0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "793",num0,match_combined lst0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdNamePrimary,
	Double(VhdOperatorString,str0))) -> Triple(Str "794",str0,match_combined lst0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdNamePrimary,
	Double(VhdSelectedName,lst1))) -> Triple(Str "795",match_combined lst0,match_combined lst1)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "796",str0,match_combined lst0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdParenthesedPrimary,
	exp0)) -> Triple(Str "797",match_combined lst0,match_combined exp0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Triple(VhdNameParametersPrimary,
	Double(VhdSelectedName,lst1),lst2)) -> Quadruple(Str "798",match_combined lst0,match_combined lst1,
	match_combined lst2)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst1)) -> Quadruple(Str "799",str0,match_combined lst0,
	match_combined lst1)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Triple(VhdSubSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0))) -> Quadruple(Str "800",
	num0,match_combined lst0,match_combined exp0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),VhdSequentialNull) -> Double(Str "801",
	match_combined lst0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdCharPrimary,
	chr0)) -> Triple(Str "171",str0,chr0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "172",str0,num0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdOperatorString,str1))) -> Triple(Str "173",str0,str1)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSelectedName,lst0))) -> Triple(Str "802",str0,match_combined lst0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Triple(Str "174",str0,str1)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNegSimpleExpression,
	Double(VhdIntPrimary,num0))) -> Triple(Str "803",str0,num0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdParenthesedPrimary,
	exp0)) -> Triple(Str "804",str0,match_combined exp0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2)))) -> Quadruple(Str "805",str0,str1,str2)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdLdotted,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2)))) -> Quintuple(Str "175",str0,str1,str2,match_combined lst0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0)) -> Quadruple(Str "176",str0,str1,match_combined lst0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0))) -> Quadruple(Str "177",
	str0,str1,num0)
| Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),VhdSequentialNull) -> Double(Str "806",
	str0)
| Triple(VhdEqualRelation,Double(VhdParenthesedPrimary,exp0),Double(VhdCharPrimary,
	chr0)) -> Triple(Str "807",chr0,match_combined exp0)
| Triple(VhdEqualRelation,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSelectedName,lst0))) -> Triple(Str "808",match_combined lst0,match_combined exp0)
| Triple(VhdEqualRelation,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "809",str0,match_combined exp0)
| Triple(VhdEqualRelation,Double(VhdParenthesedPrimary,exp0),Double(VhdParenthesedPrimary,
	exp1)) -> Triple(Str "810",match_combined exp0,match_combined exp1)
| Triple(VhdEqualRelation,Double(VhdParenthesedPrimary,exp0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)) -> Quadruple(Str "811",str0,match_combined lst0,
	match_combined exp0)
| Triple(VhdEqualRelation,Triple(VhdAddSimpleExpression,Triple(VhdDivTerm,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdIntPrimary,
	num0)),Double(VhdNamePrimary,Double(VhdSimpleName,str2))) -> Quintuple(Str "812",
	str0,str1,str2,num0)
| Triple(VhdEqualRelation,Triple(VhdDivTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdIntPrimary,num0)) -> Quadruple(Str "813",
	str0,str1,num0)
| Triple(VhdEqualRelation,Triple(VhdDivTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2))) -> Quadruple(Str "814",str0,str1,str2)
| Triple(VhdEqualRelation,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdCharPrimary,
	chr0)) -> Quintuple(Str "815",str0,chr0,match_combined lst0,match_combined lst1)
| Triple(VhdEqualRelation,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Quintuple(Str "816",str0,str1,match_combined lst0,
	match_combined lst1)
| Triple(VhdEqualRelation,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdCharPrimary,
	chr0)) -> Quintuple(Str "178",str0,str1,chr0,match_combined lst0)
| Triple(VhdEqualRelation,Triple(VhdModTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1)) -> Quadruple(Str "179",
	str0,num0,num1)
| Triple(VhdEqualRelation,Triple(VhdModTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdIntPrimary,num0)) -> Quadruple(Str "180",
	str0,str1,num0)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),
	lst1),Double(VhdCharPrimary,chr0)) -> Quadruple(Str "817",chr0,match_combined lst0,
	match_combined lst1)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),
	lst1),Double(VhdNamePrimary,Double(VhdOperatorString,str0))) -> Quadruple(Str "818",
	str0,match_combined lst0,match_combined lst1)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),
	lst1),Double(VhdNamePrimary,Double(VhdSelectedName,lst2))) -> Quadruple(Str "819",
	match_combined lst0,match_combined lst1,match_combined lst2)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),
	lst1),Double(VhdNamePrimary,Double(VhdSimpleName,str0))) -> Quadruple(Str "820",str0,
	match_combined lst0,match_combined lst1)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),
	lst1),Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst2),lst3)) -> Quintuple(Str "821",
	match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),
	lst1),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst2)) -> Quintuple(Str "822",
	str0,match_combined lst0,match_combined lst1,match_combined lst2)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Double(VhdCharPrimary,chr0)) -> Quadruple(Str "181",str0,chr0,match_combined lst0)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Double(VhdIntPrimary,num0)) -> Quadruple(Str "823",str0,num0,match_combined lst0)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Double(VhdNamePrimary,Double(VhdOperatorString,str1))) -> Quadruple(Str "182",
	str0,str1,match_combined lst0)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "824",str0,
	str1,match_combined lst0)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "825",str0,match_combined lst0,
	match_combined exp0)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1)) -> Quintuple(Str "826",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),
	Double(VhdIntPrimary,num0))) -> Quintuple(Str "827",str0,str1,num0,match_combined lst0)
| Triple(VhdExpFactor,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))) -> Triple(Str "828",str0,num0)
| Triple(VhdExpFactor,Double(VhdIntPrimary,num0),Double(VhdParenthesedPrimary,exp0)) -> Triple(Str "829",
	num0,match_combined exp0)
| Triple(VhdGreaterOrEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str0))) -> Triple(Str "830",str0,match_combined lst0)
| Triple(VhdGreaterOrEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdAttributeName,exp0))) -> Triple(Str "183",str0,match_combined exp0)
| Triple(VhdGreaterOrEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Triple(Str "184",str0,str1)
| Triple(VhdGreaterOrEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdParenthesedPrimary,exp0)) -> Triple(Str "831",str0,match_combined exp0)
| Triple(VhdGreaterOrEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,
	num0))) -> Quadruple(Str "832",str0,str1,num0)
| Triple(VhdGreaterOrEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0)) -> Quadruple(Str "833",
	str0,str1,match_combined lst0)
| Triple(VhdGreaterOrEqualRelation,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,
	num0)) -> Triple(Str "834",num0,match_combined exp0)
| Triple(VhdGreaterOrEqualRelation,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "835",str0,match_combined exp0)
| Triple(VhdGreaterOrEqualRelation,Triple(VhdExpFactor,Double(VhdIntPrimary,num0),
	Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))) -> Quadruple(Str "836",str0,str1,num0)
| Triple(VhdGreaterOrEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdIntPrimary,num0)) -> Quadruple(Str "837",str0,num0,match_combined lst0)
| Triple(VhdGreaterOrEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "838",
	str0,str1,match_combined lst0)
| Triple(VhdGreaterOrEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "839",str0,match_combined lst0,
	match_combined exp0)
| Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "840",num0,match_combined exp0)
| Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdNamePrimary,
	Double(VhdAttributeName,exp1))) -> Triple(Str "841",match_combined exp0,match_combined exp1)
| Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "842",str0,match_combined exp0)
| Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdNamePrimary,
	Double(VhdSelectedName,lst1))) -> Triple(Str "843",match_combined lst0,match_combined lst1)
| Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "185",str0,num0)
| Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdAttributeName,exp0))) -> Triple(Str "844",str0,match_combined exp0)
| Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Triple(Str "186",str0,str1)
| Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdParenthesedPrimary,
	exp0)) -> Triple(Str "845",str0,match_combined exp0)
| Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0)) -> Quadruple(Str "846",str0,str1,match_combined lst0)
| Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)))) -> Quadruple(Str "847",str0,str1,match_combined exp0)
| Triple(VhdGreaterRelation,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,
	num0)) -> Triple(Str "187",num0,match_combined exp0)
| Triple(VhdGreaterRelation,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNamePrimary,Double(VhdSimpleName,str0))) -> Quadruple(Str "848",
	str0,match_combined lst0,match_combined lst1)
| Triple(VhdGreaterRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Double(VhdIntPrimary,num0)) -> Quadruple(Str "849",str0,num0,match_combined lst0)
| Triple(VhdGreaterRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "850",str0,
	str1,match_combined lst0)
| Triple(VhdGreaterRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "851",str0,match_combined lst0,
	match_combined exp0)
| Triple(VhdGreaterRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1)) -> Quintuple(Str "852",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "188",str0,str1,
	match_combined lst0)
| Triple(VhdLessOrEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdIntPrimary,num0)) -> Triple(Str "853",str0,num0)
| Triple(VhdLessOrEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdAttributeName,exp0))) -> Triple(Str "189",str0,match_combined exp0)
| Triple(VhdLessOrEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Triple(Str "854",str0,str1)
| Triple(VhdLessOrEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdParenthesedPrimary,exp0)) -> Triple(Str "855",str0,match_combined exp0)
| Triple(VhdLessOrEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0)) -> Quadruple(Str "856",
	str0,str1,match_combined lst0)
| Triple(VhdLessOrEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNamePrimary,Double(VhdSimpleName,str0))) -> Quadruple(Str "857",
	str0,match_combined lst0,match_combined lst1)
| Triple(VhdLessOrEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdIntPrimary,num0)) -> Quadruple(Str "858",str0,num0,match_combined lst0)
| Triple(VhdLessOrEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "859",
	str0,str1,match_combined lst0)
| Triple(VhdLessOrEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "860",str0,match_combined lst0,
	match_combined exp0)
| Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "861",str0,match_combined exp0)
| Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "190",str0,num0)
| Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdAttributeName,exp0))) -> Triple(Str "862",str0,match_combined exp0)
| Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Triple(Str "863",str0,str1)
| Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdAddSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0),Double(VhdIntPrimary,
	num0))) -> Quintuple(Str "864",str0,str1,num0,match_combined lst0)
| Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0)) -> Quadruple(Str "865",str0,str1,match_combined lst0)
| Triple(VhdLessRelation,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "866",str0,match_combined exp0)
| Triple(VhdLessRelation,Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))) -> Quadruple(Str "867",str0,str1,match_combined exp0)
| Triple(VhdLessRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Double(VhdIntPrimary,num0)) -> Quadruple(Str "868",str0,num0,match_combined lst0)
| Triple(VhdLessRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "869",str0,
	str1,match_combined lst0)
| Triple(VhdLessRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1)) -> Quintuple(Str "870",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(VhdModTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Triple(Str "871",str0,str1)
| Triple(VhdMultTerm,Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1)) -> Triple(Str "872",
	num0,num1)
| Triple(VhdMultTerm,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))) -> Triple(Str "873",str0,num0)
| Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "874",num0,match_combined exp0)
| Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "875",str0,num0)
| Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Triple(Str "876",str0,str1)
| Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdParenthesedPrimary,
	exp0)) -> Triple(Str "877",str0,match_combined exp0)
| Triple(VhdMultTerm,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0)) -> Triple(Str "878",
	num0,match_combined exp0)
| Triple(VhdMultTerm,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1)) -> Quintuple(Str "879",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(VhdNameParametersPrimary,Double(VhdAttributeName,exp0),lst0) -> Triple(Str "880",
	match_combined lst0,match_combined exp0)
| Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),lst1) -> Triple(Str "881",
	match_combined lst0,match_combined lst1)
| Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0) -> Triple(Str "191",
	str0,match_combined lst0)
| Triple(VhdNorLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Triple(Str "882",str0,str1)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),
	Double(VhdNamePrimary,Double(VhdAttributeName,exp1))) -> Triple(Str "192",match_combined exp0,
	match_combined exp1)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdOperatorString,str0)),
	Double(VhdNamePrimary,Double(VhdSelectedName,lst0))) -> Triple(Str "883",str0,match_combined lst0)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdOperatorString,str0)),
	Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),lst1)) -> Quadruple(Str "884",
	str0,match_combined lst0,match_combined lst1)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdCharPrimary,
	chr0)) -> Triple(Str "885",chr0,match_combined lst0)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "886",num0,match_combined lst0)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdNamePrimary,
	Double(VhdOperatorString,str0))) -> Triple(Str "887",str0,match_combined lst0)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "888",str0,match_combined lst0)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),VhdSequentialNull) -> Double(Str "889",
	match_combined lst0)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdCharPrimary,
	chr0)) -> Triple(Str "193",str0,chr0)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0)) -> Triple(Str "194",str0,num0)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdOperatorString,str1))) -> Triple(Str "890",str0,str1)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Triple(Str "195",str0,str1)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdParenthesedPrimary,
	exp0)) -> Triple(Str "891",str0,match_combined exp0)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2)))) -> Quadruple(Str "892",str0,str1,str2)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0))) -> Quadruple(Str "196",
	str0,str1,num0)
| Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),VhdSequentialNull) -> Double(Str "893",
	str0)
| Triple(VhdNotEqualRelation,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,
	num0)) -> Triple(Str "894",num0,match_combined exp0)
| Triple(VhdNotEqualRelation,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2))) -> Quintuple(Str "197",str0,str1,str2,match_combined lst0)
| Triple(VhdNotEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNamePrimary,Double(VhdOperatorString,str0))) -> Quadruple(Str "895",
	str0,match_combined lst0,match_combined lst1)
| Triple(VhdNotEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNamePrimary,Double(VhdSelectedName,lst2))) -> Quadruple(Str "896",
	match_combined lst0,match_combined lst1,match_combined lst2)
| Triple(VhdNotEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst2),lst3)) -> Quintuple(Str "897",
	match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3)
| Triple(VhdNotEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdIntPrimary,num0)) -> Quadruple(Str "898",str0,num0,match_combined lst0)
| Triple(VhdNotEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdOperatorString,str1))) -> Quadruple(Str "899",
	str0,str1,match_combined lst0)
| Triple(VhdNotEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "900",
	str0,str1,match_combined lst0)
| Triple(VhdNotEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1)) -> Quintuple(Str "901",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),
	Double(VhdNamePrimary,Double(VhdSelectedName,lst1))) -> Triple(Str "902",match_combined lst0,
	match_combined lst1)
| Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str0))) -> Triple(Str "903",str0,match_combined lst0)
| Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdOperatorString,str1))) -> Triple(Str "904",str0,str1)
| Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Triple(Str "905",str0,str1)
| Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str1)))) -> Triple(Str "906",
	str0,str1)
| Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdParenthesedPrimary,exp0)) -> Triple(Str "907",str0,match_combined exp0)
| Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0)) -> Quadruple(Str "908",
	str0,str1,match_combined lst0)
| Triple(VhdOrLogicalExpression,Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Triple(Str "909",str0,
	str1)
| Triple(VhdOrLogicalExpression,Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Triple(VhdGreaterRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),
	Double(VhdIntPrimary,num0))) -> Quadruple(Str "910",str0,str1,num0)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSelectedName,lst0))) -> Triple(Str "911",match_combined lst0,match_combined exp0)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "912",str0,match_combined exp0)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdParenthesedPrimary,
	exp1)) -> Triple(Str "198",match_combined exp0,match_combined exp1)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdCharPrimary,chr0))) -> Quadruple(Str "913",
	chr0,match_combined lst0,match_combined exp0)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdCharPrimary,chr0))) -> Quadruple(Str "914",
	str0,chr0,match_combined exp0)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0))) -> Quadruple(Str "199",
	str0,num0,match_combined exp0)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdOperatorString,
	str1)))) -> Quadruple(Str "915",str0,str1,match_combined exp0)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)))) -> Quadruple(Str "200",str0,str1,match_combined exp0)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Double(VhdParenthesedPrimary,exp1),Double(VhdCharPrimary,chr0))) -> Quadruple(Str "916",
	chr0,match_combined exp0,match_combined exp1)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),lst1),Double(VhdCharPrimary,
	chr0))) -> Quintuple(Str "917",chr0,match_combined lst0,match_combined lst1,match_combined exp0)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdCharPrimary,
	chr0))) -> Quintuple(Str "918",str0,chr0,match_combined lst0,match_combined exp0)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,
	Double(VhdOperatorString,str1)))) -> Quintuple(Str "919",str0,str1,match_combined lst0,
	match_combined exp0)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdGreaterRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0))) -> Quadruple(Str "920",
	str0,num0,match_combined exp0)
| Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)) -> Quadruple(Str "921",str0,match_combined lst0,
	match_combined exp0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSelectedName,lst1)),Double(VhdCharPrimary,chr1))) -> Quintuple(Str "922",
	chr0,chr1,match_combined lst0,match_combined lst1)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdCharPrimary,chr1))) -> Quintuple(Str "923",
	str0,chr0,chr1,match_combined lst0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSelectedName,lst1)),Double(VhdCharPrimary,chr0))) -> Quintuple(Str "924",
	str0,chr0,match_combined lst0,match_combined lst1)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "925",
	str0,chr0,match_combined exp0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdCharPrimary,chr1))) -> Quintuple(Str "201",
	str0,str1,chr0,chr1)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,str2)))) -> Quintuple(Str "926",
	str0,str1,str2,chr0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0),Double(VhdCharPrimary,chr1))) -> Sextuple(Str "927",
	str0,str1,chr0,chr1,match_combined lst0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0),Double(VhdNamePrimary,Double(VhdOperatorString,str2)))) -> Sextuple(Str "928",
	str0,str1,str2,chr0,match_combined lst0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "929",
	str0,num0,match_combined exp0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdCharPrimary,chr0))) -> Quintuple(Str "930",
	str0,str1,num0,chr0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1))) -> Quintuple(Str "202",str0,
	str1,num0,num1)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdGreaterRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1))) -> Quintuple(Str "931",str0,
	str1,num0,num1)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdCharPrimary,chr0))) -> Quintuple(Str "932",
	str0,str1,str2,chr0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdNamePrimary,Double(VhdOperatorString,
	str3)))) -> Quintuple(Str "933",str0,str1,str2,str3)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdParenthesedPrimary,
	exp0)) -> Quadruple(Str "934",str0,str1,match_combined exp0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdCharPrimary,chr0))) -> Quintuple(Str "935",
	str0,str1,str2,chr0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdNamePrimary,Double(VhdOperatorString,
	str3)))) -> Quintuple(Str "936",str0,str1,str2,str3)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str3)))) -> Quintuple(Str "203",str0,str1,str2,str3)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdNotEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst0),Double(VhdIntPrimary,
	num0))) -> Sextuple(Str "937",str0,str1,str2,num0,match_combined lst0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdCharPrimary,chr0)),Double(VhdParenthesedPrimary,
	exp0)) -> Quintuple(Str "938",str0,chr0,match_combined lst0,match_combined exp0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdCharPrimary,chr0)),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1),Double(VhdCharPrimary,
	chr1))) -> Septuple(Str "204",str0,str1,chr0,chr1,match_combined lst0,match_combined lst1)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdIntPrimary,num0)),Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1),Double(VhdIntPrimary,
	num1))) -> Septuple(Str "939",str0,str1,num0,num1,match_combined lst0,match_combined lst1)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),
	Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdCharPrimary,
	chr0))) -> Sextuple(Str "333",str0,str1,str2,chr0,match_combined lst0)
| Triple(VhdOrLogicalExpression,Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),
	Triple(VhdEqualRelation,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),
	lst1),Double(VhdNamePrimary,Double(VhdOperatorString,str3)))) -> Septuple(Str "940",
	str0,str1,str2,str3,match_combined lst0,match_combined lst1)
| Triple(VhdOrLogicalExpression,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),
	Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str2),lst1),
	Double(VhdNamePrimary,Double(VhdSimpleName,str3)))) -> Septuple(Str "941",str0,str1,
	str2,str3,match_combined lst0,match_combined lst1)
| Triple(VhdOrLogicalExpression,Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Triple(VhdGreaterRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdCharPrimary,chr1))) -> Quintuple(Str "942",
	str0,str1,chr0,chr1)
| Triple(VhdOrLogicalExpression,Triple(VhdLessRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdAttributeName,exp0))),Triple(VhdGreaterRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdAttributeName,
	exp1)))) -> Quintuple(Str "206",str0,str1,match_combined exp0,match_combined exp1)
| Triple(VhdOrLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNamePrimary,Double(VhdSelectedName,lst2))) -> Quadruple(Str "943",
	match_combined lst0,match_combined lst1,match_combined lst2)
| Triple(VhdOrLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str0)))) -> Quadruple(Str "944",
	str0,match_combined lst0,match_combined lst1)
| Triple(VhdOrLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst2),lst3))) -> Quintuple(Str "945",match_combined lst0,match_combined lst1,match_combined lst2,
	match_combined lst3)
| Triple(VhdOrLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdParenthesedPrimary,exp0)) -> Quadruple(Str "946",match_combined lst0,
	match_combined lst1,match_combined exp0)
| Triple(VhdOrLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst2),lst3)) -> Quintuple(Str "947",
	match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3)
| Triple(VhdOrLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "207",
	str0,str1,match_combined lst0)
| Triple(VhdOrLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1)) -> Quintuple(Str "948",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(VhdOrLogicalExpression,Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdNotEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1))) -> Quintuple(Str "949",str0,
	str1,num0,num1)
| Triple(VhdOrLogicalExpression,Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdNotEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str3)))) -> Quintuple(Str "950",str0,str1,str2,str3)
| Triple(VhdSubSimpleExpression,Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1)) -> Triple(Str "951",
	num0,num1)
| Triple(VhdSubSimpleExpression,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))) -> Triple(Str "208",str0,num0)
| Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),
	Double(VhdIntPrimary,num0)) -> Triple(Str "952",num0,match_combined exp0)
| Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str0))) -> Triple(Str "209",str0,match_combined exp0)
| Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),
	Double(VhdIntPrimary,num0)) -> Triple(Str "953",num0,match_combined lst0)
| Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdIntPrimary,num0)) -> Triple(Str "954",str0,num0)
| Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdSelectedName,lst0))) -> Triple(Str "955",str0,match_combined lst0)
| Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Triple(Str "956",str0,str1)
| Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdParenthesedPrimary,exp0)) -> Triple(Str "957",str0,match_combined exp0)
| Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,
	num0))) -> Quadruple(Str "958",str0,str1,num0)
| Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0)) -> Quadruple(Str "959",
	str0,str1,match_combined lst0)
| Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,
	num0)) -> Triple(Str "960",num0,match_combined exp0)
| Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "961",str0,match_combined exp0)
| Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdMultTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0))) -> Quadruple(Str "962",
	str0,num0,match_combined exp0)
| Triple(VhdSubSimpleExpression,Triple(VhdDivTerm,Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1)) -> Quadruple(Str "963",
	num0,num1,match_combined exp0)
| Triple(VhdSubSimpleExpression,Triple(VhdDivTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1)) -> Quadruple(Str "964",
	str0,num0,num1)
| Triple(VhdSubSimpleExpression,Triple(VhdDivTerm,Double(VhdParenthesedPrimary,exp0),
	Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1)) -> Quadruple(Str "965",num0,
	num1,match_combined exp0)
| Triple(VhdSubSimpleExpression,Triple(VhdExpFactor,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))),Double(VhdIntPrimary,num1)) -> Quadruple(Str "966",str0,
	num0,num1)
| Triple(VhdSubSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdIntPrimary,num0)) -> Quadruple(Str "967",str0,num0,match_combined lst0)
| Triple(VhdSubSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "968",
	str0,str1,match_combined lst0)
| Triple(VhdSubSimpleExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1)) -> Quintuple(Str "969",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(VhdXnorLogicalExpression,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),
	Double(VhdNamePrimary,Double(VhdAttributeName,exp1))) -> Triple(Str "970",match_combined exp0,
	match_combined exp1)
| Triple(VhdXorLogicalExpression,Double(VhdConcatSimpleExpression,lst0),Double(VhdConcatSimpleExpression,
	lst1)) -> Triple(Str "210",match_combined lst0,match_combined lst1)
| Triple(VhdXorLogicalExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),
	Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst1),lst2)) -> Quadruple(Str "971",
	match_combined lst0,match_combined lst1,match_combined lst2)
| Triple(VhdXorLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Triple(Str "972",str0,str1)
| Triple(VhdXorLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0)) -> Quadruple(Str "973",
	str0,str1,match_combined lst0)
| Triple(VhdXorLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdConcatSimpleExpression,
	lst0)) -> Triple(Str "211",match_combined lst0,match_combined exp0)
| Triple(VhdXorLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "974",str0,match_combined exp0)
| Triple(VhdXorLogicalExpression,Double(VhdParenthesedPrimary,exp0),Double(VhdParenthesedPrimary,
	exp1)) -> Triple(Str "975",match_combined exp0,match_combined exp1)
| Triple(VhdXorLogicalExpression,Double(VhdParenthesedPrimary,exp0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)) -> Quadruple(Str "976",str0,match_combined lst0,
	match_combined exp0)
| Triple(VhdXorLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst2),lst3)) -> Quintuple(Str "977",
	match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3)
| Triple(VhdXorLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "978",
	str0,str1,match_combined lst0)
| Triple(VhdXorLogicalExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1)) -> Quintuple(Str "979",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Double(VhdAggregatePrimary,lst0))) -> Triple(Str "980",
	str0,match_combined lst0)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Double(VhdCharPrimary,chr0))) -> Triple(Str "212",
	str0,chr0)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Double(VhdConcatSimpleExpression,lst0))) -> Triple(Str "981",
	str0,match_combined lst0)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Double(VhdFloatPrimary,real0))) -> Triple(Str "982",
	str0,real0)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Double(VhdIntPrimary,num0))) -> Triple(Str "213",
	str0,num0)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Double(VhdNamePrimary,Double(VhdOperatorString,
	str1)))) -> Triple(Str "983",str0,str1)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)))) -> Triple(Str "984",
	str0,match_combined lst0)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)))) -> Triple(Str "214",
	str0,str1)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0)))) -> Quadruple(Str "985",
	str0,str1,num0)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,str2))))) -> Quadruple(Str "215",
	str0,str1,str2)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0))) -> Quadruple(Str "216",str0,str1,match_combined lst0)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Triple(VhdNotEqualRelation,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0)))) -> Quadruple(Str "986",
	str0,str1,num0)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num0)))) -> Quadruple(Str "217",
	str0,str1,num0)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdActualExpression,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,str2))))) -> Quadruple(Str "987",
	str0,str1,str2)
| Triple(Vhdassociation_element,Double(VhdFormalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),VhdActualOpen) -> Double(Str "988",str0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1))))) -> Triple(Str "218",
	num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)))))) -> Triple(Str "989",str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdIntPrimary,num0),Double(VhdParenthesedPrimary,
	exp0))))) -> Triple(Str "990",num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdIntPrimary,num0),Triple(VhdExpFactor,Double(VhdIntPrimary,
	num1),Double(VhdNamePrimary,Double(VhdSimpleName,str0))))))) -> Quadruple(Str "219",
	str0,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdIntPrimary,num0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0))))) -> Quadruple(Str "991",str0,num0,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdIntPrimary,
	num0))))) -> Triple(Str "992",num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)))))) -> Triple(Str "993",str0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0))))) -> Triple(Str "220",str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdAddSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0)))))) -> Quadruple(Str "994",
	str0,num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2))))))) -> Quadruple(Str "995",str0,str1,str2)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdSubSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))))))) -> Quadruple(Str "996",
	str0,str1,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,
	num0))))) -> Triple(Str "997",num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)))))) -> Triple(Str "998",str0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdParenthesedPrimary,exp0),Double(VhdParenthesedPrimary,
	exp1))))) -> Triple(Str "999",match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Double(VhdParenthesedPrimary,exp0),Triple(VhdMultTerm,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))))) -> Quadruple(Str "1000",
	str0,num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1))))) -> Quadruple(Str "1001",
	str0,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1)))))) -> Quadruple(Str "1002",
	str0,str1,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1))))) -> Quadruple(Str "1003",
	num0,num1,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0),Double(VhdIntPrimary,num0))))) -> Quadruple(Str "1004",str0,num0,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdIntPrimary,num0),
	Double(VhdIntPrimary,num1)),Double(VhdIntPrimary,num2))))) -> Quadruple(Str "1005",
	num0,num1,num2)
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
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1)))))) -> Quadruple(Str "1006",
	str0,str1,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdParenthesedPrimary,exp0))))) -> Quadruple(Str "1007",
	str0,num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1)))))) -> Quintuple(Str "1008",
	str0,str1,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num1)))))) -> Quintuple(Str "1009",str0,num0,num1,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0))))) -> Quintuple(Str "1010",str0,str1,num0,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,str2))))))) -> Quintuple(Str "1011",
	str0,str1,str2,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1))))) -> Quadruple(Str "224",
	num0,num1,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,str0)))))) -> Quadruple(Str "225",
	str0,num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num0)),Double(VhdParenthesedPrimary,exp1))))) -> Quadruple(Str "1012",
	num0,match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num0)),Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))))))) -> Quintuple(Str "1013",
	str0,str1,num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num0)),Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdParenthesedPrimary,exp1)))))) -> Quintuple(Str "1014",str0,num0,
	match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num0)),Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp1),Double(VhdNamePrimary,Double(VhdSimpleName,str0))))))) -> Quintuple(Str "1015",
	str0,num0,match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Triple(VhdDivTerm,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1)),
	Double(VhdIntPrimary,num2))))) -> Quintuple(Str "1016",str0,num0,num1,num2)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Triple(VhdExpFactor,Double(VhdIntPrimary,
	num0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdIntPrimary,num1)),
	Double(VhdIntPrimary,num2))))) -> Quintuple(Str "1017",str0,num0,num1,num2)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Triple(VhdMultTerm,Double(VhdIntPrimary,
	num0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdIntPrimary,num1)),
	Double(VhdIntPrimary,num2))))) -> Quintuple(Str "1018",str0,num0,num1,num2)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Triple(VhdMultTerm,Double(VhdIntPrimary,
	num0),Double(VhdParenthesedPrimary,exp0)),Double(VhdIntPrimary,num1)),Triple(VhdMultTerm,
	Double(VhdIntPrimary,num2),Double(VhdNamePrimary,Double(VhdSimpleName,str0))))))) -> Sextuple(Str "226",
	str0,num0,num1,num2,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Triple(VhdMultTerm,Double(VhdIntPrimary,
	num0),Double(VhdParenthesedPrimary,exp0)),Double(VhdIntPrimary,num1)),Triple(VhdSubSimpleExpression,
	Triple(VhdMultTerm,Double(VhdIntPrimary,num2),Double(VhdParenthesedPrimary,exp1)),
	Double(VhdIntPrimary,num3)))))) -> Septuple(Str "1019",num0,num1,num2,num3,match_combined exp0,
	match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Triple(VhdMultTerm,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1)),
	Double(VhdIntPrimary,num2))))) -> Quintuple(Str "1020",str0,num0,num1,num2)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Triple(VhdMultTerm,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1)),
	Triple(VhdMultTerm,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num2)))))) -> Sextuple(Str "1021",
	str0,num0,num1,num2,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Triple(VhdMultTerm,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdIntPrimary,
	num0)),Double(VhdIntPrimary,num1))))) -> Quintuple(Str "1022",str0,str1,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Triple(VhdMultTerm,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdParenthesedPrimary,exp0)),Double(VhdIntPrimary,
	num0)),Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2))))))) -> Sextuple(Str "1023",str0,str1,str2,num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Triple(VhdMultTerm,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1)),Triple(VhdMultTerm,
	Double(VhdIntPrimary,num2),Double(VhdNamePrimary,Double(VhdSimpleName,str0))))))) -> Sextuple(Str "1024",
	str0,num0,num1,num2,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Triple(VhdMultTerm,Double(VhdParenthesedPrimary,
	exp0),Double(VhdParenthesedPrimary,exp1)),Double(VhdIntPrimary,num0)),Triple(VhdMultTerm,
	Double(VhdParenthesedPrimary,exp2),Double(VhdNamePrimary,Double(VhdSimpleName,str0))))))) -> Sextuple(Str "1025",
	str0,num0,match_combined exp0,match_combined exp1,match_combined exp2)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdDecreasingRange,Triple(VhdSubSimpleExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,
	num1))))) -> Quintuple(Str "1026",str0,num0,num1,match_combined lst0)
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
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Double(VhdParenthesedPrimary,
	exp0))))) -> Triple(Str "1027",num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdIntPrimary,num1)))))) -> Quadruple(Str "1028",
	num0,num1,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0))))) -> Quadruple(Str "1029",str0,num0,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdIntPrimary,num1)))))) -> Quadruple(Str "1030",
	num0,num1,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num1)))))) -> Quadruple(Str "1031",
	str0,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num1)))))) -> Quadruple(Str "1032",
	num0,num1,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,
	Triple(VhdExpFactor,Double(VhdIntPrimary,num1),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdIntPrimary,num2)))))) -> Quintuple(Str "1033",str0,num0,num1,num2)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,
	Triple(VhdMultTerm,Double(VhdIntPrimary,num1),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdIntPrimary,num2)))))) -> Quintuple(Str "1034",str0,num0,num1,num2)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,
	Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num1)),Double(VhdIntPrimary,num2)))))) -> Quintuple(Str "1035",str0,num0,num1,num2)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdIntPrimary,
	num1)))))) -> Quintuple(Str "1036",str0,num0,num1,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdNamePrimary,
	Double(VhdAttributeName,exp1)))))) -> Triple(Str "230",match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdAttributeName,exp1)),Double(VhdIntPrimary,num0)))))) -> Quadruple(Str "231",
	num0,match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)))))) -> Triple(Str "1037",str0,str1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdSubSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0)))))) -> Quadruple(Str "1038",
	str0,num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,
	num0))))) -> Triple(Str "1039",num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Double(VhdParenthesedPrimary,exp0),Double(VhdParenthesedPrimary,
	exp1))))) -> Triple(Str "1040",match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdAttributeName,
	exp1)))))) -> Quadruple(Str "232",num0,match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdIntPrimary,num1))))) -> Quadruple(Str "1041",
	str0,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1)))))) -> Quadruple(Str "1042",
	str0,str1,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdSimpleName,str2))))))) -> Quintuple(Str "1043",
	str0,str1,str2,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,str0)))))) -> Quadruple(Str "1044",
	str0,num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdAddSimpleExpression,Triple(VhdMultTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1)),Double(VhdIntPrimary,
	num2)))))) -> Sextuple(Str "1045",str0,str1,num0,num1,num2)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdSubSimpleExpression,Double(VhdIntPrimary,num0),
	Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdIntPrimary,num1))))) -> Quadruple(Str "1046",
	str0,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdAttributeName,
	exp1)))))) -> Quadruple(Str "1047",num0,match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0)),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Triple(VhdSubSimpleExpression,
	Double(VhdParenthesedPrimary,exp1),Double(VhdIntPrimary,num0)))))) -> Quintuple(Str "1048",
	str0,num0,match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1)))))) -> Quadruple(Str "1049",
	str0,str1,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str1)),Double(VhdIntPrimary,num1)))))) -> Quintuple(Str "1050",
	str0,str1,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str2)),Double(VhdIntPrimary,num0)))))) -> Quintuple(Str "1051",
	str0,str1,str2,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Triple(VhdAddSimpleExpression,
	Double(VhdParenthesedPrimary,exp1),Double(VhdParenthesedPrimary,exp2)))))) -> Quintuple(Str "1052",
	str0,match_combined exp0,match_combined exp1,match_combined exp2)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Triple(VhdSubSimpleExpression,
	Double(VhdParenthesedPrimary,exp1),Double(VhdIntPrimary,num0)))))) -> Quintuple(Str "1053",
	str0,num0,match_combined exp0,match_combined exp1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdRange,
	Triple(VhdIncreasingRange,Triple(VhdSubSimpleExpression,Triple(VhdMultTerm,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))),Triple(VhdSubSimpleExpression,Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str2)),Double(VhdIntPrimary,num1)),Double(VhdIntPrimary,num2)))))) -> Septuple(Str "1054",
	str0,str1,str2,num0,num1,num2)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdSubTypeRange,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str0),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),
	Double(VhdIntPrimary,num1))))))) -> Quadruple(Str "1055",str0,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdSubTypeRange,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str0),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),
	Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),
	Double(VhdIntPrimary,num1)))))))) -> Quintuple(Str "1056",str0,num0,num1,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualDiscreteRange,Double(VhdSubTypeRange,
	Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,Str ""),Double(VhdSimpleName,
	str0),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,Double(VhdIntPrimary,num0),
	Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),Double(VhdIntPrimary,
	num1)))))))) -> Quintuple(Str "1057",str0,str1,num0,num1)
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
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdCharPrimary,
	chr0))) -> Double(Str "1058",chr0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdConcatSimpleExpression,
	lst0))) -> Double(Str "1059",match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdIntPrimary,
	num0))) -> Double(Str "237",num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdNamePrimary,
	Double(VhdAttributeName,exp0)))) -> Double(Str "238",match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdNamePrimary,
	Double(VhdOperatorString,Str "")))) -> (Str "1060")
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdNamePrimary,
	Double(VhdOperatorString,str0)))) -> Double(Str "239",str0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdNamePrimary,
	Double(VhdSelectedName,lst0)))) -> Double(Str "1061",match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)))) -> Double(Str "240",str0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdNotFactor,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0))))) -> Double(Str "241",str0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdNotFactor,
	Triple(VhdNameParametersPrimary,Double(VhdSelectedName,lst0),lst1)))) -> Triple(Str "1062",
	match_combined lst0,match_combined lst1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdParenthesedPrimary,
	exp0))) -> Double(Str "1063",match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Double(VhdQualifiedExpressionPrimary,
	Triple(VhdQualifiedExpression,Double(VhdSuffixSimpleName,Double(VhdSimpleName,str0)),
	Double(VhdNamePrimary,Double(VhdOperatorString,str1)))))) -> Triple(Str "1064",str0,
	str1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Double(VhdIntPrimary,num0),Triple(VhdDivTerm,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst1))))) -> Sextuple(Str "1065",
	str0,str1,num0,match_combined lst0,match_combined lst1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "1066",
	num0,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "242",
	str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdAttributeName,
	exp0))))) -> Triple(Str "1067",str0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "243",str0,str1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdParenthesedPrimary,exp0)))) -> Triple(Str "244",
	str0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0)))) -> Triple(Str "1068",
	num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))))) -> Triple(Str "1069",
	str0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)))) -> Quadruple(Str "245",str0,match_combined lst0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Triple(VhdMultTerm,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))),Double(VhdIntPrimary,num1)))) -> Quadruple(Str "1070",str0,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Triple(VhdMultTerm,Double(VhdIntPrimary,num0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Double(VhdIntPrimary,num1)))) -> Quintuple(Str "1071",str0,num0,num1,
	match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0)),Double(VhdIntPrimary,num1)))) -> Quadruple(Str "1072",str0,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))),Double(VhdNamePrimary,Double(VhdSimpleName,str2))))) -> Quadruple(Str "1073",
	str0,str1,str2)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdIntPrimary,
	num0)))) -> Quadruple(Str "246",str0,num0,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))))) -> Quadruple(Str "1074",str0,str1,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAddSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1)))) -> Quintuple(Str "247",str0,str1,match_combined lst0,
	match_combined lst1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAndLogicalExpression,
	Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),
	Double(VhdIntPrimary,num1))))) -> Quintuple(Str "1075",str0,str1,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAndLogicalExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1)))) -> Quintuple(Str "1076",str0,str1,match_combined lst0,
	match_combined lst1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdAndLogicalExpression,
	Triple(VhdNotEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,
	num0)),Triple(VhdEqualRelation,Double(VhdNamePrimary,Double(VhdSimpleName,str1)),
	Double(VhdIntPrimary,num1))))) -> Quintuple(Str "1077",str0,str1,num0,num1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdDivTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "1078",
	str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdDivTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "1079",str0,str1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdDivTerm,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0)))) -> Triple(Str "1080",
	num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdDivTerm,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))))) -> Quadruple(Str "248",str0,str1,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdEqualRelation,
	Double(VhdConcatSimpleExpression,lst0),Double(VhdNamePrimary,Double(VhdOperatorString,
	str0))))) -> Triple(Str "1081",str0,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "1082",
	str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdOperatorString,
	str1))))) -> Triple(Str "1083",str0,str1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "1084",str0,str1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdEqualRelation,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0)))) -> Triple(Str "1085",
	num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdEqualRelation,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))))) -> Quadruple(Str "1086",str0,str1,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdGreaterRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "1087",
	str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdGreaterRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "1088",str0,str1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdLdotted,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))))) -> Quadruple(Str "249",str0,str1,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdLessRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "1089",
	str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdModTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "1090",
	str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdModTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "1091",str0,str1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdMultTerm,
	Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))))) -> Triple(Str "1092",
	str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdMultTerm,
	Double(VhdIntPrimary,num0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0)))) -> Quadruple(Str "1093",str0,num0,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdMultTerm,
	Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "1094",
	num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdMultTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "1095",
	str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdMultTerm,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "1096",str0,str1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdMultTerm,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1)))) -> Quintuple(Str "250",str0,str1,match_combined lst0,
	match_combined lst1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSelectedName,lst0),lst1))) -> Triple(Str "1097",match_combined lst0,match_combined lst1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0))) -> Triple(Str "251",str0,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdNotEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "1098",
	str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdNotEqualRelation,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "1099",str0,str1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdSubSimpleExpression,
	Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))))) -> Triple(Str "1100",
	str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "1101",
	num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdAttributeName,exp0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))))) -> Triple(Str "1102",str0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdIntPrimary,num0)))) -> Triple(Str "252",
	str0,num0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))))) -> Triple(Str "1103",str0,str1)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdSubSimpleExpression,
	Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst0)))) -> Quadruple(Str "1104",str0,str1,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdSubSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdIntPrimary,num0)))) -> Triple(Str "1105",
	num0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdSubSimpleExpression,
	Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))))) -> Triple(Str "253",
	str0,match_combined exp0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdSubSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdIntPrimary,
	num0)))) -> Quadruple(Str "254",str0,num0,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdSubSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))))) -> Quadruple(Str "1106",str0,str1,match_combined lst0)
| Triple(Vhdassociation_element,VhdFormalIndexed,Double(VhdActualExpression,Triple(VhdSubSimpleExpression,
	Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1)))) -> Quintuple(Str "1107",str0,str1,match_combined lst0,
	match_combined lst1)
| Triple(Vhdassociation_element,VhdFormalIndexed,VhdActualOpen) -> (Str "1108")
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
| Triple(Vhdelement_association,lst0,Double(VhdConcatSimpleExpression,lst1)) -> Triple(Str "1109",
	match_combined lst0,match_combined lst1)
| Triple(Vhdelement_association,lst0,Double(VhdIntPrimary,num0)) -> Triple(Str "1110",
	num0,match_combined lst0)
| Triple(Vhdelement_association,lst0,Double(VhdNamePrimary,Double(VhdOperatorString,
	str0))) -> Triple(Str "260",str0,match_combined lst0)
| Triple(Vhdelement_association,lst0,Double(VhdNamePrimary,Double(VhdSimpleName,str0))) -> Triple(Str "334",
	str0,match_combined lst0)
| Triple(Vhdelement_association,lst0,Double(VhdNegSimpleExpression,Double(VhdIntPrimary,
	num0))) -> Triple(Str "1111",num0,match_combined lst0)
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
	Str ""),Double(VhdSimpleName,str0),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,
	Double(VhdIntPrimary,num0),Double(VhdIntPrimary,num1))))) -> Quintuple(Str "1112",
	str0,num0,num1,match_combined lst0)
| Triple(Vhdelement_declaration,lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,
	Str ""),Double(VhdSimpleName,str0),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,
	Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,str1)))))) -> Quintuple(Str "1113",
	str0,str1,num0,match_combined lst0)
| Triple(Vhdelement_declaration,lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,
	Str ""),Double(VhdSimpleName,str0),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,
	Double(VhdIntPrimary,num0),Triple(VhdAddSimpleExpression,Triple(VhdDivTerm,Double(VhdIntPrimary,
	num1),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdIntPrimary,num2)))))) -> Septuple(Str "1114",
	str0,str1,num0,num1,num2,match_combined lst0)
| Triple(Vhdelement_declaration,lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,
	Str ""),Double(VhdSimpleName,str0),Double(VhdRangeConstraint,Triple(VhdIncreasingRange,
	Double(VhdIntPrimary,num0),Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)),Double(VhdIntPrimary,num1)))))) -> Sextuple(Str "1115",str0,str1,num0,num1,
	match_combined lst0)
| Triple(Vhdelement_declaration,lst0,Quadruple(Vhdsubtype_indication,Double(VhdSimpleName,
	Str ""),Double(VhdSimpleName,str0),VhdNoConstraint)) -> Triple(Str "268",str0,match_combined lst0)
| Triple(Vhdentity_designator,Double(VhdEntityTagSimpleName,str0),VhdNone) -> Double(Str "1116",
	str0)
| Triple(Vhdselected_waveform,lst0,lst1) -> Triple(Str "1117",match_combined lst0,
	match_combined lst1)
| Triple(Vhdwaveform_element,Double(VhdAggregatePrimary,lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Double(Str "269",match_combined lst0)
| Triple(Vhdwaveform_element,Double(VhdAggregatePrimary,lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "1118",str0,match_combined lst0)
| Triple(Vhdwaveform_element,Double(VhdCharPrimary,chr0),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Double(Str "270",chr0)
| Triple(Vhdwaveform_element,Double(VhdCharPrimary,chr0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))) -> Triple(Str "271",str0,chr0)
| Triple(Vhdwaveform_element,Double(VhdCharPrimary,chr0),Double(VhdPhysicalPrimary,
	Triple(VhdPhysicalInteger,num0,str0))) -> Quadruple(Str "272",str0,num0,chr0)
| Triple(Vhdwaveform_element,Double(VhdConcatSimpleExpression,lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Double(Str "273",match_combined lst0)
| Triple(Vhdwaveform_element,Double(VhdConcatSimpleExpression,lst0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "1119",str0,match_combined lst0)
| Triple(Vhdwaveform_element,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Double(Str "274",num0)
| Triple(Vhdwaveform_element,Double(VhdIntPrimary,num0),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))) -> Triple(Str "1120",str0,num0)
| Triple(Vhdwaveform_element,Double(VhdNamePrimary,Double(VhdOperatorString,str0)),
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Double(Str "275",str0)
| Triple(Vhdwaveform_element,Double(VhdNamePrimary,Double(VhdSelectedName,lst0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Double(Str "1121",match_combined lst0)
| Triple(Vhdwaveform_element,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Double(Str "276",str0)
| Triple(Vhdwaveform_element,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Triple(Str "1122",str0,str1)
| Triple(Vhdwaveform_element,Double(VhdNamePrimary,Double(VhdSimpleName,str0)),Double(VhdPhysicalPrimary,
	Triple(VhdPhysicalInteger,num0,str1))) -> Quadruple(Str "1123",str0,str1,num0)
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
| Triple(Vhdwaveform_element,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "1124",
	match_combined lst0,match_combined lst1)
| Triple(Vhdwaveform_element,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "281",
	str0,match_combined lst0)
| Triple(Vhdwaveform_element,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Double(Str "282",match_combined exp0)
| Triple(Vhdwaveform_element,Double(VhdParenthesedPrimary,exp0),Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))) -> Triple(Str "1125",str0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "1126",
	num0,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "1127",
	str0,chr0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "283",
	str0,num0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "1128",
	str0,str1,num0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "1129",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2))) -> Quadruple(Str "1130",str0,str1,str2)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "1131",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2))) -> Quadruple(Str "1132",str0,str1,str2)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1133",str0,str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2))) -> Quintuple(Str "1134",str0,str1,str2,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "284",
	num0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNamePrimary,Double(VhdOperatorString,str0))),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Quadruple(Str "1135",str0,str1,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))) -> Quadruple(Str "1136",str0,str1,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1137",str0,match_combined lst0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdCharPrimary,chr0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1138",str0,chr0,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Quintuple(Str "1139",str0,str1,num0,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1140",str0,
	str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdQualifiedExpressionPrimary,Triple(VhdQualifiedExpression,
	Double(VhdSuffixSimpleName,Double(VhdSimpleName,str1)),Double(VhdNamePrimary,Double(VhdOperatorString,
	str2))))),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quintuple(Str "1141",
	str0,str1,str2,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdAddSimpleExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quintuple(Str "1142",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "285",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2))) -> Quadruple(Str "1143",str0,str1,str2)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSelectedName,
	lst0)))),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "1144",
	str0,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)))),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "286",
	str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNotFactor,Double(VhdParenthesedPrimary,exp0))),
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "1145",str0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdParenthesedPrimary,exp0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "287",str0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdParenthesedPrimary,exp0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Quadruple(Str "1146",str0,str1,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quadruple(Str "288",
	str0,str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "1147",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))),Double(VhdParenthesedPrimary,exp0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "1148",str0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Double(VhdParenthesedPrimary,
	exp0)),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "1149",str0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Double(VhdParenthesedPrimary,
	exp0)),Double(VhdNotFactor,Double(VhdParenthesedPrimary,exp1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "1150",match_combined exp0,match_combined exp1)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Double(VhdParenthesedPrimary,
	exp0)),Double(VhdParenthesedPrimary,exp1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "1151",match_combined exp0,match_combined exp1)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Double(VhdParenthesedPrimary,
	exp0)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1152",str0,match_combined lst0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSelectedName,lst0),lst1)),Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst2),lst3)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quintuple(Str "1153",
	match_combined lst0,match_combined lst1,match_combined lst2,match_combined lst3)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quadruple(Str "337",str0,
	str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "1154",str0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))) -> Quadruple(Str "1155",str0,str1,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str0)))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "1156",str0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNotFactor,Double(VhdParenthesedPrimary,exp1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "1157",match_combined exp0,match_combined exp1)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNotFactor,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),
	lst0))),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1158",
	str0,match_combined lst0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdParenthesedPrimary,exp1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "1159",match_combined exp0,match_combined exp1)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1160",str0,match_combined lst0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quadruple(Str "338",str0,
	str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,
	str1)))),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1161",
	str0,str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNotFactor,Double(VhdParenthesedPrimary,
	exp0))),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1162",
	str0,match_combined lst0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdParenthesedPrimary,exp0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1163",str0,match_combined lst0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdAndLogicalExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quintuple(Str "1164",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(Vhdwaveform_element,Triple(VhdDivTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "1165",
	str0,num0)
| Triple(Vhdwaveform_element,Triple(VhdExpFactor,Double(VhdIntPrimary,num0),Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1166",
	str0,num0,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdLdotted,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Quadruple(Str "291",str0,str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdModTerm,Double(VhdParenthesedPrimary,exp0),
	Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))) -> Quadruple(Str "1167",str0,str1,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdMultTerm,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "1168",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdMultTerm,Double(VhdParenthesedPrimary,exp0),
	Double(VhdParenthesedPrimary,exp1)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "1169",
	match_combined exp0,match_combined exp1)
| Triple(Vhdwaveform_element,Triple(VhdMultTerm,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdParenthesedPrimary,exp0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Quadruple(Str "1170",str0,match_combined lst0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdNameParametersPrimary,Double(VhdSelectedName,
	lst0),lst1),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "1171",
	match_combined lst0,match_combined lst1)
| Triple(Vhdwaveform_element,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "292",
	str0,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "293",
	str0,str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str0),lst0),Double(VhdPhysicalPrimary,Triple(VhdPhysicalInteger,num0,str1))) -> Quintuple(Str "1172",
	str0,str1,num0,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdNorLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "294",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "295",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str1)))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "1173",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdParenthesedPrimary,exp0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "1174",str0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdParenthesedPrimary,exp0)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))) -> Quadruple(Str "1175",str0,str1,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Double(VhdNotFactor,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0))),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "1176",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0)),Double(VhdNotFactor,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str1),lst1))),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quintuple(Str "339",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "1177",str0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdParenthesedPrimary,exp1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "1178",match_combined exp0,match_combined exp1)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdParenthesedPrimary,exp1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	str0))) -> Quadruple(Str "1179",str0,match_combined exp0,match_combined exp1)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1180",str0,match_combined lst0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),
	Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1181",str0,
	str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdOrLogicalExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quintuple(Str "297",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(Vhdwaveform_element,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdCharPrimary,chr0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "1182",
	str0,chr0)
| Triple(Vhdwaveform_element,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "1183",
	str0,num0)
| Triple(Vhdwaveform_element,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))) -> Quadruple(Str "1184",
	str0,str1,num0)
| Triple(Vhdwaveform_element,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "1185",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdOperatorString,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,str2))) -> Quadruple(Str "1186",str0,str1,str2)
| Triple(Vhdwaveform_element,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "1187",str0,str1)
| Triple(Vhdwaveform_element,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,Double(VhdSimpleName,
	str2))) -> Quadruple(Str "1188",str0,str1,str2)
| Triple(Vhdwaveform_element,Triple(VhdSubSimpleExpression,Double(VhdNamePrimary,Double(VhdSimpleName,
	str0)),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str1),lst0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1189",str0,str1,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Triple(Str "1190",
	num0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNamePrimary,Double(VhdOperatorString,str0))),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Quadruple(Str "1191",str0,str1,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "1192",str0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdSubSimpleExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdNamePrimary,Double(VhdSimpleName,
	str1))) -> Quadruple(Str "1193",str0,str1,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdSubSimpleExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Double(VhdIntPrimary,num0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,str1))) -> Quintuple(Str "1194",str0,str1,num0,match_combined lst0)
| Triple(Vhdwaveform_element,Triple(VhdXnorLogicalExpression,Double(VhdNamePrimary,
	Double(VhdSimpleName,str0)),Double(VhdNamePrimary,Double(VhdSimpleName,str1))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "1195",str0,str1)
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
| Triple(Vhdwaveform_element,Triple(VhdXorLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNamePrimary,Double(VhdSimpleName,str0))),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "1196",str0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdXorLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdNotFactor,Double(VhdNamePrimary,Double(VhdSimpleName,str0)))),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Triple(Str "1197",str0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdXorLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Double(VhdParenthesedPrimary,exp1)),Double(VhdNamePrimary,Double(VhdSimpleName,
	Str ""))) -> Triple(Str "1198",match_combined exp0,match_combined exp1)
| Triple(Vhdwaveform_element,Triple(VhdXorLogicalExpression,Double(VhdParenthesedPrimary,
	exp0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,str0),lst0)),Double(VhdNamePrimary,
	Double(VhdSimpleName,Str ""))) -> Quadruple(Str "1199",str0,match_combined lst0,match_combined exp0)
| Triple(Vhdwaveform_element,Triple(VhdXorLogicalExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,Str ""))) -> Quintuple(Str "301",
	str0,str1,match_combined lst0,match_combined lst1)
| Triple(Vhdwaveform_element,Triple(VhdXorLogicalExpression,Triple(VhdNameParametersPrimary,
	Double(VhdSimpleName,str0),lst0),Triple(VhdNameParametersPrimary,Double(VhdSimpleName,
	str1),lst1)),Double(VhdNamePrimary,Double(VhdSimpleName,str2))) -> Sextuple(Str "1200",
	str0,str1,str2,match_combined lst0,match_combined lst1)
| VhdChoiceOthers -> (Str "302")
| VhdSuffixAll -> (Str "1201")
| List [itm] -> match_combined itm
| List lst -> List (List.map match_combined lst)
| others -> others
