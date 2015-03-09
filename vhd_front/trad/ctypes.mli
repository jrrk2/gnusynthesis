type ctypes =
  | CAPPLIST of ctypes * ctypes
  | CConcrete
  | CDefault
  | CDown
  | CFIELDVAR
  | CFresh
  | CIDENT of string
  | CIDENTX of (ctypes*ctypes)
  | CIDLIST of string list
  | CID of string
  | CImmutable
  | CLABEL of ctypes * ctypes
  | CLABELXEXP of (ctypes*ctypes)
  | CLASSSIGNATURE of (ctypes*ctypes list)
  | CMutable
  | CNONE
  | CNonrec
  | CONSTRAINT of (ctypes*ctypes)
  | COverride
  | COVERRIDE of (ctypes*ctypes)
  | CPATALIAS of (ctypes*ctypes)
  | CPATANY
  | CPATARRAY of ctypes list
  | CPATCONS of ctypes list
  | CPATCONSTRAIN of ctypes
  | CPATLAZ of ctypes
  | CPATOR of ctypes list
  | CPATRECORD of ctypes list
  | CPATTUPLE of ctypes list
  | CPATTYP of ctypes
  | CPATVAR of ctypes
  | CPATXEXPCASE of (ctypes*ctypes)
  | CPATXEXP of (ctypes*ctypes)
  | CPrivate
  | CPublic
  | CRec
  | CRINHERIT of ctypes
  | CRTAG of (ctypes*ctypes*ctypes list)
  | CSOME of ctypes
  | CSTRING
  | CSTRINGTYPLOC of (ctypes*ctypes list)
  | CSTRINGXMODTYPE of (ctypes*ctypes)
  | CSTRINGXMODTYPXMOD of (ctypes*ctypes*ctypes)
  | CSTRINGXMUT of (ctypes*ctypes*ctypes)
  | CSTRINGXTYPDECL of (ctypes*ctypes)
  | CTOPDEF of ctypes list
  | CTOPDIR of string * ctypes list
  | CTYPALIAS of ctypes
  | CTYPANY
  | CTYPARROW of ctypes*ctypes
  | CTYPCLASS of ctypes list
  | CTYPCONSTR of ctypes * ctypes list
  | CTYPOBJ of ctypes list
  | CTYP of ctypes list
  | CTYPPACK of ctypes list
  | CTYPPOLY of string list * ctypes
  | CTYPTUPLE of ctypes list
  | CTYPVARIANT of ctypes list
  | CTYPVAR of ctypes
  | CUp
  | CVAL of ctypes * ctypes
  | CVALUEDESC of (ctypes * ctypes list)
  | CVirtual
  | PCTYCONSTR of (ctypes*ctypes list)
  | PCTYFUN of (ctypes*ctypes*ctypes)
  | PCTYSIGNATURE of (ctypes)
  | PEXPAPP of ctypes*ctypes list
  | PEXPARRAY of (ctypes list)
  | PEXPASSERTFALSE
  | PEXPASSERT of (ctypes)
  | PEXPCONSTANT of ctypes
  | PEXPCONSTRAINT of (ctypes* ctypes* ctypes)
  | PEXPCONSTR of ctypes*ctypes
  | PEXPFIELD of (ctypes* ctypes)
  | PEXPFOR of (ctypes* ctypes* ctypes* ctypes* ctypes)
  | PEXPFUN of string*ctypes*ctypes list
  | PEXPIDENT of ctypes
  | PEXPIFTHENELSE  of (ctypes* ctypes* ctypes)
  | PEXPLAZY of (ctypes)
  | PEXPLETMODULE of (ctypes* ctypes* ctypes)
  | PEXPLET of ctypes list*ctypes
  | PEXPMATCH of ctypes*ctypes list
  | PEXPNEW of (ctypes)
  | PEXPNEWTYPE of (ctypes* ctypes)
  | PEXPOBJECT of (ctypes)
  | PEXPOPEN of (ctypes* ctypes)
  | PEXPOVERRIDE of (ctypes list)
  | PEXPPACK of (ctypes* ctypes list* ctypes)
  | PEXPPOLY of (ctypes* ctypes)
  | PEXPRECORD of (ctypes list* ctypes)
  | PEXPSEND of (ctypes* ctypes)
  | PEXPSEQ of (ctypes* ctypes)
  | PEXPSETFIELD of (ctypes* ctypes* ctypes)
  | PEXPSETINSTVAR of (ctypes* ctypes)
  | PEXPTRY of ctypes*ctypes list
  | PEXPTUPLE of ctypes list
  | PEXPVARIANT of (ctypes* ctypes)
  | PEXPWHEN of (ctypes* ctypes)
  | PEXPWHILE of (ctypes* ctypes)
  | PSTRCLASS of (ctypes list)
  | PSTRCLASSTYPE of (ctypes list)
  | PSTREVAL of (ctypes)
  | PSTREXCEPTION of (ctypes*ctypes list)
  | PSTREXNREBIND of (ctypes*ctypes)
  | PSTRINCLUDE of (ctypes)
  | PSTRMODTYPE of (ctypes*ctypes)
  | PSTRMODULE of (ctypes*ctypes)
  | PSTROPEN of (ctypes)
  | PSTRPRIMITIVE of (ctypes*ctypes)
  | PSTRRECMODULE of (ctypes list)
  | PSTRTYPE of (ctypes list)
  | PSTRVALUE of (ctypes*ctypes list)
  | PTYPEABSTRACT
  | PTYPEDECL of (ctypes list* ctypes list* ctypes * ctypes *ctypes)
  | PTYPERECORD of (ctypes list)
  | PTYPEVARIANT of (ctypes list)
  | VBOOL of bool
  | VCHAR of int
  | VFLOAT of float
  | VINT32 of int32
  | VINT64 of int64
  | VINT of int
  | VNATIVEINT of nativeint
  | VSTRING of string
  | VUNIT
