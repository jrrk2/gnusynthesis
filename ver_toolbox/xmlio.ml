open Bytes
open Globals
open Vparser
open Xmlm

type xmltree = Element of string * (string*string) list * xmltree list | PCData of string;;
type libre_xml =
  | Lunknown
type extract =
  | Attr of int * libre_xml * (int * libre_xml * string) list * extract list
  | Data of string

let in_tree f = 
  let fd = open_in f in
  let i = make_input (`Channel fd) in
    let el ((tag1,tag2),lst) childs = Element (tag2, List.map (fun ((tag1,tag2),tag3) -> assert(tag1==Xmlm.ns_xml); (tag2, tag3)) lst, childs)  in
    let data d = PCData d in
    let doc = Xmlm.input_doc_tree ~el ~data i in
    close_in fd;
    doc

let frag = function
  | Element (tag, lst, childs) -> `El (((Xmlm.ns_xml, tag), List.map (fun (tag1,tag3) -> ((Xmlm.ns_xml, tag1), tag3)) lst), childs)
  | PCData d -> `Data d

let out_tree f t =
  let fd = open_out f in
  let fn arg = if arg = 10 then output_byte fd 13; output_byte fd arg in
  let o = make_output ~nl:true (`Fun fn) in
  Xmlm.output_doc_tree frag o t;
  close_out fd

let escaped s =
  let is_printable c  = c > ' ' && c <= '~' in
  let char_code = int_of_char in
  let char_chr = char_of_int in
  let n = ref 0 in
    for i = 0 to length s - 1 do
      n := !n +
        (match unsafe_get s i with
         | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
         | c -> if is_printable c then 1 else 4)
    done;
    if !n = length s then s else begin
      let s' = create !n in
        n := 0;
        for i = 0 to length s - 1 do
          begin
            match unsafe_get s i with
            | ('"' | '\\') as c ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
            | '\n' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
            | '\t' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 't'
            | '\r' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'r'
            | '\b' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'b'
            | c ->
                if is_printable c then
                  unsafe_set s' !n c
                else begin
                  let a = char_code c in
                  unsafe_set s' !n '\\';
                  incr n;
                  unsafe_set s' !n (char_chr (48 + a / 100));
                  incr n;
                  unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
                  incr n;
                  unsafe_set s' !n (char_chr (48 + a mod 10))
                end
          end;
          incr n
        done;
        s'
      end

let escaped s = String.of_bytes (escaped (Bytes.of_string s))

let xint n = Element("Int", [("_",string_of_int n)], [PCData "\n  "])
let xid {Idhash.id} = Element("Id", [("_",id)], [PCData "\n  "])
let rec xtok = function
| TLIST lst -> Element("Tlist", [], List.map xtok lst)
| THASH thash ->
   let lst1 = ref [] and lst2 = ref [] in
   Hashtbl.iter (fun x () -> lst1 := xtok x :: !lst1) (fst thash);
   Hashtbl.iter (fun x () -> lst2 := xtok x :: !lst2) (snd thash);
   Element("Thash", [], [Element("Tlist", [], !lst1); Element("Tlist", [], !lst2)])
| DOUBLE(tok,arg) -> Element("Double", [], [xtok tok; xtok arg])
| TRIPLE(tok, arg1, arg2) -> Element("Triple", [], [xtok tok; xtok arg1; xtok arg2])
| QUADRUPLE(tok, arg1, arg2, arg3) -> Element("Quadruple", [], [xtok tok; xtok arg1; xtok arg2; xtok arg3])
| QUINTUPLE(tok, arg1, arg2, arg3, arg4) -> Element("Quintuple", [], [xtok tok; xtok arg1; xtok arg2; xtok arg3; xtok arg4])
| SEXTUPLE(tok, arg1, arg2, arg3, arg4, arg5) -> Element("Sextuple", [], [xtok tok; xtok arg1; xtok arg2; xtok arg3; xtok arg4; xtok arg5])
| SEPTUPLE(tok, arg1, arg2, arg3, arg4, arg5, arg6) -> Element("Septuple", [], [xtok tok; xtok arg1; xtok arg2; xtok arg3; xtok arg4; xtok arg5; xtok arg6])
| OCTUPLE(tok, arg1, arg2, arg3, arg4, arg5, arg6, arg7) -> Element("Octuple", [], [xtok tok; xtok arg1; xtok arg2; xtok arg3; xtok arg4; xtok arg5; xtok arg6; xtok arg7])
| RANGE(arg1,arg2) -> Element("Range", [], [xtok arg1; xtok arg2])
| ASCNUM c -> Element("Ascnum", [("_",escaped c)], [PCData "\n  "])
| BINNUM c -> Element("Binnum", [("_",c)], [PCData "\n  "])
| BUFIF lev -> Element("Bufif", [("_",lev)], [PCData "\n  "])
| NOTIF lev -> Element("Notif", [("_",lev)], [PCData "\n  "])
| DECNUM c -> Element("Decnum", [("_",c)], [PCData "\n  "])
| FLOATNUM flt -> Element("Floatnum", [("_",(string_of_float flt))], [PCData "\n  "])
| HEXNUM c -> Element("Hexnum", [("_",c)], [PCData "\n  "])
| ID {Idhash.id} -> Element("Id", [("_",id)], [PCData "\n  "])
| IDSTR str -> Element("Idstr", [("_",str)], [PCData "\n  "])
| ILLEGAL c -> Element("Illegal", [("_",String.make 1 c)], [PCData "\n  "])
| INTNUM c -> Element("Intnum", [("_",c)], [PCData "\n  "])
| PREPROC str -> Element("Preproc", [("_",str)], [PCData "\n  "])
| WEAK strength -> Element("Weak", [("_",strength)], [PCData "\n  "])
| STRONG strength -> Element("Strong", [("_",strength)], [PCData "\n  "])
| WIDTHNUM(radix,sz,num) -> Element("Widthnum", [("radix", string_of_int radix); ("size", string_of_int sz); ("num", string_of_int num)], [PCData "\n  "])
| DOTTED lst -> Element("Dotted", [], List.map xtok lst)
| INT n -> xint n
| oth when Hashtbl.mem Shortlist.revsymbols oth -> Element("Ord", [("_", (Hashtbl.find Shortlist.revsymbols oth))], [PCData "\n  "])
| oth -> failwith ("xtok: "^Ord.getstr oth)

let rec xprop = function
  | Pvar (tok) -> Element ("Pvar", [], [xtok tok])
  | Pnot arg -> Element ("Pnot", [], [xprop arg])
  | Pand (arg1, arg2) -> Element ("Pand", [], [xprop arg1; xprop arg2])
  | Por (arg1, arg2) -> Element ("Por", [], [xprop arg1; xprop arg2])
  | Pimp (arg1, arg2) -> Element ("Pimp", [], [xprop arg1; xprop arg2])
  | Piff (arg1, arg2) -> Element ("Piff", [], [xprop arg1; xprop arg2])
  | Prime arg -> Element ("Prime", [], [xprop arg])
  | Ptrue -> PCData "Ptrue"
  | Pfalse -> PCData "Pfalse"
  | Punknown -> PCData "Punknown"

let xparam {pid;prng;dflt} = Element ("Xparam", [], [xid pid; xtok prng; xtok dflt])
				      
let xtoklst lst = Element ("Xtoklst", [], List.map xtok lst)

let xio {idpin = {Idhash.id}; rngpin} = Element ("Xio", [], PCData id :: [xtok rngpin])
   
let xstrlst lst = Element ("Xstrlst", List.map (fun arg -> ("_", String.escaped arg)) lst, [PCData "\n  "])

let xiolst lst = Element ("Xiolst", [], List.map xio lst)

let xparamlst lst = Element ("Xparamlst", [], List.map xparam lst)

let xtset set =
  let lst = ref [] in
  TokSet.iter (fun tok -> lst := xtok tok :: !lst) set;
  Element("Xtset", [], !lst)

let xsigattr = function
  | Sigundef -> PCData "Sigundef"
  | Sigarray arr -> let lst = Array.to_list arr in Element ("Sigarray", [], List.map xtset lst)
  | Sigparam token -> Element ("Sigparam", [], [xtok token])
  | Sigtask token -> Element ("Sigtask", [], [xtok token])
  | Sigfunc token -> Element ("Sigfunc", [], [xtok token])
  | Signamed token -> Element ("Signamed", [], [xtok token])
    
let rec xsyms = function
  | EndShash -> PCData "Endshash"
  | Shash {nxt; syms; stabarch; stabnam} -> Element ("Shash", [("stabarch", stabarch); ("stabnam", stabnam)], [xsyms nxt; xentries syms])

and xentries hash =
  let lst = ref [] in
  Hashtbl.iter (fun {Idhash.id=id} {symattr; width; path = {Idhash.id=pth}; sigattr; localsyms} ->
    lst := Element ("Sentry", [("entry", id); ("path", pth)], [xtset symattr; xtok width; xsigattr sigattr; xsyms localsyms]) :: !lst) hash;
  Element("Xentries", [], !lst)
	
let lib_trans (itm,{nlen; nam = {Idhash.id=nam}; decl; len; seq; func; prop; iolst; ipinlst; opinlst; reglst; wirlst; bnam; clk; clr; dat; en; qout; instcnt; tlst; instid; paramlst}) =
  Element ("Lib_trans", [("itm",itm); ("nam", nam)], [xint nlen;
		      xtok decl;
		      xint len;
		      xtok seq;
		      xtok func;
		      xprop prop;
		      xtoklst iolst;
		      xiolst ipinlst;
		      xiolst opinlst;
		      xiolst reglst;
		      xiolst wirlst;
		      xid bnam;
		      xid clk;
		      xid clr;
		      xid dat;
		      xid en;
		      xid qout;
		      xint instcnt;
		      xtoklst tlst;
		      xid instid;
		      xparamlst paramlst])

let unint = function
  | Element ("Int", [("_", n)], _) -> int_of_string n
  | oth -> failwith "Invalid integer"

let rec untok = function
  | Element ("Int", [("_", n)], _) -> INT (int_of_string n)
  | Element ("Ascnum", [("_",c)], _) -> ASCNUM (Scanf.unescaped c)
  | Element ("Binnum", [("_",c)], _) -> BINNUM c
  | Element ("Decnum", [("_",c)], _) -> DECNUM c
  | Element ("Hexnum", [("_",c)], _) -> HEXNUM c
  | Element ("Intnum", [("_",c)], _) -> INTNUM c
  | Element ("Floatnum", [("_",flt)], _) -> FLOATNUM (float_of_string flt)
  | Element ("Id", [("_", id)], _) -> ID (enterid id)
  | Element ("Idstr", [("_", str)], _) -> IDSTR str
  | Element ("Weak", [("_",strength)], _) -> WEAK strength
  | Element ("Strong", [("_",strength)], _) -> STRONG strength
  | Element ("Bufif", [("_",lev)], _) -> BUFIF lev
  | Element ("Notif", [("_",lev)], _) -> NOTIF lev
  | Element ("Ord", [("_", keyword)], _) ->
      if Hashtbl.mem Vlexer.ksymbols keyword then Hashtbl.find Vlexer.ksymbols keyword else
      if Hashtbl.mem Shortlist.intsymbols keyword then Hashtbl.find Shortlist.intsymbols keyword else 
      failwith ("Invalid ord: "^keyword)
  | Element ("Tlist", [], lst) -> TLIST (List.map untok lst)
  | Element ("Dotted", [], lst) -> DOTTED (List.map untok lst)
  | Element ("Thash", [], [Element ("Tlist", [], lst1); Element ("Tlist", [], lst2)]) ->
      let thash1 = Hashtbl.create 256 in
      let thash2 = Hashtbl.create 256 in
      List.iter (fun k -> Hashtbl.add thash1 k ()) (List.map untok lst1);
      List.iter (fun k -> Hashtbl.add thash2 k ()) (List.map untok lst2);
      THASH(thash1,thash2)
  | Element ("Octuple", [], [tok; arg1; arg2; arg3; arg4; arg5; arg6; arg7]) ->
      OCTUPLE(untok tok, untok arg1, untok arg2, untok arg3, untok arg4, untok arg5, untok arg6, untok arg7)
  | Element ("Septuple", [], [tok; arg1; arg2; arg3; arg4; arg5; arg6]) ->
      SEPTUPLE(untok tok, untok arg1, untok arg2, untok arg3, untok arg4, untok arg5, untok arg6)
  | Element ("Sextuple", [], [tok; arg1; arg2; arg3; arg4; arg5]) ->
      SEXTUPLE(untok tok, untok arg1, untok arg2, untok arg3, untok arg4, untok arg5)
  | Element ("Quintuple", [], [tok; arg1; arg2; arg3; arg4]) ->
      QUINTUPLE(untok tok, untok arg1, untok arg2, untok arg3, untok arg4)
  | Element ("Quadruple", [], [tok; arg1; arg2; arg3]) ->
      QUADRUPLE(untok tok, untok arg1, untok arg2, untok arg3)
  | Element ("Triple", [], [tok; arg1; arg2]) -> TRIPLE(untok tok, untok arg1, untok arg2)
  | Element ("Double", [], [tok; arg1]) -> DOUBLE(untok tok, untok arg1)
  | Element ("Range", [], [arg1; arg2]) -> RANGE(untok arg1, untok arg2)
  | Element (oth, lst1, lst2) -> failwith ("Invalid token tree: "^oth)
  | PCData oth -> failwith "Invalid tok"

let rec unid = function
  | Element("Id", [("_",id)], _) -> enterid id
  | oth -> failwith "Invalid ID"

let rec unprop = function
  | PCData "Pfalse" -> Pfalse
  | PCData "Ptrue" -> Ptrue
  | PCData "Punknown" -> Punknown
  | Element ("Pvar", [], [tok]) -> Pvar (untok tok)
  | Element ("Pnot", [], [arg]) -> Pnot (unprop arg)
  | Element ("Pand", [], [arg1; arg2]) -> Pand (unprop arg1, unprop arg2)
  | Element ("Por", [], [arg1; arg2]) -> Por (unprop arg1, unprop arg2)
  | Element ("Pimp", [], [arg1; arg2]) -> Pimp (unprop arg1, unprop arg2)
  | Element ("Piff", [], [arg1; arg2]) -> Piff (unprop arg1, unprop arg2)
  | Element ("Prime", [], [arg]) -> Prime (unprop arg)
  | oth -> failwith "Invalid prop"

let untoklst = function
  | Element ("Xtoklst", [], lst) -> List.map untok lst
  | oth -> failwith "Invalid toklst"

let uniolst = function
  | Element ("Xiolst", [], lst) -> List.map (function
      | Element ("Xio", [], PCData id :: [tok]) -> {idpin = enterid id; rngpin = untok tok}
      | oth -> failwith "Invalid iolst") lst
  | Element (oth, lst1, lst2) -> failwith ("Invalid io lst: "^oth)
  | PCData oth -> failwith "Invalid iolst"
	
let unparamlst = function
  | Element ("Xparamlst", [], lst) -> List.map (function
      | Element ("Xparam", [], [pid; prng; dflt]) -> {pid=unid pid; prng=untok prng; dflt=untok dflt}
      | oth -> failwith "Invalid paramlst") lst
  | oth -> failwith "Invalid paramlst"
	
let lib_untrans = function
  | Element ("Lib_trans", [("itm",itm); ("nam", nam)], [ nlen;
		       decl;
		       len;
		       seq;
		       func;
		       prop;
		       iolst;
		       ipinlst;
		       opinlst;
		       reglst;
		       wirlst;
		       bnam;
		       clk;
		       clr;
		       dat;
		       en;
		       qout;
		       instcnt;
		       tlst;
		       instid;
		        paramlst]) ->
			  (itm,{nlen=unint nlen;
				 nam=enterid nam;
				 decl=untok decl;
				 len=unint len;
				 seq=untok seq;
				 func=untok func;
				 prop=unprop prop;
				 iolst=untoklst iolst;
				 ipinlst=uniolst ipinlst;
				 opinlst=uniolst opinlst;
				 reglst=uniolst reglst;
				 wirlst=uniolst wirlst;
				 bnam=unid bnam;
				 clk=unid clk;
				 clr=unid clr;
				 dat=unid dat;
				 en=unid en;
				 qout=unid qout;
				 instcnt=unint instcnt;
				 tlst=untoklst tlst;
				 instid=unid instid;
				 paramlst=unparamlst paramlst})
  | oth -> failwith "Invalid library"
;;
let mod_trans (itm, {tree; symbols; unresolved; is_netlist; is_behav; is_seq; is_hier; is_top; arch; comment; datestamp}) =
  Element("Mod_trans", [("itm",itm);
			 ("is_netlist", string_of_bool is_netlist);
			 ("is_behav", string_of_bool is_behav);
			 ("is_seq", string_of_bool is_seq);
			 ("is_hier", string_of_bool is_hier);
			 ("is_top", string_of_bool is_top);
			 ("arch", arch);
			 ("comment", comment);
			 ("datestamp", string_of_float datestamp)], [xtok tree; xsyms symbols; xstrlst unresolved])
    
