open Globals
open Idhash
open Vparser
open Printf
open Read_library

type nethash = (Vparser.token, (iopin * int * string) list * bool * Vparser.token * Vparser.token) Hashtbl.t

let edifname' id =
  let escaped = ref false and i = ref 0 and len = String.length id in
  let valid = Bytes.create len in
  while !i < len do (match id.[!i] with
    | 'a'..'z' -> Bytes.set valid (!i) id.[!i]
    | 'A'..'Z' -> Bytes.set valid (!i) id.[!i]
    | '0'..'9' when !i > 0 -> Bytes.set valid (!i) id.[!i]
    | '_' when !i > 0 -> Bytes.set valid (!i) '_'
    | _ -> Bytes.set valid (!i) (if !i > 0 then '_' else 'q'); escaped := true); incr i
  done;
  (!escaped,Bytes.to_string valid)

let edifname id = edifname' id.id

let edifout fd stats tree comment = match tree with QUINTUPLE(MODULE, ID top', EMPTY, TLIST iolst, THASH insts) ->
  let unhand n other = Dump.unhandled stderr n other; fprintf fd "%s\n" (Ord.getstr other) in
  let top = top'.id in
  let top_core = top^"_core" in
  let top_inst = top_core^"_inst" in
  let porthash = Hashtbl.create 256 in
  let [inputs;outputs] = List.map (fun req -> hfilter (function
    | QUINTUPLE(dir, EMPTY, EMPTY, rng, TLIST idlst) when dir=req -> true
    | _ -> false) (fst insts)) [INPUT;OUTPUT] in

  let gmt = Unix.gmtime (Unix.time()) in
  let netid hash net rng dir = function
    | Some ref ->
      if Hashtbl.mem hash net then
        let (lst,old_is_top,old_rng,old_dir) = Hashtbl.find hash net in
        Hashtbl.replace hash net (ref :: lst, old_is_top, old_rng, old_dir)
      else
        Hashtbl.replace hash net ([ref],false,rng,dir)
    | None ->
      Hashtbl.replace hash net ([],true,rng,dir)
  in
  let dumpio hash =
    fprintf fd "       (view netlist (viewType NETLIST)\n";
    fprintf fd "         (interface\n";
    List.iter (function
      | QUINTUPLE(dir, EMPTY, EMPTY, rng, TLIST idlst) -> let dirstr = String.uppercase_ascii (Ord.getstr dir) in List.iter (function
          | TRIPLE (ID itm, EMPTY, EMPTY) -> (match rng with
              | RANGE(INT hi, INT lo) ->
                for i = lo to hi do netid hash (TRIPLE (BITSEL, ID itm, INT i)) rng dir None done;
                fprintf fd "           (port (array (rename %s \"%s[%d:%d]\") %d) (direction %s))\n"
                  itm.id itm.id hi lo (hi-lo+1) dirstr
              | EMPTY ->
                netid hash (ID itm) rng dir None;
                fprintf fd "           (port %s (direction %s))\n" itm.id dirstr
              | other -> unhand 49 other)
          | other -> unhand 50 other) idlst
      | other -> unhand 51 other) (outputs@inputs);
    fprintf fd "         )\n";
    fprintf fd "         (contents\n" in
  let dumpinst (hash:nethash) instlst = List.iter (function
    | QUADRUPLE(MODINST, ID cellref, EMPTY, TLIST kindlst) ->
      let reflib = if Hashtbl.mem libhash cellref.id then " (libraryRef xilinx)" else "" in
      let thishash = if Hashtbl.mem porthash cellref.id
        then Hashtbl.find porthash cellref.id
        else (Printf.printf "Internal error: cellref %s ports not found\n" cellref.id; Hashtbl.create 256) in
      List.iter (function
        | TRIPLE(ID instid, SCALAR, TLIST pinlst) ->
          let (rename,valid) = edifname instid in
          if rename then
            fprintf fd "          (instance (rename %s \"%s\") (viewRef netlist (cellRef %s%s)))\n"
              valid instid.id cellref.id reflib
          else
            fprintf fd "          (instance %s (viewRef netlist (cellRef %s%s)))\n" instid.id cellref.id reflib;
          List.iter (function
            | TRIPLE(CELLPIN, ID pinref, BINNUM lev) ->
              netid hash (BINNUM lev) UNKNOWN UNKNOWN (Some({idpin=pinref;rngpin=EMPTY},-1,valid))
            | TRIPLE(CELLPIN, ID pinref, TRIPLE (BITSEL, ID netref, INT idx)) ->
              netid hash (TRIPLE (BITSEL, ID netref, INT idx)) UNKNOWN UNKNOWN (Some({idpin=pinref;rngpin=EMPTY},-1,valid))
            | TRIPLE(CELLPIN, ID pinref, QUADRUPLE (PARTSEL, ID netref, INT lft, INT rght)) ->
              netid hash (QUADRUPLE (PARTSEL, ID netref, INT lft, INT rght)) UNKNOWN UNKNOWN (Some({idpin=pinref;rngpin=EMPTY},-1,valid))
            | TRIPLE(CELLPIN, ID pinref, ID netref) -> (match Hashtbl.find thishash pinref with
                | RANGE(INT hi, INT lo) ->
                  for idx = hi downto lo do
                    netid hash (TRIPLE (BITSEL, ID netref, INT idx)) UNKNOWN UNKNOWN (Some({idpin=pinref;rngpin=EMPTY},hi-idx,valid))
                  done
                | EMPTY -> netid hash (ID netref) UNKNOWN UNKNOWN (Some({idpin=pinref;rngpin=EMPTY},-1,valid))
                | other -> unhand 74 other)
            | TRIPLE(CELLPIN, ID pinref, DOUBLE(CONCAT, TLIST lst)) ->
              Array.iteri (fun member x -> match x with
                | ID net ->
                  netid hash (ID net) UNKNOWN UNKNOWN (Some({idpin=pinref;rngpin=EMPTY},member,valid))
                | TRIPLE (BITSEL, ID bus, INT idx) ->
                  netid hash (TRIPLE (BITSEL, ID bus, INT idx)) UNKNOWN UNKNOWN (Some({idpin=pinref;rngpin=EMPTY},member,valid))
                | other -> unhand 73 other
              ) (Array.of_list lst)
            | DOUBLE(CELLPIN, ID pinref) -> () (*unconnected pins need not be mentioned *)
            | EMPTY -> () (* probably should not occur *)
            | other -> unhand 75 other) pinlst
        | other -> unhand 76 other) kindlst
    | other -> unhand 77 other) instlst in
  let dumpnets (nethash:nethash) = Hashtbl.iter (fun k (reflst,is_top,rng,dir) -> match k with
    | TRIPLE (BITSEL, ID netref, INT idx) ->
      let (rename,valid) = edifname' (sprintf "%s[%d]" netref.id idx) in
      if rename then
        fprintf fd "          (net (rename %s \"%s[%d]\") (joined\n" valid netref.id idx
      else
        fprintf fd "          (net %s (joined\n" netref.id;
      if is_top then (match rng with
        | RANGE(INT hi,INT lo) -> fprintf fd "           (portRef (member %s %d))\n" netref.id (hi-idx)
        | _ -> unhand 112 rng);
      List.iter (fun ((pinref:iopin),member,instid) ->
        if instid=top_inst then (match rng with
          | RANGE(INT hi,INT lo) -> fprintf fd "           (portRef (member %s %d) (instanceRef %s))\n" pinref.idpin.id (hi-idx) instid
          | _ -> unhand 116 rng)
        else if member <> -1 then
          fprintf fd "           (portRef (member %s %d) (instanceRef %s))\n" pinref.idpin.id member instid
        else
          fprintf fd "           (portRef %s (instanceRef %s))\n" pinref.idpin.id instid;
      ) reflst;
      fprintf fd "          ))\n";
    | QUADRUPLE (PARTSEL, ID netref, INT lft, INT rght) ->
      let (rename,valid) = edifname' (sprintf "%s[%d:%d]" netref.id lft rght) in
      if rename then
        fprintf fd "          (net (rename %s \"%s[%d:%d]\") (joined\n" valid netref.id lft rght
      else
        fprintf fd "          (net %s (joined\n" netref.id;
      if is_top then (match rng with
        | RANGE(INT hi,INT lo) -> fprintf fd "           (portRef (member %s %d))\n" netref.id (hi-lft)
        | _ -> unhand 131 rng);
      List.iter (fun ((pinref:iopin),member,instid) ->
        if instid=top_inst then (match rng with
          | RANGE(INT hi,INT lo) -> fprintf fd "           (portRef (member %s %d) (instanceRef %s))\n" pinref.idpin.id (hi-lft) instid
          | _ -> unhand 135 rng)
        else if member <> -1 then
          fprintf fd "           (portRef (member %s %d) (instanceRef %s))\n" pinref.idpin.id member instid
        else match rng with
        | RANGE(INT hi,INT lo) -> fprintf fd "           (portRef (member %s %d))\n" netref.id (hi-lft)
	| EMPTY -> fprintf fd "           (portRef %s (instanceRef %s))\n" pinref.idpin.id instid
	| UNKNOWN -> fprintf fd "           (portRef %s (instanceRef %s))\n" pinref.idpin.id instid
        | _ -> unhand 141 rng
      ) reflst;
      fprintf fd "          ))\n"
    | ID netref ->
      let (rename,valid) = edifname netref in
      if rename then
        fprintf fd "          (net (rename %s \"%s\") (joined\n" valid netref.id
      else
        fprintf fd "          (net %s (joined\n" netref.id;
      if (is_top) then
        fprintf fd "           (portRef %s)\n" netref.id;
      List.iter (fun ((pinref:iopin),member,instid) ->
        if member <> -1 then
          fprintf fd "           (portRef (member %s %d) (instanceRef %s))\n" pinref.idpin.id member instid
        else
          fprintf fd "           (portRef %s (instanceRef %s))\n" pinref.idpin.id instid;
      ) reflst;
      fprintf fd "          ))\n";
    | BINNUM "1'b0" -> 
      fprintf fd "          (net edifout_gnd (joined\n";
      List.iter (fun (pinref,member,instid) ->
        fprintf fd "           (portRef %s (instanceRef %s))\n" pinref.idpin.id instid;
      ) reflst;
      fprintf fd "          ))\n";
    | BINNUM "1'b1" -> 
      fprintf fd "          (net edifout_vcc (joined\n";
      List.iter (fun (pinref,member,instid) ->
        fprintf fd "           (portRef %s (instanceRef %s))\n" pinref.idpin.id instid;
      ) reflst;
      fprintf fd "          ))\n";
    | other -> unhand 102 other) nethash in
  let dumpskel id =
    fprintf fd "    (cell %s (cellType GENERIC)\n" id;
    fprintf fd "       (view netlist (viewType NETLIST)\n";
    fprintf fd "         (interface\n";
    if Hashtbl.mem modprims id then
      begin
        let thishash = Hashtbl.create 256 in
        Hashtbl.replace porthash id thishash;
        let submod = Hashtbl.find modprims id in match submod.tree with
          | QUINTUPLE(MODULE, ID top, EMPTY, TLIST iolst, THASH insts) ->
            Hashtbl.iter (fun k _ -> match k with
              | QUINTUPLE(dir, EMPTY, EMPTY, rng, TLIST idlst) ->
                let dirstr = String.uppercase_ascii (Ord.getstr dir) in
                List.iter (function
                  | TRIPLE (ID itm, EMPTY, EMPTY) -> (match rng with
                      | RANGE(expr1, expr2) -> let (hi,lo,dir) = Const.iwidth stderr submod.symbols rng in
                                               fprintf fd "           (port (array (rename %s \"%s[%d:%d]\") %d) (direction %s))\n"
                                                 itm.id itm.id hi lo (hi-lo+1) dirstr;
                                               Hashtbl.replace thishash itm (RANGE(INT hi,INT lo))
                      | EMPTY -> fprintf fd "           (port %s (direction %s))\n" itm.id dirstr;
                        Hashtbl.replace thishash itm EMPTY                      
                      | other -> unhand 163 other)
                  | other -> unhand 164 other) idlst
              | QUADRUPLE((REG|TRI0|WIRE), _, _, _) -> ()
              | SEXTUPLE(PARAMETER, _, _, _, _, _) -> ()
              | other -> unhand 166 other) (fst insts);
            fprintf fd "         )\n";
            fprintf fd "       )\n";
            fprintf fd "    )\n"
          | other -> unhand 170 other;
      end
    else if Hashtbl.mem libhash id then
      begin
        let thishash = Hashtbl.create 256 in
        Hashtbl.replace porthash id thishash;
        let prop = Hashtbl.find libhash id in
        List.iter (fun itm -> match itm.rngpin with
        | RANGE(INT hi, INT lo) ->
          fprintf fd "           (port (array (rename %s \"%s[%d:%d]\") %d) (direction INPUT))\n"
            itm.idpin.id itm.idpin.id hi lo (hi-lo+1);
          Hashtbl.replace thishash itm.idpin (RANGE(INT hi,INT lo))
        | EMPTY -> fprintf fd "           (port %s (direction INPUT))\n" itm.idpin.id;
            Hashtbl.replace thishash itm.idpin EMPTY
	| other -> unhand 212 other) prop.ipinlst;

        List.iter (fun itm -> match itm.rngpin with
        | RANGE(INT hi, INT lo) ->
          fprintf fd "           (port (array (rename %s \"%s[%d:%d]\") %d) (direction OUTPUT))\n"
            itm.idpin.id itm.idpin.id hi lo (hi-lo+1);
          Hashtbl.replace thishash itm.idpin (RANGE(INT hi,INT lo))
        | EMPTY -> fprintf fd "           (port %s (direction OUTPUT))\n" itm.idpin.id;
            Hashtbl.replace thishash itm.idpin EMPTY
	| other -> unhand 212 other) prop.opinlst;

        fprintf fd "         )\n";
        fprintf fd "       )\n";
        fprintf fd "    )\n"
      end in
  let dumpcell cell' =
    let cell = enterid cell' in
    Hashtbl.replace porthash cell' (Hashtbl.create 256);
    fprintf fd "    (cell %s (cellType GENERIC)\n" cell.id;
    let (nethash:nethash) = Hashtbl.create 256 in
    dumpio nethash;
    dumpinst nethash (hfilter (function QUADRUPLE(MODINST, _, _, _) -> true | _ -> false) (snd insts));
    List.iter (fun (a,b,c,d) -> if Hashtbl.mem nethash (BINNUM a) then
        begin
          fprintf fd "          (instance %s (viewRef netlist (cellRef %s (libraryRef xilinx))))\n" b c;
          netid nethash (BINNUM a) UNKNOWN UNKNOWN (Some({idpin=enterid d;rngpin=EMPTY},-1,b))
        end) [("1'b0","inst_gnd","GND","G");("1'b1","inst_vcc","VCC","P")];
    dumpnets nethash;
    fprintf fd "         )\n";
    fprintf fd "       )\n";
    fprintf fd "    )\n" in
  let dumpheader top =
    fprintf fd "(edif %s\n" top;
    fprintf fd "  (edifVersion 2 0 0)\n";
    fprintf fd "  (edifLevel 0)\n";
    fprintf fd "  (keywordMap (keywordLevel 0))\n";
    fprintf fd "  (status\n";
    fprintf fd "    (written\n";
    fprintf fd "      (timeStamp %04d %02d %02d %02d %02d %02d)\n"
      (1900 + gmt.Unix.tm_year)
      (1 + gmt.Unix.tm_mon)
      gmt.Unix.tm_mday
      gmt.Unix.tm_hour
      gmt.Unix.tm_min
      gmt.Unix.tm_sec;
    fprintf fd "      (author \"Kimmitt, Jonathan\")\n";
    fprintf fd "      (program \"edifout\" (version \"%s\"))\n" comment;
    fprintf fd "     )\n";
    fprintf fd "   )\n" in
  dumpheader top;
  fprintf fd "  (library (rename xilinx \"Xilinx\")\n";
  fprintf fd "    (edifLevel 0)\n";
  fprintf fd "    (technology (numberDefinition ))\n";
  let liblst = ref [] in
  Hashtbl.iter (fun k _ -> match k with ID id -> liblst := id.id :: !liblst | _ -> failwith (Ord.getstr k)) stats;
  List.iter (fun itm' ->
    let _ = enterid itm' in if not (List.mem itm' !liblst) then liblst := itm' :: !liblst) ["IBUF";"OBUF";"VCC";"GND"];
  List.iter dumpskel !liblst;
  fprintf fd "  )\n";
  fprintf fd "  (library work\n";
  fprintf fd "    (edifLevel 0)\n";
  fprintf fd "    (technology (numberDefinition ))\n";
  dumpcell top_core;
  fprintf fd "    (cell %s (cellType GENERIC)\n" top;
  let (nethash:nethash) = Hashtbl.create 256 in
  dumpio nethash;
  let prefix = "qq_" in
  let padinst = ref [] in List.iter (function
    | QUINTUPLE(dir, EMPTY, EMPTY, rng, TLIST idlst) ->
      let ostr = enterid ((function INPUT -> "O" | OUTPUT -> "I" | _ -> "") dir) in
      let istr = enterid ((function INPUT -> "I" | OUTPUT -> "O" | _ -> "") dir) in
      let dirstr = enterid ((function INPUT -> "IBUF" | OUTPUT -> "OBUF" | _ -> "") dir) in
      List.iter (function
        | TRIPLE (ID itm, EMPTY, EMPTY) ->
          let prenet = enterid (snd (edifname' (prefix^itm.id))) in
          let postnet = enterid (snd (edifname (itm))) in
          (match rng with
            | RANGE(INT hi, INT lo) ->
              for i = lo to hi do
                let (_,netidv) = edifname' (sprintf "%s[%d]" itm.id i) in
                let padidv = enterid(Ord.getstr dir^"_"^netidv) in
                netid nethash (TRIPLE(BITSEL, ID prenet, INT i)) rng UNKNOWN (Some({idpin=itm;rngpin=RANGE(INT i, INT i)},-1,top_inst));
                netid nethash (TRIPLE(BITSEL, ID prenet, INT i)) rng UNKNOWN (Some({idpin=ostr;rngpin=RANGE(INT i, INT i)},-1,padidv.id));
                netid nethash (TRIPLE(BITSEL, ID postnet, INT i)) rng UNKNOWN (Some({idpin=istr;rngpin=RANGE(INT i, INT i)},-1,padidv.id));
                padinst := QUADRUPLE(MODINST, ID dirstr, EMPTY, TLIST [TRIPLE(ID padidv, SCALAR, TLIST [])]) :: !padinst
              done;
            | EMPTY ->
              let (_,netidv) = edifname itm in
              let padidv = enterid(Ord.getstr dir^"_"^netidv) in
              netid nethash (ID prenet) UNKNOWN UNKNOWN (Some({idpin=itm;rngpin=EMPTY},-1,top_inst));
              netid nethash (ID prenet) UNKNOWN UNKNOWN (Some({idpin=ostr;rngpin=EMPTY},-1,padidv.id));
              netid nethash (ID postnet) UNKNOWN UNKNOWN (Some({idpin=istr;rngpin=EMPTY},-1,padidv.id));
              padinst := QUADRUPLE(MODINST, ID dirstr, EMPTY, TLIST [TRIPLE(ID padidv, SCALAR, TLIST [])]) :: !padinst
            | other -> unhand 267 other)
        | other -> unhand 268 other) idlst
    | other -> unhand 269 other) (outputs@inputs);
  let maininst = QUADRUPLE(MODINST, ID (enterid top_core), EMPTY, TLIST [TRIPLE(ID (enterid (top_core^"_inst")), SCALAR, TLIST [])]) in
  dumpinst nethash (maininst :: !padinst);
  dumpnets nethash;
  fprintf fd "         )\n";
  fprintf fd "      )\n";
  fprintf fd "    )\n";
  fprintf fd "  )\n";
  fprintf fd "  (design %s (cellRef %s (libraryRef work)))\n" top top;
  fprintf fd ")\n";
  fprintf fd "\n"
  | _ -> failwith "edifout only handles modules"

let write_edif_arch arch top' =
  let _ = enterid top' in
  List.iter (fun found ->
    let fd = unique_open (top'^"_"^arch^".edn") in
    edifout fd (Count.count_flat_netlist' found) found.tree found.comment;
    close_out fd) (Count.find_arch arch top')


let tree = ref Edif2.EMPTY
let test = ref Vparser.EMPTY
let insthash = Hashtbl.create 256
let nethash = Hashtbl.create 256

let unhand_edif = ref []

let enterid' arg = ID (enterid arg)

let array_hook arg1 arg2 = Printf.printf "Array hook %s %s\n" arg1 arg2; DOUBLE(enterid' arg1, enterid' arg2)
let cell_hook arg1 arg2 = Printf.printf "Cell hook %s\n" arg1; enterid' arg1
let cellref2_hook arg1 arg2 = Printf.printf "Cellref2 hook %s\n" arg1; enterid' arg1
let rename_hook arg1 arg2 = Printf.printf "Rename hook %s %s\n" arg1 arg2; DOUBLE(enterid' arg1, enterid' arg2)
let instance2_hook arg1 arg2 arg3 arg4 = Printf.printf "Instance hook %s %s %s\n" arg1 arg2 arg3;
  Hashtbl.replace insthash arg1 (arg1, arg2, []); EMPTY
let netref_hook arg1 arg2 arg3 = Hashtbl.replace nethash arg1 arg2; List.iter (function
  | DOUBLE(ID id, ID ref) -> ()
  | TRIPLE(PARTSEL, ID mref, INT idx) -> ()
  | QUADRUPLE(PARTSEL, ID mref, INT idx, ID instref) -> if Hashtbl.mem insthash instref.id then
      begin
        let (inst,kind,connlst) = Hashtbl.find insthash instref.id in
        Printf.printf "net ref %s[%d] %s %s\n" mref.id idx instref.id arg1;
        Hashtbl.replace insthash instref.id (inst,kind,(sprintf"%s[%d]" mref.id idx, arg1) :: connlst)
      end
    else
      failwith (sprintf "Instance %s is not found\n" instref.id)
  | oth -> unhand_edif := oth :: !unhand_edif) arg3; EMPTY
let portref_hook id ref = DOUBLE(enterid' id, enterid' ref)
let portref2_hook mref idx = TRIPLE(PARTSEL, enterid' mref, INT idx)
let portref3_hook mref idx instref = QUADRUPLE(PARTSEL, enterid' mref, INT idx, enterid' instref)

let create_edif file =
  begin
    let tree = Eparse.eparse file in
    Eparse.array_handler := array_hook;
    Eparse.cell_handler := cell_hook;
    Eparse.cellref2_handler := cellref2_hook;
    Eparse.rename_handler := rename_hook;
    Eparse.instance_handler2 := instance2_hook;
    Eparse.netref_handler := netref_hook;
    Eparse.portref_handler := portref_hook;
    Eparse.portref2_handler := portref2_hook;
    Eparse.portref3_handler := portref3_hook;
    Eparse.test_match Edif2.EMPTY tree
  end

let read_edif file check lathash = create_edif file 
