open Globals
open Vparser
open Idhash
open Dump
open Printf
open Read_library

let verbose = ref false and aigerclkenv = ref UNKNOWN (*and compare_mode = ref false*)

type inv_t = Inv | Noninv

type aiger_t =
    | AigUnknown of int
    | AigInput of string
    | AigLatchIn of int
    | AigLatchOut of string
    | AigOutput of string
    | AigAndIn of int
    | AigAndOut of int

let aigstr = function
    | AigUnknown num -> "AigUnknown "^string_of_int num
    | AigInput str -> "AigInput "^str
    | AigLatchIn num -> "AigLatchIn "^string_of_int num
    | AigLatchOut str -> "AigLatchOut "^str
    | AigOutput str -> "AigOutput "^str
    | AigAndIn num -> "AigAndIn "^string_of_int num
    | AigAndOut num -> "AigAndOut "^string_of_int num

let net_type logfile nets arg typ =
  if nets.(arg/2) = AigUnknown (arg/2) || (match nets.(arg/2) with
  | AigInput _ -> (match typ with AigInput _ -> true | _ -> false)
  | AigLatchOut _ -> (match typ with AigLatchOut _ -> true | _ -> false)
  | _ -> false)
  then
    begin
      if !verbose then fprintf logfile "Net %d has type %s\n" (arg/2) (aigstr typ);
      nets.(arg/2) <- typ
    end
  else
    failwith (sprintf "Net %d already has type %s when set to type %s" (arg/2) (aigstr nets.(arg/2)) (aigstr typ))

let rec aigerGetName inv pName =
  let notstr = function
    | Noninv -> ""
    | Inv -> "inv$" in
  let pID = ID (enterid ((notstr inv) ^ pName)) in
  let length = String.length(pName) in
    (* consider the case of a signal having name "0" or "1" *)
  if ( not(length == 1 && (pName.[0] == '0' || pName.[0] == '1')) ) then
    begin
      let i = ref 0 in
      let buffer = ref EMPTY in
      while !i < length && !buffer = EMPTY do
        if ( not ((pName.[!i] >= 'a' && pName.[!i] <= 'z') || 
                     (pName.[!i] >= 'A' && pName.[!i] <= 'Z') || pName.[!i] == '$' ||
                     (pName.[!i] >= '0' && pName.[!i] <= '9') || pName.[!i] == '_') ) then begin
	  try Scanf.sscanf (String.sub pName !i (String.length pName - !i)) "[%d]%n"
                (fun idx pos -> if not ((pos > 0) && (pos + !i == length)) then buffer := pID
                  else
                    buffer := TRIPLE(BITSEL,ID (enterid (notstr inv ^ String.sub pName 0 !i)), INT idx));
          with Scanf.Scan_failure _ -> buffer := pID end;
        incr i
      done;
      if !i == length then
        pID
      else
        !buffer
    end
  else
    pID

let rec unlit inf =
  let ich = int_of_char (input_char inf) in
  ich land 127 + ( if ich >= 128 then 128*unlit inf else 0)

let read_aiger_file logfile fil =
  let inf = Vparse.my_openin fil in
  print_newline ();
  Scanf.sscanf (input_line inf) "a%cg %d %d %d %d %d"
    begin
      fun f m i l o a ->
        printf "    M = maximum variable index %d\n" m;
        printf "    I = number of inputs %d\n" i;
        printf "    L = number of latches %d\n" l;
        printf "    O = number of outputs %d\n" o;
        printf "    A = number of AND gates %d\n" a;
	flush stdout;
	let model = ref (match aigerGetName Noninv fil with ID id -> id.id | _ -> failwith "file name") in
        let nets = Array.init (m+1) (fun ix -> AigUnknown ix) in
        let inputs = Array.init i (fun ix -> (0,sprintf "i%d" ix)) in
        let latches = Array.init l (fun ix -> (0,0,sprintf "l%d" ix)) in
        let outputs = Array.init o (fun ix -> (0,sprintf "o%d" ix)) in
        let ands = Array.create a (0,0,0) in
        for k = 0 to i-1 do
	  let arg1 = match f with
	    | 'a' -> int_of_string (input_line inf)
	    | 'i' -> (k+1)*2
	    | _ -> failwith "Invalid aiger format" in
	  let str = sprintf "n%d" k in
          inputs.(k) <- (arg1,str);
	  if !verbose then Printf.fprintf logfile "%d\n" arg1;
          done;
        for k = 0 to l-1 do
	  let getlat arg1 arg2 =
	    let str = sprintf "l%d" k in
            latches.(k) <- (arg1,arg2,str);
	    if !verbose then Printf.fprintf logfile "%d %d\n" arg1 arg2 in
	  match f with
	    | 'a' -> Scanf.sscanf (input_line inf) "%d %d" getlat
	    | 'i' -> getlat ((i+k+1)*2) (* unlit inf *) (int_of_string (input_line inf))
	    | _ -> failwith "Invalid aiger format"
        done;
        for k = 0 to o-1 do
	  let arg1 = match f with
	    | 'a' -> int_of_string (input_line inf)
	    | 'i' -> (* unlit inf *) int_of_string (input_line inf)
	    | _ -> failwith "Invalid aiger format" in
          outputs.(k) <- (arg1,sprintf "o%d" k);
	  if !verbose then Printf.fprintf logfile "%d\n" arg1;
        done;
        for k = 0 to a-1 do
	  let getand arg1 arg2 arg3 =
            ands.(k) <- (arg1,arg2,arg3);
	    if !verbose then Printf.fprintf logfile "%d %d %d\n" arg1 arg2 arg3 in
	  match f with
	    | 'a' -> Scanf.sscanf (input_line inf) "%d %d %d" getand
	    | 'i' -> let lhs = ((i+k+l+1)*2) in
		     let delta0 = unlit inf in
		     let delta1 = unlit inf in
		     let rhs0 = (lhs - delta0) in
		     let rhs1 = (rhs0 - delta1) in
		     getand lhs rhs0 rhs1
	    | _ -> failwith "Invalid aiger format"
        done;
        begin
          try let comment = ref true in
	      while !comment do
		let sym = input_line inf in
		let cmd = sym.[0] in
		if !verbose then Printf.fprintf logfile "Symbol %d %s\n" (int_of_char cmd) sym;
		match cmd with
		| 'i' -> Scanf.sscanf sym "i%d %s"
                  begin
                    fun idx nam -> inputs.(idx) <- (fst inputs.(idx),nam);
		      if !verbose then Printf.fprintf logfile "i%d %s\n" idx nam;
                  end
		| 'l' -> Scanf.sscanf sym "l%d %s"
                  begin
                    fun idx nam ->
                      let (now,nxt,_) = latches.(idx) in
                      latches.(idx) <- (now,nxt,nam)
                  end
		| 'o' -> Scanf.sscanf sym "o%d %s"
                  begin
                    fun idx nam -> outputs.(idx) <- (fst outputs.(idx),nam)
                  end
		| 'c' -> comment := false
		| oth -> failwith (sprintf "character %c not understood" oth)
              done;
              model := input_line inf;
	      if !model.[String.length !model - 1] = '\000' then
		model := String.sub !model 0 (String.length !model - 1);
	      printf "Comment: \"%s\"\n" !model;
	      flush stdout
          with err -> ()
        end;
        close_in inf;
        for k = 0 to l-1 do
          begin
              let (arg1,arg2,nam) = latches.(k) in
              net_type logfile nets arg1 (AigLatchOut nam)
          end
        done;
        for k = 0 to a-1 do
          begin
              let (arg1,arg2,arg3) = ands.(k) in
	      if arg1 mod 2 = 1 then failwith "input inversion not supported";
              net_type logfile nets arg1 (AigAndOut arg1);
          end
        done;
        for k = 0 to i-1 do
	  let (arg1,name) = inputs.(k) in
	  if arg1 mod 2 = 1 then failwith "input inversion not supported";
          net_type logfile nets arg1 (AigInput name)
          done;
        for k = 0 to o-1 do
          let (arg1,nam) = outputs.(k) in
          (match nets.(arg1/2) with AigOutput _ ->
            printf "Output %d has type %s and so is constant\n" (arg1/2) (aigstr nets.(arg1/2))
	  | _ -> ());
        done;
        (!model,nets,inputs,latches,outputs,ands)
    end

let convert_ios iohash iolst dir =
    List.iter (function
      | TRIPLE (BITSEL, ID id, INT idx) -> Hashtbl.replace iohash id 
        (if Hashtbl.mem iohash id then (idx :: Hashtbl.find iohash id) else [idx])
      | ID id -> Hashtbl.replace iohash id []
      | other -> ()) iolst;
    let iolst2 = ref [] in
    let iolst3 = ref [] in
    Hashtbl.iter (fun k lst ->
      iolst2 := ID k :: !iolst2;
      let srt = List.sort compare lst in
      let rng = (match srt with
        | [] -> EMPTY;
        | x::[] -> RANGE (INT x, INT x)
        | x::y -> RANGE (INT (List.nth srt (List.length y)), INT x)) in
      if (dir <> WIRE) then
        iolst3 := QUINTUPLE(dir, EMPTY, EMPTY, rng, TLIST [TRIPLE (ID k, EMPTY, EMPTY)]) :: !iolst3
      else 
        iolst3 := QUADRUPLE(WIRE, EMPTY, TRIPLE (EMPTY, rng, EMPTY), TLIST [DOUBLE (ID k, EMPTY)]) :: !iolst3
    ) iohash;
    (!iolst2,!iolst3)

let wirnam' inv = function
	| AigInput str -> aigerGetName inv str
	| AigOutput str -> aigerGetName inv str
	| AigLatchOut str -> aigerGetName inv str
	| AigUnknown num -> if num > 0 then aigerGetName inv ("AigUnknown"^string_of_int num) else BINNUM "1'b0"
	| AigLatchIn num -> aigerGetName inv ("AigLatchIn"^string_of_int num)
	| AigAndIn num -> aigerGetName inv ("AigAndIn"^string_of_int num)
	| AigAndOut num -> aigerGetName inv ("AigAndOut"^string_of_int num)

let wirnam inst nets arg =
  let noninv = wirnam' Noninv nets.(arg/2) in
  if arg mod 2 = 0 then noninv
  else
    begin
      let inv = wirnam' Inv nets.(arg/2) in
      let cellpins = [TRIPLE (CELLPIN, ID (List.hd mybuf.inv.ipinlst).idpin, noninv);
		      TRIPLE (CELLPIN, ID (List.hd mybuf.inv.opinlst).idpin, inv)] in
      let inv1 = QUADRUPLE(MODINST,
				ID mybuf.inv.nam,
				EMPTY,
				TLIST [TRIPLE(ID (enterid (sprintf "%s_%.6d" mybuf.inv.nam.id arg)), SCALAR, TLIST cellpins)]) in
      if not (Hashtbl.mem inst inv1) then
	Hashtbl.add inst inv1 ();
      inv
    end

let cnv_aiger logfile model nets inputs latches outputs ands =
  let iolst = ref [] in
  let decl = Hashtbl.create 256 and inst = Hashtbl.create 256 in
  let iohashin = Hashtbl.create 256 in
  let (inlst2,inlst3) = convert_ios iohashin (Array.to_list ( Array.map (fun (ix,str) -> aigerGetName Noninv str) inputs)) INPUT in
  List.iter (fun arg -> if not (List.mem arg !iolst) then iolst := arg :: !iolst) inlst2;
  let iohashout = Hashtbl.create 256 in
  let (olst2,olst3) = convert_ios iohashout ( Array.to_list ( Array.map (fun (ix,str) -> aigerGetName Noninv str) outputs)) OUTPUT in
  List.iter (fun arg -> if not (List.mem arg !iolst) then iolst := arg :: !iolst) olst2;
  let wires = ref [] in
  Array.iteri (fun ix (a,b,c) ->
    let knd = mybuf.logand.nam.id in
    let cellpins = List.map (fun (pinid,arg) ->
      let wir = wirnam inst nets arg in
      wires := wir :: !wires;
      TRIPLE (CELLPIN, ID pinid.idpin, wir)
    ) [(List.nth mybuf.logand.opinlst (0),a);
       (List.nth mybuf.logand.ipinlst (0),b);
       (List.nth mybuf.logand.ipinlst (1),c)] in
    Hashtbl.add inst (QUADRUPLE(MODINST,
				ID (enterid knd),
				EMPTY,
				TLIST [TRIPLE (ID (enterid (sprintf "%s_%.6d" knd ix)), SCALAR, TLIST cellpins)])) ();
  ) ands;
  Array.iter (fun (d,q,nam) ->
    wires := wirnam inst nets d :: wirnam inst nets q :: !wires;
  ) latches;
  Array.iter (fun (arg,nam) ->
    if !verbose then fprintf logfile "Output %d is%sinverted\n" arg (if arg mod 2 = 0 then " not " else " ");
    let noninv = wirnam' Noninv nets.(arg/2) in
    let op = aigerGetName Noninv nam in
    let isinv = if arg mod 2 = 1 then mybuf.inv else mybuf.buf in
    let cellpins = [TRIPLE (CELLPIN, ID (List.hd isinv.ipinlst).idpin, noninv);
		    TRIPLE (CELLPIN, ID (List.hd isinv.opinlst).idpin, op)] in
    let inv1 = QUADRUPLE(MODINST,
			 ID isinv.nam,
			 EMPTY,
			 TLIST [TRIPLE(ID (enterid (sprintf "%s_%.6d_%s" isinv.nam.id arg nam)), SCALAR, TLIST cellpins)]) in
    if noninv <> op then Hashtbl.add inst inv1 ();
  ) outputs;
  let wirhash = Hashtbl.create 256 in
  let (wlst2,wlst3) = convert_ios wirhash !wires WIRE in
  List.iter (fun arg -> Hashtbl.add decl arg ()) wlst3;

  let aigerclk = ref (!aigerclkenv) in
  List.iter (fun arg ->
    match arg with
    | QUINTUPLE(INPUT, EMPTY, EMPTY, rng, TLIST [TRIPLE (ID id, EMPTY, EMPTY)]) ->
      let wir = QUADRUPLE(WIRE, EMPTY, TRIPLE (EMPTY, rng, EMPTY), TLIST [DOUBLE (ID id, EMPTY)]) in
      if not (Hashtbl.mem decl wir) then
	begin
          let clk = ref false in
	  if (String.lowercase id.id).[0] = 'c' then
	    begin
	      let aigclk = match rng with
		| EMPTY -> ID id
		| RANGE(INT hi, INT lo) -> TRIPLE(BITSEL, ID id, INT lo)
		| oth -> unhandled stderr 155 oth; EMPTY in
	      if !aigerclk = UNKNOWN || String.length (Count.tokenstr !aigerclk) > String.length (Count.tokenstr aigclk) then
		begin
		  print_endline ("Assuming "^id.id^" is a clock on grounds that it is an unconnected pin beginning with 'c'!");
		  aigerclk := aigclk;
		  clk := true
		end
	    end;
	  if not !clk then print_endline ("Input "^id.id^" is not used");
	end
    | oth -> unhandled stderr 157 oth;
    ) inlst3;
  List.iter (fun arg -> Hashtbl.add decl arg ()) inlst3;
  List.iter (fun arg -> Hashtbl.add decl arg ()) olst3;

  Array.iter (fun (q,d,nam) ->  
    let cellpins = [TRIPLE (CELLPIN, ID (mybuf.ff.dat), wirnam inst nets d);
		    TRIPLE (CELLPIN, ID (mybuf.ff.clk), !aigerclk);
		    TRIPLE (CELLPIN, ID mybuf.ff.qout, wirnam inst nets q)] in
    Hashtbl.add inst (QUADRUPLE(MODINST,
				ID mybuf.ff.nam,
				EMPTY,
				TLIST [TRIPLE (ID (enterid (mybuf.ff.nam.id^"$"^nam)), SCALAR, TLIST cellpins)])) ();
  ) latches;
  print_endline ("aiger model "^ model ^" conversion completed");
  (!iolst,decl,inst)

let read_aiger fil =
  let logfile = if !verbose then unique_open "read_aiger.log" else stdout in
  let (model,nets,inputs,latches,outputs,ands) = read_aiger_file logfile fil in
  print_endline ("aiger file "^fil^" read");
  let (iolst,decl,inst) = cnv_aiger logfile model nets inputs latches outputs ands in
  if !verbose then close_out logfile;
  let modul = QUINTUPLE(MODULE, ID (enterid model), EMPTY, TLIST iolst, THASH (decl,inst)) in
  Semantics.prescan stderr !Globals.archenv modul "Generated by read_aiger";
  print_endline ("aiger model "^ model ^" completed")
