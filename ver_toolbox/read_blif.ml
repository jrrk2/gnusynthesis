open Globals
open Vparser
open Idhash
open Dump
open Read_library

let blifclkenv = ref UNKNOWN

let rec split lin =
  if String.contains lin ' ' then
    begin
    let idx = String.index lin ' ' in
    let rhs = split (String.sub lin (idx+1) (String.length lin - idx - 1)) in
      if idx > 0 then
	String.sub lin 0 idx :: rhs
      else
	rhs
    end
  else
    [lin]

let read_blif_file fil =
  let model = ref "unknown_blif" and inputs = ref [] and outputs = ref [] and latches = ref [] and gates = ref [] in
  let fd = Vparse.my_openin fil and looping = ref true in
  while !looping do
    (try
    let str = ref (input_line fd) in
    if String.length !str > 0 then
      begin
	while !str.[String.length !str - 1] = '\\' do
	  str := String.sub !str 0 (String.length !str - 1) ^ input_line fd
	done;
	if !str.[0] <> '#' then
	  begin
	    let splt = split !str in
	    match splt with
	    | [] -> ()
	    | ".model" :: [name] -> model := name
	    | ".inputs" :: inp -> inputs := inp
	    | ".outputs" :: outp -> outputs := outp
	    | ".default_input_arrival" :: arg1 :: [arg2] -> ()
            | ".latch" :: d :: q :: [cnt] -> latches := (d,q,cnt) :: !latches
            | ".gate" :: knd :: iolst ->
	      gates := (knd,iolst) :: !gates
	    | ".end" :: [] -> ()
            | oth -> failwith (String.concat " :: " oth)
	  end
      end
    with End_of_file -> looping := false);
  done;
  close_in fd;
  (!model,!inputs,!outputs,!latches,!gates)

let rec blifGetName pName =
    let pID = ID (enterid pName) in
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
                        buffer := TRIPLE(BITSEL,ID (enterid (String.sub pName 0 !i)), INT idx));
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

let read_blif fil =
  let (model,inputs,outputs,latches,gates) = read_blif_file fil in
  print_endline ("blif file "^fil^" read");
  let iolst = ref [] and lin = ref 0 in
  let decl = Hashtbl.create 256 and inst = Hashtbl.create 256 in
  let iohashin = Hashtbl.create 256 in
  let (inlst2,inlst3) = convert_ios iohashin ( List.map (blifGetName) inputs) INPUT in
  List.iter (fun arg -> if not (List.mem arg !iolst) then iolst := arg :: !iolst) inlst2;
  let iohashout = Hashtbl.create 256 in
  let (olst2,olst3) = convert_ios iohashout ( List.map (blifGetName) outputs) OUTPUT in
  List.iter (fun arg -> if not (List.mem arg !iolst) then iolst := arg :: !iolst) olst2;
  let wires = ref [] in
  List.iter (fun (knd,iolst) ->
    
    let cellpins = List.map (fun arg ->
      let delim = String.index arg '=' in
      let pin = String.sub arg 0 delim in
      let pinid = ID (enterid pin) in
      let wir = blifGetName (String.sub arg (delim+1) (String.length arg - delim - 1)) in
      wires := wir :: !wires;
      TRIPLE (CELLPIN, pinid, wir)
    ) iolst in
    Hashtbl.add inst (QUADRUPLE(MODINST,
				ID (enterid knd),
				EMPTY,
				TLIST [TRIPLE (ID (enterid (knd^string_of_int !lin)), SCALAR, TLIST cellpins)])) ();
    incr lin
  ) gates;
  List.iter (fun (d,q,cnt) ->
    let iwir = blifGetName d and owir = blifGetName q in
    wires := iwir :: owir :: !wires;
  ) latches;
  let wirhash = Hashtbl.create 256 in
(*
  Hashtbl.iter (fun k x -> Hashtbl.add wirhash k x) iohashin;
  Hashtbl.iter (fun k x -> Hashtbl.add wirhash k x) iohashout;
*)
  let (wlst2,wlst3) = convert_ios wirhash !wires WIRE in
  List.iter (fun arg -> Hashtbl.add decl arg ()) wlst3;

  let blifclk = ref (!blifclkenv) in
  List.iter (fun arg ->
    match arg with
    | QUINTUPLE(INPUT, EMPTY, EMPTY, rng, TLIST [TRIPLE (ID id, EMPTY, EMPTY)]) ->
      let wir = QUADRUPLE(WIRE, EMPTY, TRIPLE (EMPTY, rng, EMPTY), TLIST [DOUBLE (ID id, EMPTY)]) in
      if not (Hashtbl.mem decl wir) then (
	print_endline ("Input "^id.id^" is not used");
      if (String.lowercase_ascii id.id).[0] = 'c' then (
	print_endline ("Assuming "^id.id^" is a clock");
	match rng with
	| EMPTY -> blifclk := ID id
	| RANGE(INT hi, INT lo) -> blifclk := TRIPLE(BITSEL, ID id, INT lo)
	| oth -> unhandled stderr 155 oth));
    | oth -> unhandled stderr 157 oth;
    ) inlst3;
  List.iter (fun arg -> Hashtbl.add decl arg ()) inlst3;
  List.iter (fun arg -> Hashtbl.add decl arg ()) olst3;

  List.iter (fun (d,q,cnt) ->  
    let cellpins = [TRIPLE (CELLPIN, ID (mybuf.ff.dat), blifGetName d);
		    TRIPLE (CELLPIN, ID (mybuf.ff.clk), !blifclk);
		    TRIPLE (CELLPIN, ID mybuf.ff.qout, blifGetName q)] in
    Hashtbl.add inst (QUADRUPLE(MODINST,
				ID mybuf.ff.nam,
				EMPTY,
				TLIST [TRIPLE (Minimap.instid [] EndShash mybuf.ff, SCALAR, TLIST cellpins)])) ();
  ) latches;
  print_endline ("blif model "^ model ^" conversion completed");
  let modul = QUINTUPLE(MODULE, ID (enterid model), EMPTY, TLIST !iolst, THASH (decl,inst)) in
  Semantics.prescan stderr !Globals.archenv modul "Generated by read_blif";
  print_endline ("blif model "^ model ^" completed")
