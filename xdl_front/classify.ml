open Xdl_parser
open Printf
open Tuple

let recog = function
| tuple -> TUPLIST tuple

let classify tuplehash = function
| TLIST tuple -> Hashtbl.replace tuplehash (recog tuple) ()
| _ -> failwith "classify"

let rec find tuplehash = function
| TLIST (CFG::lst) -> List.iter (classify tuplehash) lst; CFGLIST (List.map (function TLIST tuple -> recog tuple | oth -> SINGLE oth) lst)
| oth -> SINGLE oth

let uniquify tuplehash =
  let ohash = Hashtbl.create 256 in
  let olst = ref [] in
  Hashtbl.iter (fun k () -> (match k with
    | TUPLIST lst -> 
      let cnt = ref 0 in let cat = "| "^String.concat "::" (List.map (function ID x -> incr cnt; sprintf "ID arg%d" !cnt | SLIST x -> incr cnt; sprintf "SLIST arg%d" !cnt | oth -> Ord.getstr oth) lst)^" :: [] -> " in
      let cnt = ref 0 in let args = Buffer.create 256 in List.iter (function ID x -> incr cnt; bprintf args ", ID arg%d" !cnt | SLIST x -> incr cnt; bprintf args ", SLIST arg%d" !cnt | oth -> ()) lst;
      if not (Hashtbl.mem ohash cat) then Hashtbl.replace ohash cat (lst,!cnt,Buffer.contents args^")")
    | oth -> ())
  ) tuplehash;

  Hashtbl.iter (fun cat (orig,cnt,args) -> olst := (orig,cat,cnt,args) :: !olst) ohash;
  olst

let genpatterns olst =
  let fd = open_out "patterns.ml" in
  let ix = ref 0 in List.iter (fun (orig,cat,cnt,args) ->
             incr ix;
	     Printf.fprintf fd "%s%s(PATTERN %d%s)\n" cat (Latin.enum cnt) !ix args;
	     ) (List.sort (fun (orig1,cat1,repl1,args1) (orig2,cat2,repl2,args2) -> compare orig1 orig2) !olst);
  close_out fd
