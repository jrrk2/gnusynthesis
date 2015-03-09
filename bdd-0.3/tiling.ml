
(* Tiling the 8x8 chessboard with 2x1 dominoes
   (cf TAOCP vol 4, Section 7.1.4, page 49) *)

open Decision

(* there are 2x7x8 = 112 variables *)
let n = 112
let () = set_max_var n
let var i = mk_var (1+i)

(* m[i,j] tells whether domino i occupies cell j *)
let m = Array.create_matrix n 64 false

let () =
  let v = ref 0 in
  for l = 0 to 7 do for c = 0 to 7 do
    (* cell (l,c) of the chessboard is j = 8*l + c *)
    let j = 8 * l + c in
    (* horizontal domino *)
    if c < 7 then begin
      m.(!v).(j) <- true; m.(!v).(j+1) <- true;
      incr v;
    end;
    (* vertical domino *)
    if l < 7 then begin
      m.(!v).(j) <- true; m.(!v).(j+8) <- true;
      incr v;
    end;
  done done

(* col j is the list of all i such that m[i,j]=true *)
let col j = 
  let rec make acc i = 
    if i = n then acc else make (if m.(i).(j) then i::acc else acc) (i+1)
  in
  make [] 0

let bdd = 
  let rec make bdd j =
    if j = 64 then
      bdd 
    else
      let cell_j =
	let cj = col j in
	List.fold_left
	  (fun f i ->
	     mk_or f
	       (List.fold_left
		  (fun f i' ->
		     if i' <> i then mk_and f (mk_not (var i')) else f)
		  (var i)
		  cj))
	  zero cj
      in
      make (mk_and bdd cell_j) (j+1)
  in
  make one 0

open Format

let () = 
  printf "%Ld solutions@." (count_sat bdd)

(* 12988816 solutions *)

let () =
  let s = random_sat bdd in
  List.iter
    (fun (v,b) ->
       let i = v-1 in
       if b then begin
	 Array.iteri (fun j mij -> if mij then printf "%d " j) m.(i);
	 printf "@."
       end)
    s


