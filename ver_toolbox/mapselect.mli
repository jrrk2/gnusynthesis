type mapid = Map_hls | Map_struct | Map_xilinx
  
type tbl = {fst:(Vparser.token, unit) Hashtbl.t;
            snd:(Vparser.token, unit) Hashtbl.t;
            gentab:(string, unit) Hashtbl.t;
            namtab:(string, string) Hashtbl.t;
            defnhash:(Vparser.token*int, Vparser.token*int) Hashtbl.t;
            tused:(Idhash.idhash,Vparser.token) Hashtbl.t;
	    edges : (string, (string*string*bool) list) Hashtbl.t;
	    combs : (string, (string*string*bool) list) Hashtbl.t;
	    edgdmp : (string, Vparser.token*Vparser.token*
	      (Vparser.token*int)*(Vparser.token*int)*(Vparser.token*int)*(Vparser.token*int)) Hashtbl.t;
	    combdmp : (string, Vparser.token*Vparser.token*
			 (Vparser.token*int)*(Vparser.token*int)*(Vparser.token*int)*(Vparser.token*int)) Hashtbl.t;
	    map_hls : hls_t}

   and mapselect = {
   map_redor :  Vparser.token -> tbl -> Vparser.shash -> Vparser.token * int -> Vparser.token * int;
   map_tilde :  Vparser.token -> tbl -> Vparser.shash -> Vparser.token * int -> Vparser.token * int;
   map_unaryplus :  Vparser.token -> tbl -> Vparser.shash -> Vparser.token * int -> Vparser.token * int;
   map_pling :  Vparser.token -> tbl -> Vparser.shash -> Vparser.token * int -> Vparser.token * int;
   map_equal :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_wildequal :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_noteq :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_gte :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_lte :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_less :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_greater :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_logand :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_logor :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_log_pand :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_log_por :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_logxor :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_plus :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_minus :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_mux2 :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
   map_buffer :  tbl ->  Vparser.shash -> (Idhash.idhash -> Vparser.token) -> int -> Vparser.token * Vparser.token -> unit;
   map_flipflop : tbl -> Vparser.shash -> Vparser.token list -> Vparser.token * int -> Vparser.token * int -> unit;
   map_redand :  Vparser.token -> tbl -> Vparser.shash -> Vparser.token * int -> Vparser.token * int;
 }

and hls_t = {
    map_id : mapid;
    map_select: mapselect;
    map_times :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
    map_sleft :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
    map_sright :  Vparser.token ->  tbl ->  Vparser.shash ->  Vparser.token * int -> Vparser.token * int -> Vparser.token * int;
  }
      
