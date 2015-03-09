open Gzip
open Xml
open Printf
open Globals
open Idhash
open Vparser

type attr_t = {mutable depth:int;
               mutable width:int;
               mutable marked:bool}

type layout_t = {kind: string;
                 pins: token list;
                 this: (idhash,token)Hashtbl.t;
                 attr: attr_t;
                 mutable offx: float;
                 mutable offy: float;
                 lib: mybuft;
                 instid: string;
                 mutable conncnt:int}

let nethist = Hashtbl.create 256
let insthist = Hashtbl.create 256
let hist histhash depth =
  let cnt = (if Hashtbl.mem histhash depth then
      Hashtbl.find histhash depth else 0)+1 in
  Hashtbl.replace histhash depth cnt; cnt
  
let nul_attr () = {depth=0;width=0;marked=false}

let (objhash:(string, string * (string -> string -> float * float -> (float*float) list -> (int*string*int) list -> xml) * xml) Hashtbl.t) = Hashtbl.create 256

let ofind key = if Hashtbl.mem objhash key then Hashtbl.find objhash key else failwith ("objhash does not contain the key "^key)
let gen_create id key x y = let (typ,fn,_) = ofind key in fn id typ (x,y) []
and seg_create id key x y lst = let (typ,fn,_) = ofind key in fn id typ (x,y) lst

let and_create id = gen_create id "and"
and or_create id = gen_create id "or"
and xor_create id = gen_create id "xor"
and buf_create id = gen_create id "buffer"
and inv_create id = gen_create id "inverter"
and nand_create id = gen_create id "nand"
and nor_create id = gen_create id "nor"
and not_create id = gen_create id "not"
and conn_create id = gen_create id "connector"
and line_create id = seg_create id "line"
and poly_create id = seg_create id "poly"

let colour = sprintf "#%.2x%.2x%.2x"
let white = colour 255 255 255
let dimred = colour 128 0 0
let black = colour 0 0 0 
let cream = colour 255 255 224

let maxdepth = ref 0

let inst_place hashr = if hashr.attr.marked
  then
    let yoffset = !maxdepth + 2 in
    ((float_of_int hashr.attr.width)*.2.5,(float_of_int (yoffset-hashr.attr.depth))*.2.5)
  else
    (hashr.offx,hashr.offy)

let port_place attr =
  let yoffset = !maxdepth + 2 in
  let x = ((float_of_int attr.width)*.2.5)+.4.
  and y = ((float_of_int (yoffset-attr.depth))*.2.5)+.4.
  in (x,y)

let objinc id = let obj = "O"^string_of_int !id in incr id; obj

let genfunc prop = (match prop with
  | Por(arg1,arg2) -> or_create
  | Pand(arg1,arg2) -> and_create
  | Pnot arg -> (match arg with
      | Por(arg1,arg2) -> nor_create
      | Pand(arg1,arg2) -> nand_create
      | Pvar str -> inv_create
      | _ -> conn_create)
  | Pvar str -> buf_create
  | _ -> conn_create)

let rect_create obj typ ver (posx,posy) lst = Element
            ("dia:object",
             [("type", typ); ("version", string_of_int ver); ("id", obj)],
             [Element
                 ("dia:attribute", [("name", "obj_pos")],
                  [Element ("dia:point", [("val", sprintf "%f,%f" posx posy)], [])]);
              Element
                ("dia:attribute", [("name", "obj_bb")],
                 [Element
                     ("dia:rectangle", [("val", sprintf "%f,%f;%f,%f" (posx-.1.) (posy-.1.) (posx+.1.) (posy+.1.))], [])])] @
              lst)

let text_create id txt (posx,posy) _ _ = rect_create id "Standard - Text" 1 (posx,posy) [Element
          ("dia:attribute", [("name", "text")],
           [Element
             ("dia:composite", [("type", "text")],
              [Element
                ("dia:attribute", [("name", "string")],
                 [Element ("dia:string", [], [PCData txt])]);
               Element
                ("dia:attribute", [("name", "font")],
                 [Element
                   ("dia:font",
                    [("family", "DejaVu Serif"); ("style", "0");
                     ("name", "Courier")],
                    [])]);
               Element
                ("dia:attribute", [("name", "height")],
                 [Element ("dia:real", [("val", "0.8")], [])]);
               Element
                ("dia:attribute", [("name", "pos")],
                 [Element ("dia:point", [("val", sprintf "%f,%f" posx posy)], [])]);
               Element
                ("dia:attribute", [("name", "color")],
                 [Element ("dia:color", [("val", dimred)], [])]);
               Element
                ("dia:attribute", [("name", "alignment")],
                 [Element ("dia:enum", [("val", "0")], [])])])]);
         Element
          ("dia:attribute", [("name", "valign")],
           [Element ("dia:enum", [("val", "3")], [])])]

let arrow id len wid = [ Element
                ("dia:attribute", [("name", "end_arrow")],
                 [Element ("dia:enum", [("val", "22")], [])]);
              Element
                ("dia:attribute", [("name", "end_arrow_length")],
                 [Element ("dia:real", [("val", string_of_float len)], [])]);
              Element
                ("dia:attribute", [("name", "end_arrow_width")],
                 [Element ("dia:real", [("val", string_of_float wid)], [])])]

let ep_create id typ (posx,posy) eplst kind tail = rect_create id typ 0 (posx,posy) (Element
                ("dia:attribute", [("name", kind)],
                 List.map (fun (x,y) -> Element ("dia:point", [("val", sprintf "%f,%f" x y)], [])) eplst) :: tail)

let connect_create connlst = if connlst <> [] then
              [Element
                ("dia:connections", [],
                 List.map (fun (hand,connto,conn) -> Element
                     ("dia:connection",
                      [("handle", string_of_int hand);
                       ("to", connto); 
                       ("connection", string_of_int conn)], [])) connlst)] else []

let pol_create id typ (posx,posy) eplst connlst =
  ep_create id typ (posx,posy) eplst "poly_points" ((arrow id 0.75 0.75) @ (connect_create connlst))
  
let lin_create id typ (posx,posy) eplst connlst = ep_create id typ (posx,posy) eplst "conn_endpoints"
              (Element
                ("dia:attribute", [("name", "numcp")],
                 [Element ("dia:int", [("val", "1")], [])]):: ((arrow id 0.5 0.5) @ (connect_create connlst)))

let obj_create id typ (posx,posy) _ _ = rect_create id typ 1 (posx,posy) [Element
                ("dia:attribute", [("name", "meta")],
                 [Element ("dia:composite", [("type", "dict")], [])]);
              Element
                ("dia:attribute", [("name", "elem_corner")],
                 [Element ("dia:point", [("val", sprintf "%f,%f" posx posy)], [])]);
              Element
                ("dia:attribute", [("name", "elem_width")],
                 [Element ("dia:real", [("val", "1.6")], [])]);
              Element
                ("dia:attribute", [("name", "elem_height")],
                 [Element ("dia:real", [("val", "2")], [])]);
              Element
                ("dia:attribute", [("name", "line_width")],
                 [Element ("dia:real", [("val", "0.1")], [])]);
              Element
                ("dia:attribute", [("name", "line_colour")],
                 [Element ("dia:color", [("val", black)], [])]);
              Element
                ("dia:attribute", [("name", "fill_colour")],
                 [Element ("dia:color", [("val", white)], [])]);
              Element
                ("dia:attribute", [("name", "show_background")],
                 [Element ("dia:boolean", [("val", "true")], [])]);
              Element
                ("dia:attribute", [("name", "line_style")],
                 [Element ("dia:enum", [("val", "0")], []);
                  Element ("dia:real", [("val", "1")], [])]);
              Element
                ("dia:attribute", [("name", "flip_horizontal")],
                 [Element ("dia:boolean", [("val", "false")], [])]);
              Element
                ("dia:attribute", [("name", "flip_vertical")],
                 [Element ("dia:boolean", [("val", "false")], [])]);
              Element
                ("dia:attribute", [("name", "subscale")],
                 [Element ("dia:real", [("val", "1")], [])])]

let top_create margin objsort =
  Element
    ("dia:diagram", [("xmlns:dia", "http://www.lysator.liu.se/~alla/dia/")],
     [Element
         ("dia:diagramdata", [],
          [Element
              ("dia:attribute", [("name", "background")],
               [Element ("dia:color", [("val", cream)], [])]);
           Element
             ("dia:attribute", [("name", "pagebreak")],
              [Element ("dia:color", [("val", cream)], [])]);
           Element
             ("dia:attribute", [("name", "paper")],
              [Element
                  ("dia:composite", [("type", "paper")],
                   [Element
                       ("dia:attribute", [("name", "name")],
                        [Element ("dia:string", [], [PCData "#A4#"])]);
                    Element
                      ("dia:attribute", [("name", "tmargin")],
                       [Element ("dia:real", [("val", string_of_float margin)], [])]);
                    Element
                      ("dia:attribute", [("name", "bmargin")],
                       [Element ("dia:real", [("val", string_of_float margin)], [])]);
                    Element
                      ("dia:attribute", [("name", "lmargin")],
                       [Element ("dia:real", [("val", string_of_float margin)], [])]);
                    Element
                      ("dia:attribute", [("name", "rmargin")],
                       [Element ("dia:real", [("val", string_of_float margin)], [])]);
                    Element
                      ("dia:attribute", [("name", "is_portrait")],
                       [Element ("dia:boolean", [("val", "true")], [])]);
                    Element
                      ("dia:attribute", [("name", "scaling")],
                       [Element ("dia:real", [("val", "0.5")], [])]);
                    Element
                      ("dia:attribute", [("name", "fitto")],
                       [Element ("dia:boolean", [("val", "false")], [])])])]);
           Element
             ("dia:attribute", [("name", "grid")],
              [Element
                  ("dia:composite", [("type", "grid")],
                   [Element
                       ("dia:attribute", [("name", "width_x")],
                        [Element ("dia:real", [("val", "0")], [])]);
                    Element
                      ("dia:attribute", [("name", "width_y")],
                       [Element ("dia:real", [("val", "0")], [])]);
                    Element
                      ("dia:attribute", [("name", "visible_x")],
                       [Element ("dia:int", [("val", "0")], [])]);
                    Element
                      ("dia:attribute", [("name", "visible_y")],
                       [Element ("dia:int", [("val", "0")], [])]);
                    Element ("dia:composite", [("type", "color")], [])])]);
           Element
             ("dia:attribute", [("name", "color")],
              [Element ("dia:color", [("val", cream)], [])]);
           Element
             ("dia:attribute", [("name", "guides")],
              [Element
                  ("dia:composite", [("type", "guides")],
                   [Element ("dia:attribute", [("name", "hguides")], []);
                    Element ("dia:attribute", [("name", "vguides")], [])])])]);
      Element
        ("dia:layer",
         [("name", "Background"); ("visible", "true"); ("active", "true")],
         Array.to_list objsort)])

let topxml oc stats insts =
  let unhand n other = Dump.unhandled stderr n other; fprintf stderr "%s\n" (Ord.getstr other)
  and layout = Hashtbl.create 256
  and offset = ref 0.
  and porthash = Hashtbl.create 256
  and [inputs;outputs] = List.map (fun req -> Read_library.hfilter (function
    | QUINTUPLE(dir, EMPTY, EMPTY, rng, TLIST idlst) when dir=req -> true
    | _ -> false) (fst insts)) [INPUT;OUTPUT]
  and netid hash net rng dir = function
    | Some ref ->
    if Hashtbl.mem hash net then
      let (lst,old_is_top,old_rng,old_dir,old_attr) = Hashtbl.find hash net in
      Hashtbl.replace hash net (ref :: lst, old_is_top, old_rng, old_dir, old_attr)
    else
      Hashtbl.replace hash net ([ref],false,rng,dir,nul_attr())
    | None ->
        Hashtbl.replace hash net ([],true,rng,dir,nul_attr()) in
  let dumpio hash =
  List.iter (function
    | QUINTUPLE(dir, EMPTY, EMPTY, rng, TLIST idlst) -> let dirstr = String.uppercase(Ord.getstr dir) in List.iter (function
        | TRIPLE (ID itm, EMPTY, EMPTY) -> (match rng with
            | RANGE(INT hi, INT lo) ->
              for i = lo to hi do netid hash (TRIPLE (BITSEL, ID itm, INT i)) rng dir None done;
(*
              printf "           (port (array (rename %s \"%s[%d:%d]\") %d) (direction %s))\n"
                itm itm hi lo (hi-lo+1) dirstr
*)
            | EMPTY ->
              netid hash (ID itm) rng dir None;
(*
              printf "           (port %s (direction %s))\n" itm dirstr
*)
            | other -> unhand 194 other)
        | other -> unhand 195 other) idlst
    | other -> unhand 196 other) (outputs@inputs)
  and dumpskel id =
    begin
      let thishash = Hashtbl.create 256 in
      Hashtbl.replace porthash id thishash;
      let submod = Hashtbl.find modprims id in match submod.tree with
        | QUINTUPLE(MODULE, ID top, EMPTY, TLIST iolst, THASH insts) ->
          Hashtbl.iter (fun k _ -> match k with
            | QUINTUPLE(dir, EMPTY, EMPTY, rng, TLIST idlst) ->
              let dirstr = String.uppercase(Ord.getstr dir) in
              List.iter (function
                | TRIPLE (ID itm, EMPTY, EMPTY) -> (match rng with
                    | RANGE(expr1, expr2) -> let (hi,lo,dir) = Const.iwidth stderr submod.symbols rng in
(*
                      printf "           (port (array (rename %s \"%s[%d:%d]\") %d) (direction %s))\n"
                        itm itm hi lo (hi-lo+1) dirstr;
*)
                        Hashtbl.replace thishash itm (RANGE(INT hi,INT lo))
                    | EMPTY ->
(*
  printf "           (port %s (direction %s))\n" itm dirstr;
*)
                        Hashtbl.replace thishash itm EMPTY                      
                    | other -> unhand 163 other)
                | other -> unhand 164 other) idlst
            | QUADRUPLE((REG|TRI0|WIRE), _, _, _) -> ()
            | SEXTUPLE(PARAMETER,_, _, _, _, _) -> ()
            | other -> unhand 166 other) (fst insts);
        | other -> unhand 170 other;
    end in
  let rec markinst reflst depth =
      List.iter (fun (pinref,member,instid) ->
        let found = Hashtbl.find layout instid in
        List.iter (function
          | TRIPLE(CELLPIN, ID pinref, BINNUM lev) -> if Read_library.is_member pinref found.lib.opinlst then
            let net = Hashtbl.find hash (BINNUM lev) in onenet (depth+1) net
          | TRIPLE(CELLPIN, ID pinref, TRIPLE (BITSEL, ID netref, INT idx)) -> if Read_library.is_member pinref found.lib.opinlst then
            let net = Hashtbl.find hash (TRIPLE (BITSEL, ID netref, INT idx)) in onenet (depth+1) net
          | TRIPLE(CELLPIN, ID pinref, ID netref) -> if Read_library.is_member pinref found.lib.opinlst then
              (match Hashtbl.find found.this pinref with
                | RANGE(INT hi, INT lo) ->
                  for idx = hi downto lo do
                    let net = Hashtbl.find hash (TRIPLE (BITSEL, ID netref, INT idx)) in onenet (depth+1) net
                  done
                | EMPTY -> let net = Hashtbl.find hash (ID netref) in onenet (depth+1) net
                | other -> unhand 272 other)
          | TRIPLE(CELLPIN, ID pinref, DOUBLE(CONCAT, TLIST lst)) ->  if Read_library.is_member pinref found.lib.opinlst then
            Array.iteri (fun member x -> match x with
              | ID net ->
                let net = Hashtbl.find hash (ID net) in onenet (depth+1) net
              | TRIPLE (BITSEL, ID bus, INT idx) ->
                let net = Hashtbl.find hash (TRIPLE (BITSEL, ID bus, INT idx)) in onenet (depth+1) net
              | other -> unhand 279 other
            ) (Array.of_list lst)
          | DOUBLE(CELLPIN, ID pinref) -> () (* unconnected pins need not be mentioned *)
          | EMPTY -> () (* probably should not occur *)
          | other -> unhand 283 other) found.pins;
        found.attr.depth <- depth;
        if !maxdepth < depth then maxdepth := depth;
        found.attr.width <- hist insthist depth;
        found.attr.marked <- true;
      ) reflst
  and onenet depth (reflst,is_top,rng,dir,attr) = if depth < attr.depth or not attr.marked then
        begin
          attr.depth <- depth;
          if !maxdepth < depth then maxdepth := depth;
          attr.width <- hist nethist depth;
          attr.marked <- true;
          markinst reflst (depth+1)
        end
  and dumpnets nethash = Hashtbl.iter (fun k (reflst,is_top,rng,dir,attr) -> match k with
    | TRIPLE (BITSEL, ID netref, INT idx) -> if is_top && dir=INPUT then onenet 0 (reflst,is_top,rng,dir,attr)
    | ID netref -> if is_top && dir=INPUT then onenet 0 (reflst,is_top,rng,dir,attr)
    | BINNUM "1'b0" -> markinst reflst 0
    | BINNUM "1'b1" -> markinst reflst 0
    | other -> unhand 102 other) nethash
  and dumpinst hash id = function
    | QUADRUPLE(MODINST, ID cellref, EMPTY, TLIST kindlst) ->
      if Hashtbl.mem libhash cellref.id then
        begin
          let libref = Hashtbl.find libhash cellref.id
          and thishash = if Hashtbl.mem porthash cellref.id
            then Hashtbl.find porthash cellref.id
            else (Printf.printf "Internal error: cellref %s ports not found\n" cellref.id; Hashtbl.create 256) in
          List.iter (function
            | TRIPLE(ID instid, SCALAR, TLIST pinlst) ->             
              List.iter (function
                | TRIPLE(CELLPIN, ID pinref, BINNUM lev) ->
                  netid hash (BINNUM lev) UNKNOWN UNKNOWN (Some(pinref,-1,instid))
                | TRIPLE(CELLPIN, ID pinref, TRIPLE (BITSEL, ID netref, INT idx)) ->
                  netid hash (TRIPLE (BITSEL, ID netref, INT idx)) UNKNOWN UNKNOWN (Some(pinref,-1,instid))
                | TRIPLE(CELLPIN, ID pinref, ID netref) -> (match Hashtbl.find thishash pinref with
                    | RANGE(INT hi, INT lo) ->
                      for idx = hi downto lo do
                        netid hash (TRIPLE (BITSEL, ID netref, INT idx)) UNKNOWN UNKNOWN (Some(pinref,hi-idx,instid))
                      done
                    | EMPTY -> netid hash (ID netref) UNKNOWN UNKNOWN (Some(pinref,-1,instid))
                    | other -> unhand 74 other)
                | TRIPLE(CELLPIN, ID pinref, DOUBLE(CONCAT, TLIST lst)) ->
                  Array.iteri (fun member x -> match x with
                    | ID net ->
                      netid hash (ID net) UNKNOWN UNKNOWN (Some(pinref,member,instid))
                    | TRIPLE (BITSEL, ID bus, INT idx) ->
                      netid hash (TRIPLE (BITSEL, ID bus, INT idx)) UNKNOWN UNKNOWN (Some(pinref,member,instid))
                    | other -> unhand 206 other
                  ) (Array.of_list lst)
                | DOUBLE(CELLPIN, ID pinref) -> () (* unconnected pins need not be mentioned *)
                | EMPTY -> () (* probably should not occur *)
                | other -> unhand 210 other) pinlst;
              offset := !offset +. 2.;
              Hashtbl.add layout instid
                {kind=cellref.id;pins=pinlst;this=thishash;offx= !offset;offy=2.;lib=libref;attr=nul_attr();instid=objinc id;conncnt=0}
            | other -> unhand 213 other) kindlst
        end
    | other -> unhand 214 other
  and hash = Hashtbl.create 256
  and onenet' (netref:string) objlst id (reflst,is_top,rng,dir,attr) =
    begin
      let (x,y) = port_place attr
      and ptlst = ref []
      and connlst = ref [] in
      ptlst := (x,y-.0.5) :: !ptlst;
      ptlst := (x,y-.2.5) :: !ptlst;
      if attr.marked then
        begin
          let hand = ref 0 in
          List.iter (fun (pinref,member,instid) ->
            let found = Hashtbl.find layout instid in
            connlst := (found.conncnt,found.instid,!hand) :: !connlst;
            found.conncnt <- found.conncnt + 1;
            incr hand;
            let (x,y) = inst_place found in
            ptlst := (x+.0.8,y+.2.) :: !ptlst;
(*
            List.iter (function
              | TRIPLE(CELLPIN, ID pinref, BINNUM lev) -> if Read_library.is_member pinref found.lib.opinlst then
                  let net = Hashtbl.find hash (BINNUM lev) in onenet (depth+1) net
              | TRIPLE(CELLPIN, ID pinref, TRIPLE (BITSEL, ID netref, INT idx)) -> if Read_library.is_member pinref found.lib.opinlst then
                  let net = Hashtbl.find hash (TRIPLE (BITSEL, ID netref, INT idx)) in onenet (depth+1) net
              | TRIPLE(CELLPIN, ID pinref, ID netref) -> if Read_library.is_member pinref found.lib.opinlst then
                  (match Hashtbl.find found.this pinref with
                    | RANGE(INT hi, INT lo) ->
                      for idx = hi downto lo do
                        let net = Hashtbl.find hash (TRIPLE (BITSEL, ID netref, INT idx)) in onenet (depth+1) net
                      done
                    | EMPTY -> let net = Hashtbl.find hash (ID netref) in onenet (depth+1) net
                    | other -> unhand 272 other)
              | TRIPLE(CELLPIN, ID pinref, DOUBLE(CONCAT, TLIST lst)) ->  if Read_library.is_member pinref found.lib.opinlst then
                  Array.iteri (fun member x -> match x with
                    | ID net ->
                      let net = Hashtbl.find hash (ID net) in onenet (depth+1) net
                    | TRIPLE (BITSEL, ID bus, INT idx) ->
                      let net = Hashtbl.find hash (TRIPLE (BITSEL, ID bus, INT idx)) in onenet (depth+1) net
                    | other -> unhand 279 other
                  ) (Array.of_list lst)
              | DOUBLE(CELLPIN, ID pinref) -> () (* unconnected pins need not be mentioned *)
              | EMPTY -> () (* probably should not occur *)
              | other -> unhand 283 other) found.pins;
*)
          ) reflst;
          let ix = !id in objlst := (ix, poly_create (objinc id) x y (List.rev !ptlst) !connlst) :: !objlst;
          let ix = !id in objlst := (ix, text_create (objinc id) ("#"^netref^"#") (x,y) [] []) :: !objlst;
        end
    end
  and dumpnets' nethash objlst id = Hashtbl.iter (fun k (reflst,is_top,rng,dir,attr) -> match k with
    | TRIPLE (BITSEL, ID netref, INT idx) ->
      if is_top && dir=INPUT then onenet' (sprintf "%s[%d]" netref.id idx) objlst id (reflst,is_top,rng,dir,attr)
    | ID netref ->
      if is_top && dir=INPUT then onenet' netref.id objlst id (reflst,is_top,rng,dir,attr)
    | BINNUM "1'b0" -> ()
    | BINNUM "1'b1" -> ()
    | other -> unhand 102 other) nethash
  and id = ref 0
  and liblst = ref [] in
  Hashtbl.iter (fun k _ -> match k with ID id -> liblst := id.id :: !liblst | _ -> failwith (Ord.getstr k)) stats;
  List.iter (fun itm' ->
    let itm = enterid itm' in if not (List.mem itm' !liblst) then liblst := itm' :: !liblst) ["IBUF";"OBUF";"VCC";"GND"];
  List.iter dumpskel !liblst;
  dumpio hash;
  Hashtbl.iter (fun k _ -> dumpinst hash id k ) (snd insts);
  List.iter (fun (a,b,c,d) -> if Hashtbl.mem hash (BINNUM a) then
      begin
        printf "          (instance %s (viewRef netlist (cellRef %s (libraryRef xilinx))))\n" b c;
        netid hash (BINNUM a) UNKNOWN UNKNOWN (Some(enterid d,-1,enterid b))
      end) [("1'b0","inst_gnd","GND","G");("1'b1","inst_vcc","VCC","P")];
  dumpnets hash;
  let objlst = ref [] in
  dumpnets' hash objlst id;
  assert(!id == List.length !objlst + Hashtbl.length layout);
  let objsort = Array.make !id (Element("",[],[])) in
  List.iter(fun (ix,obj) -> try objsort.(ix) <- obj
    with e -> failwith (sprintf "xmlout.ml:486 - array index %d out of range 0..%d" ix (!id-1))) !objlst;
  Hashtbl.iter (fun k hashr ->
    let (x,y) = inst_place hashr in
    let idx = int_of_string(String.sub hashr.instid 1 (String.length hashr.instid -1)) in
    try objsort.(idx) <- (genfunc hashr.lib.prop) hashr.instid x y []
    with e -> failwith (sprintf "xmlout.ml:491 - array index %d out of range 0..%d" idx (!id-1))) layout;  
  top_create 2.8 objsort

let xmlout oc stats insts = let env = "/usr/share/dia/shapes/Logic/" in Array.iter (fun itm ->
  let file = env^itm in
  let len = String.length file in
  if String.sub file (len-6) 6 = ".shape" then
    let xml = Xml.parse_file file in
    let key = String.sub itm 0 (String.length itm - 6) in
    match xml with 
      | Element
          ("shape",
           [("xmlns", "http://www.daa.com.au/~james/dia-shape-ns");
            ("xmlns:svg", "http://www.w3.org/2000/svg")],
           [Element ("name", [], [PCData func]);
            Element ("icon", [], [PCData icon]);
            Element
              ("connections", [], connlst);
            Element ("aspectratio", [("type", "fixed")], []);
            Element
              ("svg:svg", [], pathlst)]) -> Printf.printf "%.12s\t%20s\t(icon %s)\n" key ("\""^func^"\"") icon;
            Hashtbl.add objhash key (func,obj_create,xml)
      | _ -> ()) (Sys.readdir (env));
List.iter (fun (key,typ,fn) -> Hashtbl.add objhash key (typ,fn,Xml.Element("",[],[]))) [
  ("line", "Standard - Line",     lin_create);
  ("text", "Standard - Text",     text_create);
  ("poly", "Standard - PolyLine", pol_create);
];
let topstr = Xml.to_string_fmt (topxml oc stats insts) in
output oc topstr 0 (String.length topstr)

let write_xml_arch arch top =
  List.iter (fun found ->
    let oc = open_out (top^"_"^arch^".dia") in
    (match found.tree with
      | QUINTUPLE(MODULE, ID top, EMPTY, TLIST iolst, THASH insts) -> xmlout oc (Count.count_flat_netlist' found) insts;
      | _ -> failwith "xmlout only handles modules");
    close_out oc) (Count.find_arch arch top)
