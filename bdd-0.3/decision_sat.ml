(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Format
open Decision

module Time = struct
  
  open Unix
    
  let utime f x =                                                   
    let u = (times()).tms_utime in                                  
    let y = f x in
    let ut = (times()).tms_utime -. u in
    (y,ut)
      
  let print_utime f x = 
    let (y,ut) = utime f x in
    printf "user time: %2.2f@." ut;
    y
      
end

let nb = ref 0

let sat_unsat f =
  incr nb;
  let (_,b) = bdd_of_formula f in
  printf "%d: %s " !nb (if tautology b then "valid" else "invalid")

let check = Time.print_utime sat_unsat
  
let () =
  let lb = Lexing.from_channel stdin in
  let fl = Parser0.file Lexer0.token lb in
  List.iter check fl

let () = 
  Array.iter
    (fun (l,n,s,b1,b2,b3) ->
       printf "table length: %d / nb. entries: %d / sum of bucket length: %d@."
	 l n s;
       printf "smallest bucket: %d / median bucket: %d / biggest bucket: %d@."
	 b1 b2 b3)
    (stats ())


