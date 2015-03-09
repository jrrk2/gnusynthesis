open Printf
open Latin

let _ =
printf "open Xdl_parser\n\n";
printf "type tuple =\n";
printf "  | CFGLIST of tuple list\n";
printf "  | TUPLIST of token list\n";
for i = 1 to 100 do
let args = "  | " ^ enum i ^ " of (" ^ String.concat " * " (Array.to_list (Array.init i (fun arg -> "token"))) ^ ")" in
printf "%s\n" args done
