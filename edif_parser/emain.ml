open Eparse

let rslt = ref []

let _ = List.iter ( fun arg ->
  print_endline arg;
  let tree = eparse arg in
  let test = test_match Edif2.EMPTY tree in
  rslt := test :: !rslt
) (List.tl (Array.to_list Sys.argv))
