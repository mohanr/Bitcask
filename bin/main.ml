open Bitcask__Adaptive_radix_tree.RADIXOp

let radix()=
  let n = new_node4 in Printf.printf "Made new_node4!\n%!";
	match n with
	  | ( _, _, _, children ) ->
         match children with
          |  node ->
            Array.iter (fun n -> Printf.printf "%s" (Format.asprintf "%a" pp_node n)) node

let () = radix ()
