open Bitcask__Adaptive_radix_tree.RADIXOp

let%expect_test _=
  let n = new_node4 in
	match n with
	  | ( _, _, _, children ) ->
         match children with
          |  node ->
            Array.iter (fun n -> Printf.printf "%s" (Format.asprintf "%a" pp_node n)) node;
  [%expect {|
    (Types.MakeRadixNode.Inner_node
       ((Types.MakeRadixNode.Prefix (
           ["\000"; "\000"; "\000"; "\000"; "\000"; "\000"; "\000"; "\000";
             "\000"; "\000"],
           0, 0)),
        (Types.MakeRadixNode.Node4 0), ["\000"; "\000"; "\000"; "\000"],
        Types.MakeRadixNode.Empty, Types.MakeRadixNode.Empty,
        Types.MakeRadixNode.Empty, Types.MakeRadixNode.Empty))(Types.MakeRadixNode.Inner_node
       ((Types.MakeRadixNode.Prefix (
           ["\000"; "\000"; "\000"; "\000"; "\000"; "\000"; "\000"; "\000";
             "\000"; "\000"],
           0, 0)),
        (Types.MakeRadixNode.Node4 0), ["\000"; "\000"; "\000"; "\000"],
        Types.MakeRadixNode.Empty, Types.MakeRadixNode.Empty,
        Types.MakeRadixNode.Empty, Types.MakeRadixNode.Empty))(Types.MakeRadixNode.Inner_node
       ((Types.MakeRadixNode.Prefix (
           ["\000"; "\000"; "\000"; "\000"; "\000"; "\000"; "\000"; "\000";
             "\000"; "\000"],
           0, 0)),
        (Types.MakeRadixNode.Node4 0), ["\000"; "\000"; "\000"; "\000"],
        Types.MakeRadixNode.Empty, Types.MakeRadixNode.Empty,
        Types.MakeRadixNode.Empty, Types.MakeRadixNode.Empty))(Types.MakeRadixNode.Inner_node
       ((Types.MakeRadixNode.Prefix (
           ["\000"; "\000"; "\000"; "\000"; "\000"; "\000"; "\000"; "\000";
             "\000"; "\000"],
           0, 0)),
        (Types.MakeRadixNode.Node4 0), ["\000"; "\000"; "\000"; "\000"],
        Types.MakeRadixNode.Empty, Types.MakeRadixNode.Empty,
        Types.MakeRadixNode.Empty, Types.MakeRadixNode.Empty))
    |}]

let make_nodes parent child size =

     let rec loop_while parent idx key =

            if idx < size then(
                    (* let k = Bytes.get key 0 in *)

                    (* let k_incr = Char.chr (Char.code k + 1) in  (\* increment the char *\) *)

                    let k_incr = Char.chr ( key + 1) in  (* increment the char *)

                    let b_key = Bytes.make 1  k_incr in
		            let newer_parent_node = add_child  b_key parent child in
                    loop_while  newer_parent_node (idx + 1) (key + 1);
            )else parent
     in
     loop_while parent 0 0


let%expect_test _=  (* Add 4 children to Node type 4*)
  let parent = new_node4 in
  let child = new_node4 in
  let parent_node = make_nodes parent (Inner_node child) 4
  in
	match parent_node with
	     | (  Prefix(_, new_size, _), _, keys, children ) ->
           Printf.printf "Size is %d\n" new_size;
           Printf.printf "Size of children %d\n" (Array.length children);

   (* Array.iter (fun n -> Printf.printf "%s" (Format.asprintf "%a" pp_node n)) children; *)

    List.iter ( fun k ->
    Fmt.pr "add_child BYTE representation :[ \\x%02X]\n" (Char.code (Bytes.get  k 0))
    ) keys;
[%expect {|
  Grow node16
  Grow node48
  Size is 52
  Size of children 256
  add_child BYTE representation :[ \x01]
  add_child BYTE representation :[ \x02]
  add_child BYTE representation :[ \x03]
  add_child BYTE representation :[ \x04]
  |}]


let%expect_test "Add 16 children to Node type 16" =
  let parent = new_node16 in
  let child = new_node16 in
  let parent_node = make_nodes parent (Inner_node child) 16
  in
	match parent_node with
	     | (  Prefix(_, new_size, _), _, keys, children ) ->
           Printf.printf "Size is %d\n" new_size;
           Printf.printf "Size of children %d\n" (Array.length children);
           List.iter ( fun k ->
               Fmt.pr "BYTE representation :[ \\x%02X]\n" (Char.code (Bytes.get  k 0))
             ) keys;
   [%expect {|
     Grow node16
     Grow node48
     Size is 64
     Size of children 256
     BYTE representation :[ \x01]
     BYTE representation :[ \x02]
     BYTE representation :[ \x03]
     BYTE representation :[ \x04]
     BYTE representation :[ \x05]
     BYTE representation :[ \x06]
     BYTE representation :[ \x07]
     BYTE representation :[ \x08]
     BYTE representation :[ \x09]
     BYTE representation :[ \x0A]
     BYTE representation :[ \x0B]
     BYTE representation :[ \x0C]
     BYTE representation :[ \x0D]
     BYTE representation :[ \x0E]
     BYTE representation :[ \x0F]
     BYTE representation :[ \x10]
     |}]

let%expect_test "Add 20 children to Node type 16" =

  let parent = new_node16 in
  let child = new_node16 in
  let parent_node = make_nodes parent (Inner_node child) 20
  in
	match parent_node with
	     | (  Prefix(_, new_size, _), _, keys, children ) ->
           Printf.printf "Size is %d\n" new_size;
           Printf.printf "Size of children %d\n" (Array.length children);
           List.iter ( fun k ->
               Fmt.pr "BYTE representation :[ \\x%02X]\n" (Char.code (Bytes.get  k 0))
             ) keys;
   [%expect {|
     Grow node16
     Grow node48
     Size is 68
     Size of children 256
     BYTE representation :[ \x01]
     BYTE representation :[ \x02]
     BYTE representation :[ \x03]
     BYTE representation :[ \x04]
     BYTE representation :[ \x05]
     BYTE representation :[ \x06]
     BYTE representation :[ \x07]
     BYTE representation :[ \x08]
     BYTE representation :[ \x09]
     BYTE representation :[ \x0A]
     BYTE representation :[ \x0B]
     BYTE representation :[ \x0C]
     BYTE representation :[ \x0D]
     BYTE representation :[ \x0E]
     BYTE representation :[ \x0F]
     BYTE representation :[ \x10]
     BYTE representation :[ \x11]
     BYTE representation :[ \x12]
     BYTE representation :[ \x13]
     BYTE representation :[ \x14]
     |}]
