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

let empty_tree =
    let root = Empty in
    {root = root; size = 0}

let%expect_test "Test insertion" =
	let node = empty_tree in

	let n = insert_tree node
     [Bytes.make 1 'h'; Bytes.make 1 'e'; Bytes.make 1 '1'; Bytes.make 1 '1'; Bytes.make 1 'o']  (Int64.of_int 1) in
     (match n with
	     | Inner_node inn ->
          (match inn with
           |(  Prefix(_, new_size, _), _, keys, children ) ->
           Printf.printf "Size is %d\n" new_size;
           Printf.printf "Size of children %d\n" (Array.length children);
           List.iter ( fun k ->
               Fmt.pr "BYTE representation :[ \\x%02X]\n" (Char.code (Bytes.get  k 0))
             ) keys;
          )
	     | Leaf _  -> Printf.printf "Leaf"
	     | Empty   -> Printf.printf "Empty");
   [%expect {| Leaf |}]

let%expect_test "Test insertion and search" =
	let node = empty_tree in
	let n = insert_tree node
     [Bytes.make 1 'h'; Bytes.make 1 'e'; Bytes.make 1 '1'; Bytes.make 1 '1'; Bytes.make 1 'o']  (Int64.of_int 1) in

	let result = search_after_terminating n [Bytes.make 1 'h'; Bytes.make 1 'e'; Bytes.make 1 '1'; Bytes.make 1 '1'; Bytes.make 1 'o']  0 in
	Printf.printf "%Ld" (match result with | Some i -> i | None -> failwith "Faulty seasrch result");
    [%expect {|
      Searching BYTE representation :[ \x68]
      Searching BYTE representation :[ \x65]
      Searching BYTE representation :[ \x31]
      Searching BYTE representation :[ \x31]
      Searching BYTE representation :[ \x6F]
      Searching BYTE representation :[ \x00]
      Searching BYTE representation :[ \x68]
      Searching BYTE representation :[ \x65]
      Searching BYTE representation :[ \x31]
      Searching BYTE representation :[ \x31]
      Searching BYTE representation :[ \x6F]
      Searching BYTE representation :[ \x00]
      1
      |}]

let%expect_test "Test multiple insertion and search" =
	let node = empty_tree in
	let n = insert_tree node
     [Bytes.make 1 'h'; Bytes.make 1 'e'; Bytes.make 1 '1'; Bytes.make 1 '1'; Bytes.make 1 'o']  (Int64.of_int 1) in
    let updated_root = {root = n ; size = node.size} in
	let n1 = insert_tree updated_root
     [Bytes.make 1 'r'; Bytes.make 1 'o']  (Int64.of_int 2) in
    let updated_root = {root = n1 ; size = node.size} in
	let n2 = insert_tree updated_root
     [Bytes.make 1 'r'; Bytes.make 1 'o'; Bytes.make 1 'u'; Bytes.make 1 'n']  (Int64.of_int 3) in

	let result = search_after_terminating n2 [Bytes.make 1 'r'; Bytes.make 1 'o'] 0 in
	Printf.printf "%Ld" (match result with | Some i -> i | None -> failwith "Faulty seasrch result");
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Failure "Faulty seasrch result")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Test_bitcask.(fun) in file "Bitcask/bitcask/test/test_bitcask.ml", line 221, characters 64-96
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28

  Trailing output
  ---------------
  Searching BYTE representation :[ \x68]
  Searching BYTE representation :[ \x65]
  Searching BYTE representation :[ \x31]
  Searching BYTE representation :[ \x31]
  Searching BYTE representation :[ \x6F]
  Searching BYTE representation :[ \x00]
  Searching BYTE representation :[ \x72]
  Searching BYTE representation :[ \x6F]
  Searching BYTE representation :[ \x00]
  Dest. BYTE representation :[ \x72]
  Dest. BYTE representation :[ \x6F]
  Dest. BYTE representation :[ \x00]
  level 0 copy_bytes Grow node16
  Grow node48
  Searching BYTE representation :[ \x72]
  Searching BYTE representation :[ \x6F]
  Searching BYTE representation :[ \x00]
  Searching BYTE representation :[ \x72]
  Searching BYTE representation :[ \x6F]
  Searching BYTE representation :[ \x75]
  Searching BYTE representation :[ \x6E]
  Searching BYTE representation :[ \x00]
  Dest. BYTE representation :[ \x72]
  Dest. BYTE representation :[ \x6F]
  Dest. BYTE representation :[ \x75]
  Dest. BYTE representation :[ \x6E]
  Dest. BYTE representation :[ \x00]
  level 1 copy_bytes Grow node16
  Grow node48
  Level 0 Key size 3
  |}]
