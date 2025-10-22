open Bitcask__Adaptive_radix_tree.RADIXOp

let%expect_test _=
  let n = new_node4() in
	match n with
	  | ( _, _, _, children ) ->
         match children with
          |  node ->
            Array.iter (fun n -> Printf.printf "%s" (Format.asprintf "%a" pp_node n)) node;
  [%expect {|
    DEBUG new_node4: array length=4
      [0] = Empty ✓
      [1] = Empty ✓
      [2] = Empty ✓
      [3] = Empty ✓
    count_non_empty_children: found 0 non-empty in array of length 4
    Types.MakeRadixNode.EmptyTypes.MakeRadixNode.EmptyTypes.MakeRadixNode.EmptyTypes.MakeRadixNode.Empty
    |}]

let make_nodes parent child size =

      Printf.eprintf "add_child ENTER \n%!";
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
  let parent = new_node4() in
  let child = new_node4() in
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
    DEBUG new_node4: array length=4
      [0] = Empty ✓
      [1] = Empty ✓
      [2] = Empty ✓
      [3] = Empty ✓
    count_non_empty_children: found 0 non-empty in array of length 4
    DEBUG new_node4: array length=4
      [0] = Empty ✓
      [1] = Empty ✓
      [2] = Empty ✓
      [3] = Empty ✓
    count_non_empty_children: found 0 non-empty in array of length 4
    add_child ENTER
    count_non_empty_children: found 0 non-empty in array of length 4
      parent size=0 nonempty=0
    count_non_empty_children: found 0 non-empty in array of length 4
    Added child at 0 -> 0
    count_non_empty_children: found 1 non-empty in array of length 4
      parent size=1 nonempty=1
    count_non_empty_children: found 1 non-empty in array of length 4
    Added child at 1 -> 1
    count_non_empty_children: found 2 non-empty in array of length 4
      parent size=2 nonempty=2
    count_non_empty_children: found 2 non-empty in array of length 4
    Added child at 2 -> 2
    count_non_empty_children: found 3 non-empty in array of length 4
      parent size=3 nonempty=3
    count_non_empty_children: found 3 non-empty in array of length 4
    Added child at 3 -> 3
    Size is 4
    Size of children 4
    add_child BYTE representation :[ \x01]
    add_child BYTE representation :[ \x02]
    add_child BYTE representation :[ \x03]
    add_child BYTE representation :[ \x04]
    |}]


let%expect_test "Add 16 children to Node type 16" =
  let parent = new_node16() in
  let child = new_node16() in
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
     count_non_empty_children: found 0 non-empty in array of length 16
     count_non_empty_children: found 0 non-empty in array of length 16
     add_child ENTER
     count_non_empty_children: found 0 non-empty in array of length 16
       parent size=0 nonempty=0
     count_non_empty_children: found 0 non-empty in array of length 16
     Added child at 0 -> Inner_node
     count_non_empty_children: found 1 non-empty in array of length 16
       parent size=1 nonempty=1
     count_non_empty_children: found 1 non-empty in array of length 16
     Added child at 1 -> Inner_node
     count_non_empty_children: found 2 non-empty in array of length 16
       parent size=2 nonempty=2
     count_non_empty_children: found 2 non-empty in array of length 16
     Added child at 2 -> Inner_node
     count_non_empty_children: found 3 non-empty in array of length 16
       parent size=3 nonempty=3
     count_non_empty_children: found 3 non-empty in array of length 16
     Added child at 3 -> Inner_node
     count_non_empty_children: found 4 non-empty in array of length 16
       parent size=4 nonempty=4
     count_non_empty_children: found 4 non-empty in array of length 16
     Added child at 4 -> Inner_node
     count_non_empty_children: found 5 non-empty in array of length 16
       parent size=5 nonempty=5
     count_non_empty_children: found 5 non-empty in array of length 16
     Added child at 5 -> Inner_node
     count_non_empty_children: found 6 non-empty in array of length 16
       parent size=6 nonempty=6
     count_non_empty_children: found 6 non-empty in array of length 16
     Added child at 6 -> Inner_node
     count_non_empty_children: found 7 non-empty in array of length 16
       parent size=7 nonempty=7
     count_non_empty_children: found 7 non-empty in array of length 16
     Added child at 7 -> Inner_node
     count_non_empty_children: found 8 non-empty in array of length 16
       parent size=8 nonempty=8
     count_non_empty_children: found 8 non-empty in array of length 16
     Added child at 8 -> Inner_node
     count_non_empty_children: found 9 non-empty in array of length 16
       parent size=9 nonempty=9
     count_non_empty_children: found 9 non-empty in array of length 16
     Added child at 9 -> Inner_node
     count_non_empty_children: found 10 non-empty in array of length 16
       parent size=10 nonempty=10
     count_non_empty_children: found 10 non-empty in array of length 16
     Added child at 10 -> Inner_node
     count_non_empty_children: found 11 non-empty in array of length 16
       parent size=11 nonempty=11
     count_non_empty_children: found 11 non-empty in array of length 16
     Added child at 11 -> Inner_node
     count_non_empty_children: found 12 non-empty in array of length 16
       parent size=12 nonempty=12
     count_non_empty_children: found 12 non-empty in array of length 16
     Added child at 12 -> Inner_node
     count_non_empty_children: found 13 non-empty in array of length 16
       parent size=13 nonempty=13
     count_non_empty_children: found 13 non-empty in array of length 16
     Added child at 13 -> Inner_node
     count_non_empty_children: found 14 non-empty in array of length 16
       parent size=14 nonempty=14
     count_non_empty_children: found 14 non-empty in array of length 16
     Added child at 14 -> Inner_node
     count_non_empty_children: found 15 non-empty in array of length 16
       parent size=15 nonempty=15
     count_non_empty_children: found 15 non-empty in array of length 16
     Added child at 15 -> Inner_node
     Size is 16
     Size of children 16
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

  let parent = new_node16() in
  let child = new_node16() in
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
     count_non_empty_children: found 0 non-empty in array of length 16
     count_non_empty_children: found 0 non-empty in array of length 16
     add_child ENTER
     count_non_empty_children: found 0 non-empty in array of length 16
       parent size=0 nonempty=0
     count_non_empty_children: found 0 non-empty in array of length 16
     Added child at 0 -> Inner_node
     count_non_empty_children: found 1 non-empty in array of length 16
       parent size=1 nonempty=1
     count_non_empty_children: found 1 non-empty in array of length 16
     Added child at 1 -> Inner_node
     count_non_empty_children: found 2 non-empty in array of length 16
       parent size=2 nonempty=2
     count_non_empty_children: found 2 non-empty in array of length 16
     Added child at 2 -> Inner_node
     count_non_empty_children: found 3 non-empty in array of length 16
       parent size=3 nonempty=3
     count_non_empty_children: found 3 non-empty in array of length 16
     Added child at 3 -> Inner_node
     count_non_empty_children: found 4 non-empty in array of length 16
       parent size=4 nonempty=4
     count_non_empty_children: found 4 non-empty in array of length 16
     Added child at 4 -> Inner_node
     count_non_empty_children: found 5 non-empty in array of length 16
       parent size=5 nonempty=5
     count_non_empty_children: found 5 non-empty in array of length 16
     Added child at 5 -> Inner_node
     count_non_empty_children: found 6 non-empty in array of length 16
       parent size=6 nonempty=6
     count_non_empty_children: found 6 non-empty in array of length 16
     Added child at 6 -> Inner_node
     count_non_empty_children: found 7 non-empty in array of length 16
       parent size=7 nonempty=7
     count_non_empty_children: found 7 non-empty in array of length 16
     Added child at 7 -> Inner_node
     count_non_empty_children: found 8 non-empty in array of length 16
       parent size=8 nonempty=8
     count_non_empty_children: found 8 non-empty in array of length 16
     Added child at 8 -> Inner_node
     count_non_empty_children: found 9 non-empty in array of length 16
       parent size=9 nonempty=9
     count_non_empty_children: found 9 non-empty in array of length 16
     Added child at 9 -> Inner_node
     count_non_empty_children: found 10 non-empty in array of length 16
       parent size=10 nonempty=10
     count_non_empty_children: found 10 non-empty in array of length 16
     Added child at 10 -> Inner_node
     count_non_empty_children: found 11 non-empty in array of length 16
       parent size=11 nonempty=11
     count_non_empty_children: found 11 non-empty in array of length 16
     Added child at 11 -> Inner_node
     count_non_empty_children: found 12 non-empty in array of length 16
       parent size=12 nonempty=12
     count_non_empty_children: found 12 non-empty in array of length 16
     Added child at 12 -> Inner_node
     count_non_empty_children: found 13 non-empty in array of length 16
       parent size=13 nonempty=13
     count_non_empty_children: found 13 non-empty in array of length 16
     Added child at 13 -> Inner_node
     count_non_empty_children: found 14 non-empty in array of length 16
       parent size=14 nonempty=14
     count_non_empty_children: found 14 non-empty in array of length 16
     Added child at 14 -> Inner_node
     count_non_empty_children: found 15 non-empty in array of length 16
       parent size=15 nonempty=15
     count_non_empty_children: found 15 non-empty in array of length 16
     Added child at 15 -> Inner_node
     count_non_empty_children: found 16 non-empty in array of length 16
       parent size=16 nonempty=16
     count_non_empty_children: found 16 non-empty in array of length 16
     count_non_empty_children: found 0 non-empty in array of length 48
     count_non_empty_children: found 16 non-empty in array of length 48
     count_non_empty_children: found 16 non-empty in array of length 48
       parent size=16 nonempty=16
     count_non_empty_children: found 16 non-empty in array of length 48
     Added child at idx=16 for byte=17
     count_non_empty_children: found 17 non-empty in array of length 48
       parent size=17 nonempty=17
     count_non_empty_children: found 17 non-empty in array of length 48
     Added child at idx=17 for byte=18
     count_non_empty_children: found 18 non-empty in array of length 48
       parent size=18 nonempty=18
     count_non_empty_children: found 18 non-empty in array of length 48
     Added child at idx=18 for byte=19
     count_non_empty_children: found 19 non-empty in array of length 48
       parent size=19 nonempty=19
     count_non_empty_children: found 19 non-empty in array of length 48
     Added child at idx=19 for byte=20
     Grow node16
     Node48Node48Node48Node48Size is 20
     Size of children 48
     BYTE representation :[ \x00]
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
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
     BYTE representation :[ \x00]
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
   [%expect {|
     Leafterminate INPUT: [68,65,31,31,6F]
     terminate OUTPUT: [68,65,31,31,6F,00]
     |}]


let insert_tr (t : tree) key value =
  let new_root = insert_tree t key value in
  { root = new_root; size = t.size + 1 }
let%expect_test "Test multiple insertion and search" =
let tree = empty_tree in
let tree = insert_tr tree [Bytes.of_string "aa"] 1L in
(match search_after_terminating tree.root [Bytes.of_string "aa"] 0 with
| Some v -> Printf.printf "Found: %Ld\n" v
| None -> Printf.printf " Not found\n");
  [%expect {|
    Searching leavesSearch key a compared with a Found: 1
    terminate INPUT: [61]
    terminate OUTPUT: [61,00]
    terminate INPUT: [61]
    terminate OUTPUT: [61,00]
    |}]

let print_node_type  node =
(* After inserting 'e', before searching *)
Printf.eprintf "\n=== DEBUG: Children array contents ===\n";
match node  with
| Inner_node (_, _, _, children) ->
    Array.iteri (fun i child ->
      Printf.eprintf "children[%d]: " i;
      match child with
      | Empty -> Printf.eprintf "Empty\n"
      | Leaf (KeyValue kv) ->
          Printf.eprintf "Leaf with key=%s\n"
            (String.concat "" (List.map Bytes.to_string kv.key))
      | Inner_node _ -> Printf.eprintf "Inner_node\n"
    ) children
| _ -> ();
  match node with
  | Empty -> Printf.printf "Empty\n"
  | Leaf _ -> Printf.printf "Leaf\n"
  | Inner_node (_, node_type, _, _) ->
      (match node_type with
       | Node4 _ -> Printf.printf "Node4\n"
       | Node16 _ -> Printf.printf "Node16\n"
       | Node48 _ -> Printf.printf "Node48\n"
       | Node256 _ -> Printf.printf "Node256\n"
       | Leaf _ -> Printf.printf "Leaf (inner)\n")

(* Test 1: Trigger Node4 → Node16 growth *)
let%expect_test "Node4 to Node16 growth" =
  Printf.printf "=== Test: Node4 → Node16 Growth ===\n";
  let tree = empty_tree in

  (* Insert 5 keys with same prefix to force growth *)
  let keys = ["a"; "b"; "c"; "d"; "e"] in
  let tree = List.fold_left (fun acc key ->
    Printf.printf "Inserting key: %s\n%!" key;
    insert_tr acc [Bytes.of_string key] (Int64.of_int (Char.code key.[0]))
  ) tree keys in

  Printf.printf "Root node type after 5 insertions: ";
  print_node_type tree.root;

  (* Verify all keys are found *)
  Printf.printf "\nVerifying all keys:\n";
  List.iter (fun key ->
    match search_after_terminating tree.root [Bytes.of_string key] 0 with
    | Some v -> Printf.printf "✓ Found '%s' → %Ld\n" key v
    | None -> Printf.printf "✗ Key '%s' NOT FOUND\n" key
  ) keys;
  [%expect {|
    === Test: Node4 → Node16 Growth ===
    Inserting key: a
    Inserting key: b
    terminate INPUT: [61]
    terminate OUTPUT: [61,00]
    terminate INPUT: [62]
    terminate OUTPUT: [62,00]
    INSERT: Leaf case - existing key=a , new key=b 
    INSERT: Splitting leaf, creating Node4
    DEBUG new_node4: array length=4
      [0] = Empty ✓
      [1] = Empty ✓
      [2] = Empty ✓
      [3] = Empty ✓
    count_non_empty_children: found 0 non-empty in array of length 4
    count_non_empty_children: found 0 non-empty in array of length 4
    INSERT: shared prefix length=0
    INSERT: new_level=0
    INSERT: old_key length=2, new_key length=2
    INSERT: old leaf goes at byte=61
    INSERT: new leaf goes at byte=62
    DEBUG: old leaf key=a , new leaf key=b 
    DEBUG: Adding old leaf with key byte=61
    count_non_empty_children: found 0 non-empty in array of length 4
    add_child called: key=61 parent_size=0 nonempty_children=0 keys=[00,00,00,00]
    count_non_empty_children: found 0 non-empty in array of length 4
      parent size=0 nonempty=0
    count_non_empty_children: found 0 non-empty in array of length 4
    Added child at 0 -> 0
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child returned: updated_keys=[61] nonempty_children=1
    DEBUG: Adding new leaf with key byte=62
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child called: key=62 parent_size=1 nonempty_children=1 keys=[61]
    count_non_empty_children: found 1 non-empty in array of length 4
      parent size=1 nonempty=1
    count_non_empty_children: found 1 non-empty in array of length 4
    Added child at 1 -> 1
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child returned: updated_keys=[61,62] nonempty_children=2
    Inserting key: c
    terminate INPUT: [63]
    terminate OUTPUT: [63,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child called: key=63 parent_size=2 nonempty_children=2 keys=[61,62]
    count_non_empty_children: found 2 non-empty in array of length 4
      parent size=2 nonempty=2
    count_non_empty_children: found 2 non-empty in array of length 4
    Added child at 2 -> 2
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child returned: updated_keys=[61,62,63] nonempty_children=3
    Node4/16(2) [  c = a ]Node4/16(2) [  c = b ]Inserting key: d
    terminate INPUT: [64]
    terminate OUTPUT: [64,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child called: key=64 parent_size=3 nonempty_children=3 keys=[61,62,63]
    count_non_empty_children: found 3 non-empty in array of length 4
      parent size=3 nonempty=3
    count_non_empty_children: found 3 non-empty in array of length 4
    Added child at 3 -> 3
    count_non_empty_children: found 4 non-empty in array of length 4
    add_child returned: updated_keys=[61,62,63,64] nonempty_children=4
    Node4/16(3) [  d = a ]Node4/16(3) [  d = b ]Node4/16(3) [  d = c ]Inserting key: e
    terminate INPUT: [65]
    terminate OUTPUT: [65,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    count_non_empty_children: found 4 non-empty in array of length 4
    add_child called: key=65 parent_size=4 nonempty_children=4 keys=[61,62,63,64]
    count_non_empty_children: found 4 non-empty in array of length 4
      parent size=4 nonempty=4
    count_non_empty_children: found 4 non-empty in array of length 4
    count_non_empty_children: found 0 non-empty in array of length 16
    count_non_empty_children: found 4 non-empty in array of length 16
    count_non_empty_children: found 4 non-empty in array of length 16
      parent size=4 nonempty=4
    count_non_empty_children: found 4 non-empty in array of length 16
    Added child at 4 -> Leaf_node
    count_non_empty_children: found 5 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65] nonempty_children=5
    Node4/16(4) [  e = a ]Node4/16(4) [  e = b ]Node4/16(4) [  e = c ]Node4/16(4) [  e = d ]Computed index = � for key byte c
    Computed index = � for key byte d
    Computed index = � for key byte e
    Root node type after 5 insertions:
    Verifying all keys:
    Searching in Node keys: [[ a][ b][ c][ d][ e]]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ a]
    [  ]
    Level 0
    Node4/16(5) [  a = a ]Computed index =   for key byte a
    Search key a compared with a ✓ Found 'a' → 97
    Searching in Node keys: [[ a][ b][ c][ d][ e]]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ b]
    [  ]
    Level 0
    Node4/16(5) [  b = a ]Node4/16(5) [  b = b ]Computed index =  for key byte b
    Search key b compared with b ✓ Found 'b' → 98
    Searching in Node keys: [[ a][ b][ c][ d][ e]]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ c]
    [  ]
    Level 0
    Node4/16(5) [  c = a ]Node4/16(5) [  c = b ]Node4/16(5) [  c = c ]Computed index =  for key byte c
    Search key c compared with c ✓ Found 'c' → 99
    Searching in Node keys: [[ a][ b][ c][ d][ e]]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ d]
    [  ]
    Level 0
    Node4/16(5) [  d = a ]Node4/16(5) [  d = b ]Node4/16(5) [  d = c ]Node4/16(5) [  d = d ]Computed index =  for key byte d
    Search key d compared with d ✓ Found 'd' → 100
    Searching in Node keys: [[ a][ b][ c][ d][ e]]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ e]
    [  ]
    Level 0
    Node4/16(5) [  e = a ]Node4/16(5) [  e = b ]Node4/16(5) [  e = c ]Node4/16(5) [  e = d ]Node4/16(5) [  e = e ]Computed index =  for key byte e
    Search key e compared with e ✓ Found 'e' → 101

    === DEBUG: Children array contents ===
    children[0]: Leaf with key=a 
    children[1]: Leaf with key=b 
    children[2]: Leaf with key=c 
    children[3]: Leaf with key=d 
    children[4]: Leaf with key=e 
    children[5]: Empty
    children[6]: Empty
    children[7]: Empty
    children[8]: Empty
    children[9]: Empty
    children[10]: Empty
    children[11]: Empty
    children[12]: Empty
    children[13]: Empty
    children[14]: Empty
    children[15]: Empty
    terminate INPUT: [61]
    terminate OUTPUT: [61,00]
    find_child Node4/16: idx=0, array_length=16
    find_child Node4/16: returning children[0]
      -> was Leaf
    terminate INPUT: [62]
    terminate OUTPUT: [62,00]
    find_child Node4/16: idx=1, array_length=16
    find_child Node4/16: returning children[1]
      -> was Leaf
    terminate INPUT: [63]
    terminate OUTPUT: [63,00]
    find_child Node4/16: idx=2, array_length=16
    find_child Node4/16: returning children[2]
      -> was Leaf
    terminate INPUT: [64]
    terminate OUTPUT: [64,00]
    find_child Node4/16: idx=3, array_length=16
    find_child Node4/16: returning children[3]
      -> was Leaf
    terminate INPUT: [65]
    terminate OUTPUT: [65,00]
    find_child Node4/16: idx=4, array_length=16
    find_child Node4/16: returning children[4]
      -> was Leaf
    |}]
