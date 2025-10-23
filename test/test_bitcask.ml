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
      [0] = Empty âœ“
      [1] = Empty âœ“
      [2] = Empty âœ“
      [3] = Empty âœ“
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
      [0] = Empty âœ“
      [1] = Empty âœ“
      [2] = Empty âœ“
      [3] = Empty âœ“
    count_non_empty_children: found 0 non-empty in array of length 4
    DEBUG new_node4: array length=4
      [0] = Empty âœ“
      [1] = Empty âœ“
      [2] = Empty âœ“
      [3] = Empty âœ“
    count_non_empty_children: found 0 non-empty in array of length 4
    add_child ENTER
    count_non_empty_children: found 0 non-empty in array of length 4
      parent size=0 nonempty=0
    count_non_empty_children: found 0 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 1 keys):
      [0]: '' (len=1 bytes)
    add_child Node4: storing key byte=01 ('') at idx=0
    Added child at 0 -> 0
    count_non_empty_children: found 1 non-empty in array of length 4
      parent size=1 nonempty=1
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 2 keys):
      [0]: '' (len=1 bytes)
      [1]: '' (len=1 bytes)
    add_child Node4: storing key byte=02 ('') at idx=1
    Added child at 1 -> 1
    count_non_empty_children: found 2 non-empty in array of length 4
      parent size=2 nonempty=2
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 3 keys):
      [0]: '' (len=1 bytes)
      [1]: '' (len=1 bytes)
      [2]: '' (len=1 bytes)
    add_child Node4: storing key byte=03 ('') at idx=2
    Added child at 2 -> 2
    count_non_empty_children: found 3 non-empty in array of length 4
      parent size=3 nonempty=3
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 4 keys):
      [0]: '' (len=1 bytes)
      [1]: '' (len=1 bytes)
      [2]: '' (len=1 bytes)
      [3]: '' (len=1 bytes)
    add_child Node4: storing key byte=04 ('') at idx=3
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
     add_child Node4: storing key byte=01 ('') at idx=0
     Added child at 0 -> Inner_node
     count_non_empty_children: found 1 non-empty in array of length 16
       parent size=1 nonempty=1
     count_non_empty_children: found 1 non-empty in array of length 16
     add_child Node4: storing key byte=02 ('') at idx=1
     Added child at 1 -> Inner_node
     count_non_empty_children: found 2 non-empty in array of length 16
       parent size=2 nonempty=2
     count_non_empty_children: found 2 non-empty in array of length 16
     add_child Node4: storing key byte=03 ('') at idx=2
     Added child at 2 -> Inner_node
     count_non_empty_children: found 3 non-empty in array of length 16
       parent size=3 nonempty=3
     count_non_empty_children: found 3 non-empty in array of length 16
     add_child Node4: storing key byte=04 ('') at idx=3
     Added child at 3 -> Inner_node
     count_non_empty_children: found 4 non-empty in array of length 16
       parent size=4 nonempty=4
     count_non_empty_children: found 4 non-empty in array of length 16
     add_child Node4: storing key byte=05 ('') at idx=4
     Added child at 4 -> Inner_node
     count_non_empty_children: found 5 non-empty in array of length 16
       parent size=5 nonempty=5
     count_non_empty_children: found 5 non-empty in array of length 16
     add_child Node4: storing key byte=06 ('') at idx=5
     Added child at 5 -> Inner_node
     count_non_empty_children: found 6 non-empty in array of length 16
       parent size=6 nonempty=6
     count_non_empty_children: found 6 non-empty in array of length 16
     add_child Node4: storing key byte=07 ('') at idx=6
     Added child at 6 -> Inner_node
     count_non_empty_children: found 7 non-empty in array of length 16
       parent size=7 nonempty=7
     count_non_empty_children: found 7 non-empty in array of length 16
     add_child Node4: storing key byte=08 ('') at idx=7
     Added child at 7 -> Inner_node
     count_non_empty_children: found 8 non-empty in array of length 16
       parent size=8 nonempty=8
     count_non_empty_children: found 8 non-empty in array of length 16
     add_child Node4: storing key byte=09 ('	') at idx=8
     Added child at 8 -> Inner_node
     count_non_empty_children: found 9 non-empty in array of length 16
       parent size=9 nonempty=9
     count_non_empty_children: found 9 non-empty in array of length 16
     add_child Node4: storing key byte=0A ('
     ') at idx=9
     Added child at 9 -> Inner_node
     count_non_empty_children: found 10 non-empty in array of length 16
       parent size=10 nonempty=10
     count_non_empty_children: found 10 non-empty in array of length 16
     add_child Node4: storing key byte=0B ('') at idx=10
     Added child at 10 -> Inner_node
     count_non_empty_children: found 11 non-empty in array of length 16
       parent size=11 nonempty=11
     count_non_empty_children: found 11 non-empty in array of length 16
     add_child Node4: storing key byte=0C ('') at idx=11
     Added child at 11 -> Inner_node
     count_non_empty_children: found 12 non-empty in array of length 16
       parent size=12 nonempty=12
     count_non_empty_children: found 12 non-empty in array of length 16
     add_child Node4: storing key byte=0D ('') at idx=12
     Added child at 12 -> Inner_node
     count_non_empty_children: found 13 non-empty in array of length 16
       parent size=13 nonempty=13
     count_non_empty_children: found 13 non-empty in array of length 16
     add_child Node4: storing key byte=0E ('') at idx=13
     Added child at 13 -> Inner_node
     count_non_empty_children: found 14 non-empty in array of length 16
       parent size=14 nonempty=14
     count_non_empty_children: found 14 non-empty in array of length 16
     add_child Node4: storing key byte=0F ('') at idx=14
     Added child at 14 -> Inner_node
     count_non_empty_children: found 15 non-empty in array of length 16
       parent size=15 nonempty=15
     count_non_empty_children: found 15 non-empty in array of length 16
     add_child Node4: storing key byte=10 ('') at idx=15
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
     add_child Node4: storing key byte=01 ('') at idx=0
     Added child at 0 -> Inner_node
     count_non_empty_children: found 1 non-empty in array of length 16
       parent size=1 nonempty=1
     count_non_empty_children: found 1 non-empty in array of length 16
     add_child Node4: storing key byte=02 ('') at idx=1
     Added child at 1 -> Inner_node
     count_non_empty_children: found 2 non-empty in array of length 16
       parent size=2 nonempty=2
     count_non_empty_children: found 2 non-empty in array of length 16
     add_child Node4: storing key byte=03 ('') at idx=2
     Added child at 2 -> Inner_node
     count_non_empty_children: found 3 non-empty in array of length 16
       parent size=3 nonempty=3
     count_non_empty_children: found 3 non-empty in array of length 16
     add_child Node4: storing key byte=04 ('') at idx=3
     Added child at 3 -> Inner_node
     count_non_empty_children: found 4 non-empty in array of length 16
       parent size=4 nonempty=4
     count_non_empty_children: found 4 non-empty in array of length 16
     add_child Node4: storing key byte=05 ('') at idx=4
     Added child at 4 -> Inner_node
     count_non_empty_children: found 5 non-empty in array of length 16
       parent size=5 nonempty=5
     count_non_empty_children: found 5 non-empty in array of length 16
     add_child Node4: storing key byte=06 ('') at idx=5
     Added child at 5 -> Inner_node
     count_non_empty_children: found 6 non-empty in array of length 16
       parent size=6 nonempty=6
     count_non_empty_children: found 6 non-empty in array of length 16
     add_child Node4: storing key byte=07 ('') at idx=6
     Added child at 6 -> Inner_node
     count_non_empty_children: found 7 non-empty in array of length 16
       parent size=7 nonempty=7
     count_non_empty_children: found 7 non-empty in array of length 16
     add_child Node4: storing key byte=08 ('') at idx=7
     Added child at 7 -> Inner_node
     count_non_empty_children: found 8 non-empty in array of length 16
       parent size=8 nonempty=8
     count_non_empty_children: found 8 non-empty in array of length 16
     add_child Node4: storing key byte=09 ('	') at idx=8
     Added child at 8 -> Inner_node
     count_non_empty_children: found 9 non-empty in array of length 16
       parent size=9 nonempty=9
     count_non_empty_children: found 9 non-empty in array of length 16
     add_child Node4: storing key byte=0A ('
     ') at idx=9
     Added child at 9 -> Inner_node
     count_non_empty_children: found 10 non-empty in array of length 16
       parent size=10 nonempty=10
     count_non_empty_children: found 10 non-empty in array of length 16
     add_child Node4: storing key byte=0B ('') at idx=10
     Added child at 10 -> Inner_node
     count_non_empty_children: found 11 non-empty in array of length 16
       parent size=11 nonempty=11
     count_non_empty_children: found 11 non-empty in array of length 16
     add_child Node4: storing key byte=0C ('') at idx=11
     Added child at 11 -> Inner_node
     count_non_empty_children: found 12 non-empty in array of length 16
       parent size=12 nonempty=12
     count_non_empty_children: found 12 non-empty in array of length 16
     add_child Node4: storing key byte=0D ('') at idx=12
     Added child at 12 -> Inner_node
     count_non_empty_children: found 13 non-empty in array of length 16
       parent size=13 nonempty=13
     count_non_empty_children: found 13 non-empty in array of length 16
     add_child Node4: storing key byte=0E ('') at idx=13
     Added child at 13 -> Inner_node
     count_non_empty_children: found 14 non-empty in array of length 16
       parent size=14 nonempty=14
     count_non_empty_children: found 14 non-empty in array of length 16
     add_child Node4: storing key byte=0F ('') at idx=14
     Added child at 14 -> Inner_node
     count_non_empty_children: found 15 non-empty in array of length 16
       parent size=15 nonempty=15
     count_non_empty_children: found 15 non-empty in array of length 16
     add_child Node4: storing key byte=10 ('') at idx=15
     Added child at 15 -> Inner_node
     count_non_empty_children: found 16 non-empty in array of length 16
       parent size=16 nonempty=16
     count_non_empty_children: found 16 non-empty in array of length 16
     count_non_empty_children: found 0 non-empty in array of length 48
     count_non_empty_children: found 16 non-empty in array of length 48
     count_non_empty_children: found 16 non-empty in array of length 48
       parent size=16 nonempty=16
     count_non_empty_children: found 16 non-empty in array of length 48
     add_child Node48: storing mapping for byte=11 ('')
     Added child at idx=16 for byte=17
     count_non_empty_children: found 17 non-empty in array of length 48
       parent size=17 nonempty=17
     count_non_empty_children: found 17 non-empty in array of length 48
     add_child Node48: storing mapping for byte=12 ('')
     Added child at idx=17 for byte=18
     count_non_empty_children: found 18 non-empty in array of length 48
       parent size=18 nonempty=18
     count_non_empty_children: found 18 non-empty in array of length 48
     add_child Node48: storing mapping for byte=13 ('')
     Added child at idx=18 for byte=19
     count_non_empty_children: found 19 non-empty in array of length 48
       parent size=19 nonempty=19
     count_non_empty_children: found 19 non-empty in array of length 48
     add_child Node48: storing mapping for byte=14 ('')
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

let%expect_test "Node4 to Node16 growth" =
  let tree = empty_tree in

  let keys = ["a"; "b"; "c"; "d"; "e"] in
  let tree = List.fold_left (fun acc key ->
    Printf.printf "Inserting key: %s\n%!" key;
    insert_tr acc [Bytes.of_string key] (Int64.of_int (Char.code key.[0]))
  ) tree keys in

  Printf.printf "Root node type after 5 insertions: ";
  print_node_type tree.root;

  List.iter (fun key ->
    match search_after_terminating tree.root [Bytes.of_string key] 0 with
    | Some v -> Printf.printf " Found '%s'  %Ld\n" key v
    | None -> Printf.printf " Key '%s' NOT FOUND\n" key
  ) keys;
  [%expect {|
    Inserting key: a
    Inserting key: b
    terminate INPUT: [61]
    terminate OUTPUT: [61,00]
    terminate INPUT: [62]
    terminate OUTPUT: [62,00]
    INSERT: Leaf case - existing key=a , new key=b 
    INSERT: Splitting leaf, creating Node4
    DEBUG: level=0, limit=0, new_level will be=0
    DEBUG new_node4: array length=4
      [0] = Empty âœ“
      [1] = Empty âœ“
      [2] = Empty âœ“
      [3] = Empty âœ“
    count_non_empty_children: found 0 non-empty in array of length 4
    count_non_empty_children: found 0 non-empty in array of length 4
    INSERT: shared prefix length=0
    INSERT: new_level=0
    INSERT: old_key length=2, new_key length=2
    INSERT: old leaf goes at byte=61
    INSERT: new leaf goes at byte=62
    DEBUG: old leaf key=a , new leaf key=b 
    DEBUG: Adding old leaf with key byte=61
    INSERT Leaf split: new_level=0, old_key_byte='a' (byte=61), new_key_byte='b' (byte=62)
    count_non_empty_children: found 0 non-empty in array of length 4
    add_child called: key=61 parent_size=0 nonempty_children=0 keys=[00,00,00,00]
    count_non_empty_children: found 0 non-empty in array of length 4
      parent size=0 nonempty=0
    count_non_empty_children: found 0 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 1 keys):
      [0]: 'a' (len=1 bytes)
    add_child Node4: storing key byte=61 ('a') at idx=0
    Added child at 0 -> 0
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child returned: updated_keys=[61] nonempty_children=1
    DEBUG: Adding new leaf with key byte=62
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child called: key=62 parent_size=1 nonempty_children=1 keys=[61]
    count_non_empty_children: found 1 non-empty in array of length 4
      parent size=1 nonempty=1
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 2 keys):
      [0]: 'a' (len=1 bytes)
      [1]: 'b' (len=1 bytes)
    add_child Node4: storing key byte=62 ('b') at idx=1
    Added child at 1 -> 1
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child returned: updated_keys=[61,62] nonempty_children=2
    Inserting key: c
    terminate INPUT: [63]
    terminate OUTPUT: [63,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'c' (first byte=63)
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child called: key=63 parent_size=2 nonempty_children=2 keys=[61,62]
    count_non_empty_children: found 2 non-empty in array of length 4
      parent size=2 nonempty=2
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 3 keys):
      [0]: 'a' (len=1 bytes)
      [1]: 'b' (len=1 bytes)
      [2]: 'c' (len=1 bytes)
    add_child Node4: storing key byte=63 ('c') at idx=2
    Added child at 2 -> 2
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child returned: updated_keys=[61,62,63] nonempty_children=3
    [  c = a ][  c = b ]Inserting key: d
    terminate INPUT: [64]
    terminate OUTPUT: [64,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'd' (first byte=64)
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child called: key=64 parent_size=3 nonempty_children=3 keys=[61,62,63]
    count_non_empty_children: found 3 non-empty in array of length 4
      parent size=3 nonempty=3
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 4 keys):
      [0]: 'a' (len=1 bytes)
      [1]: 'b' (len=1 bytes)
      [2]: 'c' (len=1 bytes)
      [3]: 'd' (len=1 bytes)
    add_child Node4: storing key byte=64 ('d') at idx=3
    Added child at 3 -> 3
    count_non_empty_children: found 4 non-empty in array of length 4
    add_child returned: updated_keys=[61,62,63,64] nonempty_children=4
    [  d = a ][  d = b ][  d = c ]Inserting key: e
    terminate INPUT: [65]
    terminate OUTPUT: [65,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'e' (first byte=65)
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
    add_child Node4: storing key byte=65 ('e') at idx=4
    Added child at 4 -> Leaf_node
    count_non_empty_children: found 5 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65] nonempty_children=5
    [  e = a ][  e = b ][  e = c ][  e = d ]Computed index = ÿ for key byte c
    Computed index = ÿ for key byte d
    Computed index = ÿ for key byte e
    Root node type after 5 insertions: Searching in Node keys: [BYTE representation :[ a]
    BYTE representation :[ b]
    BYTE representation :[ c]
    BYTE representation :[ d]
    BYTE representation :[ e]
    ]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ a]
    [  ]
    Level 0
    [  a = a ]Computed index =   for key byte a
    search: find_child returned: Leaf
    Search key a compared with a  Found 'a'  97
    Searching in Node keys: [BYTE representation :[ a]
    BYTE representation :[ b]
    BYTE representation :[ c]
    BYTE representation :[ d]
    BYTE representation :[ e]
    ]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ b]
    [  ]
    Level 0
    [  b = a ][  b = b ]Computed index =  for key byte b
    search: find_child returned: Leaf
    Search key b compared with b  Found 'b'  98
    Searching in Node keys: [BYTE representation :[ a]
    BYTE representation :[ b]
    BYTE representation :[ c]
    BYTE representation :[ d]
    BYTE representation :[ e]
    ]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ c]
    [  ]
    Level 0
    [  c = a ][  c = b ][  c = c ]Computed index =  for key byte c
    search: find_child returned: Leaf
    Search key c compared with c  Found 'c'  99
    Searching in Node keys: [BYTE representation :[ a]
    BYTE representation :[ b]
    BYTE representation :[ c]
    BYTE representation :[ d]
    BYTE representation :[ e]
    ]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ d]
    [  ]
    Level 0
    [  d = a ][  d = b ][  d = c ][  d = d ]Computed index =  for key byte d
    search: find_child returned: Leaf
    Search key d compared with d  Found 'd'  100
    Searching in Node keys: [BYTE representation :[ a]
    BYTE representation :[ b]
    BYTE representation :[ c]
    BYTE representation :[ d]
    BYTE representation :[ e]
    ]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ e]
    [  ]
    Level 0
    [  e = a ][  e = b ][  e = c ][  e = d ][  e = e ]Computed index =  for key byte e
    search: find_child returned: Leaf
    Search key e compared with e  Found 'e'  101

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
let%expect_test "Node16 to Node48 growth" =
  let tree = empty_tree in

  (* Insert 17 keys to trigger Node4 â†’ Node16 â†’ Node48 *)
  let keys = List.init 17 (fun i ->
    String.make 1 (Char.chr (97 + i))  (* a-q *)
  ) in

  let tree = List.fold_left (fun acc key ->
    insert_tr acc [Bytes.of_string key] (Int64.of_int (Char.code key.[0]))
  ) tree keys in

  Printf.printf "Root node type after 17 insertions: ";
  print_node_type tree.root;

  (* Verify random sample *)
  Printf.printf "\nVerifying sample keys:\n";
  List.iter (fun key ->
    match search_after_terminating tree.root [Bytes.of_string key] 0 with
    | Some v -> Printf.printf " Found '%s'  %Ld\n" key v
    | None -> Printf.printf " Key '%s' NOT FOUND\n" key
  ) ["a"; "h"; "q"];
  [%expect {|
    terminate INPUT: [61]
    terminate OUTPUT: [61,00]
    terminate INPUT: [62]
    terminate OUTPUT: [62,00]
    INSERT: Leaf case - existing key=a , new key=b 
    INSERT: Splitting leaf, creating Node4
    DEBUG: level=0, limit=0, new_level will be=0
    DEBUG new_node4: array length=4
      [0] = Empty âœ“
      [1] = Empty âœ“
      [2] = Empty âœ“
      [3] = Empty âœ“
    count_non_empty_children: found 0 non-empty in array of length 4
    count_non_empty_children: found 0 non-empty in array of length 4
    INSERT: shared prefix length=0
    INSERT: new_level=0
    INSERT: old_key length=2, new_key length=2
    INSERT: old leaf goes at byte=61
    INSERT: new leaf goes at byte=62
    DEBUG: old leaf key=a , new leaf key=b 
    DEBUG: Adding old leaf with key byte=61
    INSERT Leaf split: new_level=0, old_key_byte='a' (byte=61), new_key_byte='b' (byte=62)
    count_non_empty_children: found 0 non-empty in array of length 4
    add_child called: key=61 parent_size=0 nonempty_children=0 keys=[00,00,00,00]
    count_non_empty_children: found 0 non-empty in array of length 4
      parent size=0 nonempty=0
    count_non_empty_children: found 0 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 1 keys):
      [0]: 'a' (len=1 bytes)
    add_child Node4: storing key byte=61 ('a') at idx=0
    Added child at 0 -> 0
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child returned: updated_keys=[61] nonempty_children=1
    DEBUG: Adding new leaf with key byte=62
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child called: key=62 parent_size=1 nonempty_children=1 keys=[61]
    count_non_empty_children: found 1 non-empty in array of length 4
      parent size=1 nonempty=1
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 2 keys):
      [0]: 'a' (len=1 bytes)
      [1]: 'b' (len=1 bytes)
    add_child Node4: storing key byte=62 ('b') at idx=1
    Added child at 1 -> 1
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child returned: updated_keys=[61,62] nonempty_children=2
    terminate INPUT: [63]
    terminate OUTPUT: [63,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'c' (first byte=63)
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child called: key=63 parent_size=2 nonempty_children=2 keys=[61,62]
    count_non_empty_children: found 2 non-empty in array of length 4
      parent size=2 nonempty=2
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 3 keys):
      [0]: 'a' (len=1 bytes)
      [1]: 'b' (len=1 bytes)
      [2]: 'c' (len=1 bytes)
    add_child Node4: storing key byte=63 ('c') at idx=2
    Added child at 2 -> 2
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child returned: updated_keys=[61,62,63] nonempty_children=3
    terminate INPUT: [64]
    terminate OUTPUT: [64,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'd' (first byte=64)
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child called: key=64 parent_size=3 nonempty_children=3 keys=[61,62,63]
    count_non_empty_children: found 3 non-empty in array of length 4
      parent size=3 nonempty=3
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 4 keys):
      [0]: 'a' (len=1 bytes)
      [1]: 'b' (len=1 bytes)
      [2]: 'c' (len=1 bytes)
      [3]: 'd' (len=1 bytes)
    add_child Node4: storing key byte=64 ('d') at idx=3
    Added child at 3 -> 3
    count_non_empty_children: found 4 non-empty in array of length 4
    add_child returned: updated_keys=[61,62,63,64] nonempty_children=4
    terminate INPUT: [65]
    terminate OUTPUT: [65,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'e' (first byte=65)
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
    add_child Node4: storing key byte=65 ('e') at idx=4
    Added child at 4 -> Leaf_node
    count_non_empty_children: found 5 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65] nonempty_children=5
    terminate INPUT: [66]
    terminate OUTPUT: [66,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'f' (first byte=66)
    count_non_empty_children: found 5 non-empty in array of length 16
    add_child called: key=66 parent_size=5 nonempty_children=5 keys=[61,62,63,64,65]
    count_non_empty_children: found 5 non-empty in array of length 16
      parent size=5 nonempty=5
    count_non_empty_children: found 5 non-empty in array of length 16
    add_child Node4: storing key byte=66 ('f') at idx=5
    Added child at 5 -> Leaf_node
    count_non_empty_children: found 6 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66] nonempty_children=6
    terminate INPUT: [67]
    terminate OUTPUT: [67,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'g' (first byte=67)
    count_non_empty_children: found 6 non-empty in array of length 16
    add_child called: key=67 parent_size=6 nonempty_children=6 keys=[61,62,63,64,65,66]
    count_non_empty_children: found 6 non-empty in array of length 16
      parent size=6 nonempty=6
    count_non_empty_children: found 6 non-empty in array of length 16
    add_child Node4: storing key byte=67 ('g') at idx=6
    Added child at 6 -> Leaf_node
    count_non_empty_children: found 7 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67] nonempty_children=7
    terminate INPUT: [68]
    terminate OUTPUT: [68,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'h' (first byte=68)
    count_non_empty_children: found 7 non-empty in array of length 16
    add_child called: key=68 parent_size=7 nonempty_children=7 keys=[61,62,63,64,65,66,67]
    count_non_empty_children: found 7 non-empty in array of length 16
      parent size=7 nonempty=7
    count_non_empty_children: found 7 non-empty in array of length 16
    add_child Node4: storing key byte=68 ('h') at idx=7
    Added child at 7 -> Leaf_node
    count_non_empty_children: found 8 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68] nonempty_children=8
    terminate INPUT: [69]
    terminate OUTPUT: [69,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'i' (first byte=69)
    count_non_empty_children: found 8 non-empty in array of length 16
    add_child called: key=69 parent_size=8 nonempty_children=8 keys=[61,62,63,64,65,66,67,68]
    count_non_empty_children: found 8 non-empty in array of length 16
      parent size=8 nonempty=8
    count_non_empty_children: found 8 non-empty in array of length 16
    add_child Node4: storing key byte=69 ('i') at idx=8
    Added child at 8 -> Leaf_node
    count_non_empty_children: found 9 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69] nonempty_children=9
    terminate INPUT: [6A]
    terminate OUTPUT: [6A,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'j' (first byte=6A)
    count_non_empty_children: found 9 non-empty in array of length 16
    add_child called: key=6A parent_size=9 nonempty_children=9 keys=[61,62,63,64,65,66,67,68,69]
    count_non_empty_children: found 9 non-empty in array of length 16
      parent size=9 nonempty=9
    count_non_empty_children: found 9 non-empty in array of length 16
    add_child Node4: storing key byte=6A ('j') at idx=9
    Added child at 9 -> Leaf_node
    count_non_empty_children: found 10 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69,6A] nonempty_children=10
    terminate INPUT: [6B]
    terminate OUTPUT: [6B,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'k' (first byte=6B)
    count_non_empty_children: found 10 non-empty in array of length 16
    add_child called: key=6B parent_size=10 nonempty_children=10 keys=[61,62,63,64,65,66,67,68,69,6A]
    count_non_empty_children: found 10 non-empty in array of length 16
      parent size=10 nonempty=10
    count_non_empty_children: found 10 non-empty in array of length 16
    add_child Node4: storing key byte=6B ('k') at idx=10
    Added child at 10 -> Leaf_node
    count_non_empty_children: found 11 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69,6A,6B] nonempty_children=11
    terminate INPUT: [6C]
    terminate OUTPUT: [6C,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'l' (first byte=6C)
    count_non_empty_children: found 11 non-empty in array of length 16
    add_child called: key=6C parent_size=11 nonempty_children=11 keys=[61,62,63,64,65,66,67,68,69,6A,6B]
    count_non_empty_children: found 11 non-empty in array of length 16
      parent size=11 nonempty=11
    count_non_empty_children: found 11 non-empty in array of length 16
    add_child Node4: storing key byte=6C ('l') at idx=11
    Added child at 11 -> Leaf_node
    count_non_empty_children: found 12 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C] nonempty_children=12
    terminate INPUT: [6D]
    terminate OUTPUT: [6D,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'm' (first byte=6D)
    count_non_empty_children: found 12 non-empty in array of length 16
    add_child called: key=6D parent_size=12 nonempty_children=12 keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C]
    count_non_empty_children: found 12 non-empty in array of length 16
      parent size=12 nonempty=12
    count_non_empty_children: found 12 non-empty in array of length 16
    add_child Node4: storing key byte=6D ('m') at idx=12
    Added child at 12 -> Leaf_node
    count_non_empty_children: found 13 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D] nonempty_children=13
    terminate INPUT: [6E]
    terminate OUTPUT: [6E,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'n' (first byte=6E)
    count_non_empty_children: found 13 non-empty in array of length 16
    add_child called: key=6E parent_size=13 nonempty_children=13 keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D]
    count_non_empty_children: found 13 non-empty in array of length 16
      parent size=13 nonempty=13
    count_non_empty_children: found 13 non-empty in array of length 16
    add_child Node4: storing key byte=6E ('n') at idx=13
    Added child at 13 -> Leaf_node
    count_non_empty_children: found 14 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D,6E] nonempty_children=14
    terminate INPUT: [6F]
    terminate OUTPUT: [6F,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'o' (first byte=6F)
    count_non_empty_children: found 14 non-empty in array of length 16
    add_child called: key=6F parent_size=14 nonempty_children=14 keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D,6E]
    count_non_empty_children: found 14 non-empty in array of length 16
      parent size=14 nonempty=14
    count_non_empty_children: found 14 non-empty in array of length 16
    add_child Node4: storing key byte=6F ('o') at idx=14
    Added child at 14 -> Leaf_node
    count_non_empty_children: found 15 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D,6E,6F] nonempty_children=15
    terminate INPUT: [70]
    terminate OUTPUT: [70,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'p' (first byte=70)
    count_non_empty_children: found 15 non-empty in array of length 16
    add_child called: key=70 parent_size=15 nonempty_children=15 keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D,6E,6F]
    count_non_empty_children: found 15 non-empty in array of length 16
      parent size=15 nonempty=15
    count_non_empty_children: found 15 non-empty in array of length 16
    add_child Node4: storing key byte=70 ('p') at idx=15
    Added child at 15 -> Leaf_node
    count_non_empty_children: found 16 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D,6E,6F,70] nonempty_children=16
    terminate INPUT: [71]
    terminate OUTPUT: [71,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'q' (first byte=71)
    count_non_empty_children: found 16 non-empty in array of length 16
    add_child called: key=71 parent_size=16 nonempty_children=16 keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D,6E,6F,70]
    count_non_empty_children: found 16 non-empty in array of length 16
      parent size=16 nonempty=16
    count_non_empty_children: found 16 non-empty in array of length 16
    count_non_empty_children: found 0 non-empty in array of length 48
    count_non_empty_children: found 16 non-empty in array of length 48
    count_non_empty_children: found 16 non-empty in array of length 48
      parent size=16 nonempty=16
    count_non_empty_children: found 16 non-empty in array of length 48
    add_child Node48: storing mapping for byte=71 ('q')
    Added child at idx=16 for byte=113
    count_non_empty_children: found 17 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=17
    [  c = a ][  c = b ][  d = a ][  d = b ][  d = c ][  e = a ][  e = b ][  e = c ][  e = d ]Computed index = ÿ for key byte c
    Computed index = ÿ for key byte d
    Computed index = ÿ for key byte e
    [  f = a ][  f = b ][  f = c ][  f = d ][  f = e ]Computed index = ÿ for key byte f
    [  g = a ][  g = b ][  g = c ][  g = d ][  g = e ][  g = f ]Computed index = ÿ for key byte g
    [  h = a ][  h = b ][  h = c ][  h = d ][  h = e ][  h = f ][  h = g ]Computed index = ÿ for key byte h
    [  i = a ][  i = b ][  i = c ][  i = d ][  i = e ][  i = f ][  i = g ][  i = h ]Computed index = ÿ for key byte i
    [  j = a ][  j = b ][  j = c ][  j = d ][  j = e ][  j = f ][  j = g ][  j = h ][  j = i ]Computed index = ÿ for key byte j
    [  k = a ][  k = b ][  k = c ][  k = d ][  k = e ][  k = f ][  k = g ][  k = h ][  k = i ][  k = j ]Computed index = ÿ for key byte k
    [  l = a ][  l = b ][  l = c ][  l = d ][  l = e ][  l = f ][  l = g ][  l = h ][  l = i ][  l = j ][  l = k ]Computed index = ÿ for key byte l
    [  m = a ][  m = b ][  m = c ][  m = d ][  m = e ][  m = f ][  m = g ][  m = h ][  m = i ][  m = j ][  m = k ][  m = l ]Computed index = ÿ for key byte m
    [  n = a ][  n = b ][  n = c ][  n = d ][  n = e ][  n = f ][  n = g ][  n = h ][  n = i ][  n = j ][  n = k ][  n = l ][  n = m ]Computed index = ÿ for key byte n
    [  o = a ][  o = b ][  o = c ][  o = d ][  o = e ][  o = f ][  o = g ][  o = h ][  o = i ][  o = j ][  o = k ][  o = l ][  o = m ][  o = n ]Computed index = ÿ for key byte o
    [  p = a ][  p = b ][  p = c ][  p = d ][  p = e ][  p = f ][  p = g ][  p = h ][  p = i ][  p = j ][  p = k ][  p = l ][  p = m ][  p = n ][  p = o ]Computed index = ÿ for key byte p
    [  q = a ][  q = b ][  q = c ][  q = d ][  q = e ][  q = f ][  q = g ][  q = h ][  q = i ][  q = j ][  q = k ][  q = l ][  q = m ][  q = n ][  q = o ][  q = p ]Computed index = ÿ for key byte q
    Grow node16
    Node48Root node type after 17 insertions:
    Verifying sample keys:
    Searching in Node keys: [BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ 	]
    BYTE representation :[
    ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    ]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ a]
    [  ]
    Level 0
    Node48 hit for 61 â†’ child 0
    Computed index =   for key byte a
    search: find_child returned: Leaf
    Search key a compared with a  Found 'a'  97
    Searching in Node keys: [BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ 	]
    BYTE representation :[
    ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    ]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ h]
    [  ]
    Level 0
    Node48 hit for 68 â†’ child 7
    Computed index =  for key byte h
    search: find_child returned: Leaf
    Search key h compared with h  Found 'h'  104
    Searching in Node keys: [BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ 	]
    BYTE representation :[
    ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[ ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    BYTE representation :[  ]
    ]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ q]
    [  ]
    Level 0
    Node48 hit for 71 â†’ child 16
    Computed index =  for key byte q
    search: find_child returned: Leaf
    Search key q compared with q  Found 'q'  113

    === DEBUG: Children array contents ===
    children[0]: Leaf with key=a 
    children[1]: Leaf with key=b 
    children[2]: Leaf with key=c 
    children[3]: Leaf with key=d 
    children[4]: Leaf with key=e 
    children[5]: Leaf with key=f 
    children[6]: Leaf with key=g 
    children[7]: Leaf with key=h 
    children[8]: Leaf with key=i 
    children[9]: Leaf with key=j 
    children[10]: Leaf with key=k 
    children[11]: Leaf with key=l 
    children[12]: Leaf with key=m 
    children[13]: Leaf with key=n 
    children[14]: Leaf with key=o 
    children[15]: Leaf with key=p 
    children[16]: Leaf with key=q 
    children[17]: Empty
    children[18]: Empty
    children[19]: Empty
    children[20]: Empty
    children[21]: Empty
    children[22]: Empty
    children[23]: Empty
    children[24]: Empty
    children[25]: Empty
    children[26]: Empty
    children[27]: Empty
    children[28]: Empty
    children[29]: Empty
    children[30]: Empty
    children[31]: Empty
    children[32]: Empty
    children[33]: Empty
    children[34]: Empty
    children[35]: Empty
    children[36]: Empty
    children[37]: Empty
    children[38]: Empty
    children[39]: Empty
    children[40]: Empty
    children[41]: Empty
    children[42]: Empty
    children[43]: Empty
    children[44]: Empty
    children[45]: Empty
    children[46]: Empty
    children[47]: Empty
    terminate INPUT: [61]
    terminate OUTPUT: [61,00]
    find_child Node48: idx=0
    find_child Node48: real_idx=0
    terminate INPUT: [68]
    terminate OUTPUT: [68,00]
    find_child Node48: idx=7
    find_child Node48: real_idx=7
    terminate INPUT: [71]
    terminate OUTPUT: [71,00]
    find_child Node48: idx=16
    find_child Node48: real_idx=16
    |}]

let%expect_test "Node48 to Node256 growth" =
  let tree = empty_tree in

  (* Insert 49 keys to trigger full growth chain *)
  let keys = List.init 49 (fun i ->
    if i < 26 then
      String.make 1 (Char.chr (97 + i))  (* a-z *)
    else
      String.make 1 (Char.chr (65 + (i - 26)))  (* A-W *)
  ) in

  let tree = List.fold_left (fun acc key ->
    insert_tr acc [Bytes.of_string key] (Int64.of_int (Char.code key.[0]))
  ) tree keys in

  Printf.printf "Root node type after 49 insertions: ";
  print_node_type tree.root;

  Printf.printf "\nVerifying sample keys:\n";
  List.iter (fun key ->
    match search_after_terminating tree.root [Bytes.of_string key] 0 with
    | Some v -> Printf.printf " Found '%s' %Ld\n" key v
    | None -> Printf.printf " Key '%s' NOT FOUND\n" key
  ) ["a"; "m"; "z"; "A"; "M"; "W"];
  [%expect {|
    terminate INPUT: [61]
    terminate OUTPUT: [61,00]
    terminate INPUT: [62]
    terminate OUTPUT: [62,00]
    INSERT: Leaf case - existing key=a , new key=b 
    INSERT: Splitting leaf, creating Node4
    DEBUG: level=0, limit=0, new_level will be=0
    DEBUG new_node4: array length=4
      [0] = Empty âœ“
      [1] = Empty âœ“
      [2] = Empty âœ“
      [3] = Empty âœ“
    count_non_empty_children: found 0 non-empty in array of length 4
    count_non_empty_children: found 0 non-empty in array of length 4
    INSERT: shared prefix length=0
    INSERT: new_level=0
    INSERT: old_key length=2, new_key length=2
    INSERT: old leaf goes at byte=61
    INSERT: new leaf goes at byte=62
    DEBUG: old leaf key=a , new leaf key=b 
    DEBUG: Adding old leaf with key byte=61
    INSERT Leaf split: new_level=0, old_key_byte='a' (byte=61), new_key_byte='b' (byte=62)
    count_non_empty_children: found 0 non-empty in array of length 4
    add_child called: key=61 parent_size=0 nonempty_children=0 keys=[00,00,00,00]
    count_non_empty_children: found 0 non-empty in array of length 4
      parent size=0 nonempty=0
    count_non_empty_children: found 0 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 1 keys):
      [0]: 'a' (len=1 bytes)
    add_child Node4: storing key byte=61 ('a') at idx=0
    Added child at 0 -> 0
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child returned: updated_keys=[61] nonempty_children=1
    DEBUG: Adding new leaf with key byte=62
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child called: key=62 parent_size=1 nonempty_children=1 keys=[61]
    count_non_empty_children: found 1 non-empty in array of length 4
      parent size=1 nonempty=1
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 2 keys):
      [0]: 'a' (len=1 bytes)
      [1]: 'b' (len=1 bytes)
    add_child Node4: storing key byte=62 ('b') at idx=1
    Added child at 1 -> 1
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child returned: updated_keys=[61,62] nonempty_children=2
    terminate INPUT: [63]
    terminate OUTPUT: [63,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'c' (first byte=63)
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child called: key=63 parent_size=2 nonempty_children=2 keys=[61,62]
    count_non_empty_children: found 2 non-empty in array of length 4
      parent size=2 nonempty=2
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 3 keys):
      [0]: 'a' (len=1 bytes)
      [1]: 'b' (len=1 bytes)
      [2]: 'c' (len=1 bytes)
    add_child Node4: storing key byte=63 ('c') at idx=2
    Added child at 2 -> 2
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child returned: updated_keys=[61,62,63] nonempty_children=3
    terminate INPUT: [64]
    terminate OUTPUT: [64,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'd' (first byte=64)
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child called: key=64 parent_size=3 nonempty_children=3 keys=[61,62,63]
    count_non_empty_children: found 3 non-empty in array of length 4
      parent size=3 nonempty=3
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 4 keys):
      [0]: 'a' (len=1 bytes)
      [1]: 'b' (len=1 bytes)
      [2]: 'c' (len=1 bytes)
      [3]: 'd' (len=1 bytes)
    add_child Node4: storing key byte=64 ('d') at idx=3
    Added child at 3 -> 3
    count_non_empty_children: found 4 non-empty in array of length 4
    add_child returned: updated_keys=[61,62,63,64] nonempty_children=4
    terminate INPUT: [65]
    terminate OUTPUT: [65,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'e' (first byte=65)
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
    add_child Node4: storing key byte=65 ('e') at idx=4
    Added child at 4 -> Leaf_node
    count_non_empty_children: found 5 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65] nonempty_children=5
    terminate INPUT: [66]
    terminate OUTPUT: [66,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'f' (first byte=66)
    count_non_empty_children: found 5 non-empty in array of length 16
    add_child called: key=66 parent_size=5 nonempty_children=5 keys=[61,62,63,64,65]
    count_non_empty_children: found 5 non-empty in array of length 16
      parent size=5 nonempty=5
    count_non_empty_children: found 5 non-empty in array of length 16
    add_child Node4: storing key byte=66 ('f') at idx=5
    Added child at 5 -> Leaf_node
    count_non_empty_children: found 6 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66] nonempty_children=6
    terminate INPUT: [67]
    terminate OUTPUT: [67,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'g' (first byte=67)
    count_non_empty_children: found 6 non-empty in array of length 16
    add_child called: key=67 parent_size=6 nonempty_children=6 keys=[61,62,63,64,65,66]
    count_non_empty_children: found 6 non-empty in array of length 16
      parent size=6 nonempty=6
    count_non_empty_children: found 6 non-empty in array of length 16
    add_child Node4: storing key byte=67 ('g') at idx=6
    Added child at 6 -> Leaf_node
    count_non_empty_children: found 7 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67] nonempty_children=7
    terminate INPUT: [68]
    terminate OUTPUT: [68,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'h' (first byte=68)
    count_non_empty_children: found 7 non-empty in array of length 16
    add_child called: key=68 parent_size=7 nonempty_children=7 keys=[61,62,63,64,65,66,67]
    count_non_empty_children: found 7 non-empty in array of length 16
      parent size=7 nonempty=7
    count_non_empty_children: found 7 non-empty in array of length 16
    add_child Node4: storing key byte=68 ('h') at idx=7
    Added child at 7 -> Leaf_node
    count_non_empty_children: found 8 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68] nonempty_children=8
    terminate INPUT: [69]
    terminate OUTPUT: [69,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'i' (first byte=69)
    count_non_empty_children: found 8 non-empty in array of length 16
    add_child called: key=69 parent_size=8 nonempty_children=8 keys=[61,62,63,64,65,66,67,68]
    count_non_empty_children: found 8 non-empty in array of length 16
      parent size=8 nonempty=8
    count_non_empty_children: found 8 non-empty in array of length 16
    add_child Node4: storing key byte=69 ('i') at idx=8
    Added child at 8 -> Leaf_node
    count_non_empty_children: found 9 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69] nonempty_children=9
    terminate INPUT: [6A]
    terminate OUTPUT: [6A,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'j' (first byte=6A)
    count_non_empty_children: found 9 non-empty in array of length 16
    add_child called: key=6A parent_size=9 nonempty_children=9 keys=[61,62,63,64,65,66,67,68,69]
    count_non_empty_children: found 9 non-empty in array of length 16
      parent size=9 nonempty=9
    count_non_empty_children: found 9 non-empty in array of length 16
    add_child Node4: storing key byte=6A ('j') at idx=9
    Added child at 9 -> Leaf_node
    count_non_empty_children: found 10 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69,6A] nonempty_children=10
    terminate INPUT: [6B]
    terminate OUTPUT: [6B,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'k' (first byte=6B)
    count_non_empty_children: found 10 non-empty in array of length 16
    add_child called: key=6B parent_size=10 nonempty_children=10 keys=[61,62,63,64,65,66,67,68,69,6A]
    count_non_empty_children: found 10 non-empty in array of length 16
      parent size=10 nonempty=10
    count_non_empty_children: found 10 non-empty in array of length 16
    add_child Node4: storing key byte=6B ('k') at idx=10
    Added child at 10 -> Leaf_node
    count_non_empty_children: found 11 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69,6A,6B] nonempty_children=11
    terminate INPUT: [6C]
    terminate OUTPUT: [6C,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'l' (first byte=6C)
    count_non_empty_children: found 11 non-empty in array of length 16
    add_child called: key=6C parent_size=11 nonempty_children=11 keys=[61,62,63,64,65,66,67,68,69,6A,6B]
    count_non_empty_children: found 11 non-empty in array of length 16
      parent size=11 nonempty=11
    count_non_empty_children: found 11 non-empty in array of length 16
    add_child Node4: storing key byte=6C ('l') at idx=11
    Added child at 11 -> Leaf_node
    count_non_empty_children: found 12 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C] nonempty_children=12
    terminate INPUT: [6D]
    terminate OUTPUT: [6D,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'm' (first byte=6D)
    count_non_empty_children: found 12 non-empty in array of length 16
    add_child called: key=6D parent_size=12 nonempty_children=12 keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C]
    count_non_empty_children: found 12 non-empty in array of length 16
      parent size=12 nonempty=12
    count_non_empty_children: found 12 non-empty in array of length 16
    add_child Node4: storing key byte=6D ('m') at idx=12
    Added child at 12 -> Leaf_node
    count_non_empty_children: found 13 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D] nonempty_children=13
    terminate INPUT: [6E]
    terminate OUTPUT: [6E,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'n' (first byte=6E)
    count_non_empty_children: found 13 non-empty in array of length 16
    add_child called: key=6E parent_size=13 nonempty_children=13 keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D]
    count_non_empty_children: found 13 non-empty in array of length 16
      parent size=13 nonempty=13
    count_non_empty_children: found 13 non-empty in array of length 16
    add_child Node4: storing key byte=6E ('n') at idx=13
    Added child at 13 -> Leaf_node
    count_non_empty_children: found 14 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D,6E] nonempty_children=14
    terminate INPUT: [6F]
    terminate OUTPUT: [6F,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'o' (first byte=6F)
    count_non_empty_children: found 14 non-empty in array of length 16
    add_child called: key=6F parent_size=14 nonempty_children=14 keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D,6E]
    count_non_empty_children: found 14 non-empty in array of length 16
      parent size=14 nonempty=14
    count_non_empty_children: found 14 non-empty in array of length 16
    add_child Node4: storing key byte=6F ('o') at idx=14
    Added child at 14 -> Leaf_node
    count_non_empty_children: found 15 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D,6E,6F] nonempty_children=15
    terminate INPUT: [70]
    terminate OUTPUT: [70,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'p' (first byte=70)
    count_non_empty_children: found 15 non-empty in array of length 16
    add_child called: key=70 parent_size=15 nonempty_children=15 keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D,6E,6F]
    count_non_empty_children: found 15 non-empty in array of length 16
      parent size=15 nonempty=15
    count_non_empty_children: found 15 non-empty in array of length 16
    add_child Node4: storing key byte=70 ('p') at idx=15
    Added child at 15 -> Leaf_node
    count_non_empty_children: found 16 non-empty in array of length 16
    add_child returned: updated_keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D,6E,6F,70] nonempty_children=16
    terminate INPUT: [71]
    terminate OUTPUT: [71,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'q' (first byte=71)
    count_non_empty_children: found 16 non-empty in array of length 16
    add_child called: key=71 parent_size=16 nonempty_children=16 keys=[61,62,63,64,65,66,67,68,69,6A,6B,6C,6D,6E,6F,70]
    count_non_empty_children: found 16 non-empty in array of length 16
      parent size=16 nonempty=16
    count_non_empty_children: found 16 non-empty in array of length 16
    count_non_empty_children: found 0 non-empty in array of length 48
    count_non_empty_children: found 16 non-empty in array of length 48
    count_non_empty_children: found 16 non-empty in array of length 48
      parent size=16 nonempty=16
    count_non_empty_children: found 16 non-empty in array of length 48
    add_child Node48: storing mapping for byte=71 ('q')
    Added child at idx=16 for byte=113
    count_non_empty_children: found 17 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=17
    === Test: Node48 â†’ Node256 Growth ===
    [  c = a ][  c = b ][  d = a ][  d = b ][  d = c ][  e = a ][  e = b ][  e = c ][  e = d ]Computed index = ÿ for key byte c
    Computed index = ÿ for key byte d
    Computed index = ÿ for key byte e
    [  f = a ][  f = b ][  f = c ][  f = d ][  f = e ]Computed index = ÿ for key byte f
    [  g = a ][  g = b ][  g = c ][  g = d ][  g = e ][  g = f ]Computed index = ÿ for key byte g
    [  h = a ][  h = b ][  h = c ][  h = d ][  h = e ][  h = f ][  h = g ]Computed index = ÿ for key byte h
    [  i = a ][  i = b ][  i = c ][  i = d ][  i = e ][  i = f ][  i = g ][  i = h ]Computed index = ÿ for key byte i
    [  j = a ][  j = b ][  j = c ][  j = d ][  j = e ][  j = f ][  j = g ][  j = h ][  j = i ]Computed index = ÿ for key byte j
    [  k = a ][  k = b ][  k = c ][  k = d ][  k = e ][  k = f ][  k = g ][  k = h ][  k = i ][  k = j ]Computed index = ÿ for key byte k
    [  l = a ][  l = b ][  l = c ][  l = d ][  l = e ][  l = f ][  l = g ][  l = h ][  l = i ][  l = j ][  l = k ]Computed index = ÿ for key byte l
    [  m = a ][  m = b ][  m = c ][  m = d ][  m = e ][  m = f ][  m = g ][  m = h ][  m = i ][  m = j ][  m = k ][  m = l ]Computed index = ÿ for key byte m
    [  n = a ][  n = b ][  n = c ][  n = d ][  n = e ][  n = f ][  n = g ][  n = h ][  n = i ][  n = j ][  n = k ][  n = l ][  n = m ]Computed index = ÿ for key byte n
    [  o = a ][  o = b ][  o = c ][  o = d ][  o = e ][  o = f ][  o = g ][  o = h ][  o = i ][  o = j ][  o = k ][  o = l ][  o = m ][  o = n ]Computed index = ÿ for key byte o
    [  p = a ][  p = b ][  p = c ][  p = d ][  p = e ][  p = f ][  p = g ][  p = h ][  p = i ][  p = j ][  p = k ][  p = l ][  p = m ][  p = n ][  p = o ]Computed index = ÿ for key byte p
    [  q = a ][  q = b ][  q = c ][  q = d ][  q = e ][  q = f ][  q = g ][  q = h ][  q = i ][  q = j ][  q = k ][  q = l ][  q = m ][  q = n ][  q = o ][  q = p ]Computed index = ÿ for key byte q
    Grow node16
    Node48Node48 miss for 72
    terminate INPUT: [72]
    terminate OUTPUT: [72,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'r' (first byte=72)
    count_non_empty_children: found 17 non-empty in array of length 48
    add_child called: key=72 parent_size=17 nonempty_children=17 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 17 non-empty in array of length 48
      parent size=17 nonempty=17
    count_non_empty_children: found 17 non-empty in array of length 48
    add_child Node48: storing mapping for byte=72 ('r')
    Added child at idx=17 for byte=114
    count_non_empty_children: found 18 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=18
    Computed index = ÿ for key byte r
    Node48Node48 miss for 73
    terminate INPUT: [73]
    terminate OUTPUT: [73,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 's' (first byte=73)
    count_non_empty_children: found 18 non-empty in array of length 48
    add_child called: key=73 parent_size=18 nonempty_children=18 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 18 non-empty in array of length 48
      parent size=18 nonempty=18
    count_non_empty_children: found 18 non-empty in array of length 48
    add_child Node48: storing mapping for byte=73 ('s')
    Added child at idx=18 for byte=115
    count_non_empty_children: found 19 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=19
    Computed index = ÿ for key byte s
    Node48Node48 miss for 74
    terminate INPUT: [74]
    terminate OUTPUT: [74,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 't' (first byte=74)
    count_non_empty_children: found 19 non-empty in array of length 48
    add_child called: key=74 parent_size=19 nonempty_children=19 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 19 non-empty in array of length 48
      parent size=19 nonempty=19
    count_non_empty_children: found 19 non-empty in array of length 48
    add_child Node48: storing mapping for byte=74 ('t')
    Added child at idx=19 for byte=116
    count_non_empty_children: found 20 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=20
    Computed index = ÿ for key byte t
    Node48Node48 miss for 75
    terminate INPUT: [75]
    terminate OUTPUT: [75,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'u' (first byte=75)
    count_non_empty_children: found 20 non-empty in array of length 48
    add_child called: key=75 parent_size=20 nonempty_children=20 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 20 non-empty in array of length 48
      parent size=20 nonempty=20
    count_non_empty_children: found 20 non-empty in array of length 48
    add_child Node48: storing mapping for byte=75 ('u')
    Added child at idx=20 for byte=117
    count_non_empty_children: found 21 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=21
    Computed index = ÿ for key byte u
    Node48Node48 miss for 76
    terminate INPUT: [76]
    terminate OUTPUT: [76,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'v' (first byte=76)
    count_non_empty_children: found 21 non-empty in array of length 48
    add_child called: key=76 parent_size=21 nonempty_children=21 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 21 non-empty in array of length 48
      parent size=21 nonempty=21
    count_non_empty_children: found 21 non-empty in array of length 48
    add_child Node48: storing mapping for byte=76 ('v')
    Added child at idx=21 for byte=118
    count_non_empty_children: found 22 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=22
    Computed index = ÿ for key byte v
    Node48Node48 miss for 77
    terminate INPUT: [77]
    terminate OUTPUT: [77,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'w' (first byte=77)
    count_non_empty_children: found 22 non-empty in array of length 48
    add_child called: key=77 parent_size=22 nonempty_children=22 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 22 non-empty in array of length 48
      parent size=22 nonempty=22
    count_non_empty_children: found 22 non-empty in array of length 48
    add_child Node48: storing mapping for byte=77 ('w')
    Added child at idx=22 for byte=119
    count_non_empty_children: found 23 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=23
    Computed index = ÿ for key byte w
    Node48Node48 miss for 78
    terminate INPUT: [78]
    terminate OUTPUT: [78,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'x' (first byte=78)
    count_non_empty_children: found 23 non-empty in array of length 48
    add_child called: key=78 parent_size=23 nonempty_children=23 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 23 non-empty in array of length 48
      parent size=23 nonempty=23
    count_non_empty_children: found 23 non-empty in array of length 48
    add_child Node48: storing mapping for byte=78 ('x')
    Added child at idx=23 for byte=120
    count_non_empty_children: found 24 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=24
    Computed index = ÿ for key byte x
    Node48Node48 miss for 79
    terminate INPUT: [79]
    terminate OUTPUT: [79,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'y' (first byte=79)
    count_non_empty_children: found 24 non-empty in array of length 48
    add_child called: key=79 parent_size=24 nonempty_children=24 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 24 non-empty in array of length 48
      parent size=24 nonempty=24
    count_non_empty_children: found 24 non-empty in array of length 48
    add_child Node48: storing mapping for byte=79 ('y')
    Added child at idx=24 for byte=121
    count_non_empty_children: found 25 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=25
    Computed index = ÿ for key byte y
    Node48Node48 miss for 7A
    terminate INPUT: [7A]
    terminate OUTPUT: [7A,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'z' (first byte=7A)
    count_non_empty_children: found 25 non-empty in array of length 48
    add_child called: key=7A parent_size=25 nonempty_children=25 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 25 non-empty in array of length 48
      parent size=25 nonempty=25
    count_non_empty_children: found 25 non-empty in array of length 48
    add_child Node48: storing mapping for byte=7A ('z')
    Added child at idx=25 for byte=122
    count_non_empty_children: found 26 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=26
    Computed index = ÿ for key byte z
    Node48Node48 miss for 41
    terminate INPUT: [41]
    terminate OUTPUT: [41,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'A' (first byte=41)
    count_non_empty_children: found 26 non-empty in array of length 48
    add_child called: key=41 parent_size=26 nonempty_children=26 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 26 non-empty in array of length 48
      parent size=26 nonempty=26
    count_non_empty_children: found 26 non-empty in array of length 48
    add_child Node48: storing mapping for byte=41 ('A')
    Added child at idx=26 for byte=65
    count_non_empty_children: found 27 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=27
    Computed index = ÿ for key byte A
    Node48Node48 miss for 42
    terminate INPUT: [42]
    terminate OUTPUT: [42,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'B' (first byte=42)
    count_non_empty_children: found 27 non-empty in array of length 48
    add_child called: key=42 parent_size=27 nonempty_children=27 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 27 non-empty in array of length 48
      parent size=27 nonempty=27
    count_non_empty_children: found 27 non-empty in array of length 48
    add_child Node48: storing mapping for byte=42 ('B')
    Added child at idx=27 for byte=66
    count_non_empty_children: found 28 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=28
    Computed index = ÿ for key byte B
    Node48Node48 miss for 43
    terminate INPUT: [43]
    terminate OUTPUT: [43,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'C' (first byte=43)
    count_non_empty_children: found 28 non-empty in array of length 48
    add_child called: key=43 parent_size=28 nonempty_children=28 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 28 non-empty in array of length 48
      parent size=28 nonempty=28
    count_non_empty_children: found 28 non-empty in array of length 48
    add_child Node48: storing mapping for byte=43 ('C')
    Added child at idx=28 for byte=67
    count_non_empty_children: found 29 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=29
    Computed index = ÿ for key byte C
    Node48Node48 miss for 44
    terminate INPUT: [44]
    terminate OUTPUT: [44,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'D' (first byte=44)
    count_non_empty_children: found 29 non-empty in array of length 48
    add_child called: key=44 parent_size=29 nonempty_children=29 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 29 non-empty in array of length 48
      parent size=29 nonempty=29
    count_non_empty_children: found 29 non-empty in array of length 48
    add_child Node48: storing mapping for byte=44 ('D')
    Added child at idx=29 for byte=68
    count_non_empty_children: found 30 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=30
    Computed index = ÿ for key byte D
    Node48Node48 miss for 45
    terminate INPUT: [45]
    terminate OUTPUT: [45,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'E' (first byte=45)
    count_non_empty_children: found 30 non-empty in array of length 48
    add_child called: key=45 parent_size=30 nonempty_children=30 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 30 non-empty in array of length 48
      parent size=30 nonempty=30
    count_non_empty_children: found 30 non-empty in array of length 48
    add_child Node48: storing mapping for byte=45 ('E')
    Added child at idx=30 for byte=69
    count_non_empty_children: found 31 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=31
    Computed index = ÿ for key byte E
    Node48Node48 miss for 46
    terminate INPUT: [46]
    terminate OUTPUT: [46,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'F' (first byte=46)
    count_non_empty_children: found 31 non-empty in array of length 48
    add_child called: key=46 parent_size=31 nonempty_children=31 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 31 non-empty in array of length 48
      parent size=31 nonempty=31
    count_non_empty_children: found 31 non-empty in array of length 48
    add_child Node48: storing mapping for byte=46 ('F')
    Added child at idx=31 for byte=70
    count_non_empty_children: found 32 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=32
    Computed index = ÿ for key byte F
    Node48Node48 miss for 47
    terminate INPUT: [47]
    terminate OUTPUT: [47,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'G' (first byte=47)
    count_non_empty_children: found 32 non-empty in array of length 48
    add_child called: key=47 parent_size=32 nonempty_children=32 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 32 non-empty in array of length 48
      parent size=32 nonempty=32
    count_non_empty_children: found 32 non-empty in array of length 48
    add_child Node48: storing mapping for byte=47 ('G')
    Added child at idx=32 for byte=71
    count_non_empty_children: found 33 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=33
    Computed index = ÿ for key byte G
    Node48Node48 miss for 48
    terminate INPUT: [48]
    terminate OUTPUT: [48,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'H' (first byte=48)
    count_non_empty_children: found 33 non-empty in array of length 48
    add_child called: key=48 parent_size=33 nonempty_children=33 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 33 non-empty in array of length 48
      parent size=33 nonempty=33
    count_non_empty_children: found 33 non-empty in array of length 48
    add_child Node48: storing mapping for byte=48 ('H')
    Added child at idx=33 for byte=72
    count_non_empty_children: found 34 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=34
    Computed index = ÿ for key byte H
    Node48Node48 miss for 49
    terminate INPUT: [49]
    terminate OUTPUT: [49,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'I' (first byte=49)
    count_non_empty_children: found 34 non-empty in array of length 48
    add_child called: key=49 parent_size=34 nonempty_children=34 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 34 non-empty in array of length 48
      parent size=34 nonempty=34
    count_non_empty_children: found 34 non-empty in array of length 48
    add_child Node48: storing mapping for byte=49 ('I')
    Added child at idx=34 for byte=73
    count_non_empty_children: found 35 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=35
    Computed index = ÿ for key byte I
    Node48Node48 miss for 4A
    terminate INPUT: [4A]
    terminate OUTPUT: [4A,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'J' (first byte=4A)
    count_non_empty_children: found 35 non-empty in array of length 48
    add_child called: key=4A parent_size=35 nonempty_children=35 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 35 non-empty in array of length 48
      parent size=35 nonempty=35
    count_non_empty_children: found 35 non-empty in array of length 48
    add_child Node48: storing mapping for byte=4A ('J')
    Added child at idx=35 for byte=74
    count_non_empty_children: found 36 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=36
    Computed index = ÿ for key byte J
    Node48Node48 miss for 4B
    terminate INPUT: [4B]
    terminate OUTPUT: [4B,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'K' (first byte=4B)
    count_non_empty_children: found 36 non-empty in array of length 48
    add_child called: key=4B parent_size=36 nonempty_children=36 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 36 non-empty in array of length 48
      parent size=36 nonempty=36
    count_non_empty_children: found 36 non-empty in array of length 48
    add_child Node48: storing mapping for byte=4B ('K')
    Added child at idx=36 for byte=75
    count_non_empty_children: found 37 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=37
    Computed index = ÿ for key byte K
    Node48Node48 miss for 4C
    terminate INPUT: [4C]
    terminate OUTPUT: [4C,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'L' (first byte=4C)
    count_non_empty_children: found 37 non-empty in array of length 48
    add_child called: key=4C parent_size=37 nonempty_children=37 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 37 non-empty in array of length 48
      parent size=37 nonempty=37
    count_non_empty_children: found 37 non-empty in array of length 48
    add_child Node48: storing mapping for byte=4C ('L')
    Added child at idx=37 for byte=76
    count_non_empty_children: found 38 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=38
    Computed index = ÿ for key byte L
    Node48Node48 miss for 4D
    terminate INPUT: [4D]
    terminate OUTPUT: [4D,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'M' (first byte=4D)
    count_non_empty_children: found 38 non-empty in array of length 48
    add_child called: key=4D parent_size=38 nonempty_children=38 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 38 non-empty in array of length 48
      parent size=38 nonempty=38
    count_non_empty_children: found 38 non-empty in array of length 48
    add_child Node48: storing mapping for byte=4D ('M')
    Added child at idx=38 for byte=77
    count_non_empty_children: found 39 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=39
    Computed index = ÿ for key byte M
    Node48Node48 miss for 4E
    terminate INPUT: [4E]
    terminate OUTPUT: [4E,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'N' (first byte=4E)
    count_non_empty_children: found 39 non-empty in array of length 48
    add_child called: key=4E parent_size=39 nonempty_children=39 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 39 non-empty in array of length 48
      parent size=39 nonempty=39
    count_non_empty_children: found 39 non-empty in array of length 48
    add_child Node48: storing mapping for byte=4E ('N')
    Added child at idx=39 for byte=78
    count_non_empty_children: found 40 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=40
    Computed index = ÿ for key byte N
    Node48Node48 miss for 4F
    terminate INPUT: [4F]
    terminate OUTPUT: [4F,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'O' (first byte=4F)
    count_non_empty_children: found 40 non-empty in array of length 48
    add_child called: key=4F parent_size=40 nonempty_children=40 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 40 non-empty in array of length 48
      parent size=40 nonempty=40
    count_non_empty_children: found 40 non-empty in array of length 48
    add_child Node48: storing mapping for byte=4F ('O')
    Added child at idx=40 for byte=79
    count_non_empty_children: found 41 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=41
    Computed index = ÿ for key byte O
    Node48Node48 miss for 50
    terminate INPUT: [50]
    terminate OUTPUT: [50,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'P' (first byte=50)
    count_non_empty_children: found 41 non-empty in array of length 48
    add_child called: key=50 parent_size=41 nonempty_children=41 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 41 non-empty in array of length 48
      parent size=41 nonempty=41
    count_non_empty_children: found 41 non-empty in array of length 48
    add_child Node48: storing mapping for byte=50 ('P')
    Added child at idx=41 for byte=80
    count_non_empty_children: found 42 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,2A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=42
    Computed index = ÿ for key byte P
    Node48Node48 miss for 51
    terminate INPUT: [51]
    terminate OUTPUT: [51,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'Q' (first byte=51)
    count_non_empty_children: found 42 non-empty in array of length 48
    add_child called: key=51 parent_size=42 nonempty_children=42 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,2A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 42 non-empty in array of length 48
      parent size=42 nonempty=42
    count_non_empty_children: found 42 non-empty in array of length 48
    add_child Node48: storing mapping for byte=51 ('Q')
    Added child at idx=42 for byte=81
    count_non_empty_children: found 43 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,2A,2B,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=43
    Computed index = ÿ for key byte Q
    Node48Node48 miss for 52
    terminate INPUT: [52]
    terminate OUTPUT: [52,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'R' (first byte=52)
    count_non_empty_children: found 43 non-empty in array of length 48
    add_child called: key=52 parent_size=43 nonempty_children=43 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,2A,2B,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 43 non-empty in array of length 48
      parent size=43 nonempty=43
    count_non_empty_children: found 43 non-empty in array of length 48
    add_child Node48: storing mapping for byte=52 ('R')
    Added child at idx=43 for byte=82
    count_non_empty_children: found 44 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,2A,2B,2C,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=44
    Computed index = ÿ for key byte R
    Node48Node48 miss for 53
    terminate INPUT: [53]
    terminate OUTPUT: [53,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'S' (first byte=53)
    count_non_empty_children: found 44 non-empty in array of length 48
    add_child called: key=53 parent_size=44 nonempty_children=44 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,2A,2B,2C,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 44 non-empty in array of length 48
      parent size=44 nonempty=44
    count_non_empty_children: found 44 non-empty in array of length 48
    add_child Node48: storing mapping for byte=53 ('S')
    Added child at idx=44 for byte=83
    count_non_empty_children: found 45 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,2A,2B,2C,2D,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=45
    Computed index = ÿ for key byte S
    Node48Node48 miss for 54
    terminate INPUT: [54]
    terminate OUTPUT: [54,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'T' (first byte=54)
    count_non_empty_children: found 45 non-empty in array of length 48
    add_child called: key=54 parent_size=45 nonempty_children=45 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,2A,2B,2C,2D,00,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 45 non-empty in array of length 48
      parent size=45 nonempty=45
    count_non_empty_children: found 45 non-empty in array of length 48
    add_child Node48: storing mapping for byte=54 ('T')
    Added child at idx=45 for byte=84
    count_non_empty_children: found 46 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,2A,2B,2C,2D,2E,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=46
    Computed index = ÿ for key byte T
    Node48Node48 miss for 55
    terminate INPUT: [55]
    terminate OUTPUT: [55,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'U' (first byte=55)
    count_non_empty_children: found 46 non-empty in array of length 48
    add_child called: key=55 parent_size=46 nonempty_children=46 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,2A,2B,2C,2D,2E,00,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 46 non-empty in array of length 48
      parent size=46 nonempty=46
    count_non_empty_children: found 46 non-empty in array of length 48
    add_child Node48: storing mapping for byte=55 ('U')
    Added child at idx=46 for byte=85
    count_non_empty_children: found 47 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,2A,2B,2C,2D,2E,2F,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=47
    Computed index = ÿ for key byte U
    Node48Node48 miss for 56
    terminate INPUT: [56]
    terminate OUTPUT: [56,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'V' (first byte=56)
    count_non_empty_children: found 47 non-empty in array of length 48
    add_child called: key=56 parent_size=47 nonempty_children=47 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,2A,2B,2C,2D,2E,2F,00,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 47 non-empty in array of length 48
      parent size=47 nonempty=47
    count_non_empty_children: found 47 non-empty in array of length 48
    add_child Node48: storing mapping for byte=56 ('V')
    Added child at idx=47 for byte=86
    count_non_empty_children: found 48 non-empty in array of length 48
    add_child returned: updated_keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,2A,2B,2C,2D,2E,2F,30,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00] nonempty_children=48
    Computed index = ÿ for key byte V
    Node48Node48 miss for 57
    terminate INPUT: [57]
    terminate OUTPUT: [57,00]
    find_child Node48: idx=255
    INSERT Inner_node: level=0, List.nth key level = 'W' (first byte=57)
    count_non_empty_children: found 48 non-empty in array of length 48
    add_child called: key=57 parent_size=48 nonempty_children=48 keys=[00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,1B,1C,1D,1E,1F,20,21,22,23,24,25,26,27,28,29,2A,2B,2C,2D,2E,2F,30,00,00,00,00,00,00,00,00,00,00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F,10,11,12,13,14,15,16,17,18,19,1A,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00]
    count_non_empty_children: found 48 non-empty in array of length 48
      parent size=48 nonempty=48
    count_non_empty_children: found 48 non-empty in array of length 48
    count_non_empty_children: found 0 non-empty in array of length 256
    count_non_empty_children: found 48 non-empty in array of length 256
    count_non_empty_children: found 48 non-empty in array of length 256
      parent size=48 nonempty=48
    count_non_empty_children: found 48 non-empty in array of length 256
    add_child Node48: storing mapping for byte=57 ('W')
    count_non_empty_children: found 49 non-empty in array of length 256
    add_child returned: updated_keys=[] nonempty_children=49
    Computed index = ÿ for key byte W
    Grow node48
    Node256Root node type after 49 insertions:
    Verifying sample keys:
    Searching in Node keys: []
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ a]
    [  ]
    Level 0
    Node256 [  a  ]Computed index = a for key byte a
    search: find_child returned: Leaf
    Search key a compared with a  Found 'a' 97
    Searching in Node keys: []
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ m]
    [  ]
    Level 0
    Node256 [  m  ]Computed index = m for key byte m
    search: find_child returned: Leaf
    Search key m compared with m  Found 'm' 109
    Searching in Node keys: []
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ z]
    [  ]
    Level 0
    Node256 [  z  ]Computed index = z for key byte z
    search: find_child returned: Leaf
    Search key z compared with z  Found 'z' 122
    Searching in Node keys: []
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ A]
    [  ]
    Level 0
    Node256 [  A  ]Computed index = A for key byte A
    search: find_child returned: Leaf
    Search key A compared with A  Found 'A' 65
    Searching in Node keys: []
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ M]
    [  ]
    Level 0
    Node256 [  M  ]Computed index = M for key byte M
    search: find_child returned: Leaf
    Search key M compared with M  Found 'M' 77
    Searching in Node keys: []
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 2
     [ W]
    [  ]
    Level 0
    Node256 [  W  ]Computed index = W for key byte W
    search: find_child returned: Leaf
    Search key W compared with W  Found 'W' 87

    === DEBUG: Children array contents ===
    children[0]: Empty
    children[1]: Empty
    children[2]: Empty
    children[3]: Empty
    children[4]: Empty
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
    children[16]: Empty
    children[17]: Empty
    children[18]: Empty
    children[19]: Empty
    children[20]: Empty
    children[21]: Empty
    children[22]: Empty
    children[23]: Empty
    children[24]: Empty
    children[25]: Empty
    children[26]: Empty
    children[27]: Empty
    children[28]: Empty
    children[29]: Empty
    children[30]: Empty
    children[31]: Empty
    children[32]: Empty
    children[33]: Empty
    children[34]: Empty
    children[35]: Empty
    children[36]: Empty
    children[37]: Empty
    children[38]: Empty
    children[39]: Empty
    children[40]: Empty
    children[41]: Empty
    children[42]: Empty
    children[43]: Empty
    children[44]: Empty
    children[45]: Empty
    children[46]: Empty
    children[47]: Empty
    children[48]: Empty
    children[49]: Empty
    children[50]: Empty
    children[51]: Empty
    children[52]: Empty
    children[53]: Empty
    children[54]: Empty
    children[55]: Empty
    children[56]: Empty
    children[57]: Empty
    children[58]: Empty
    children[59]: Empty
    children[60]: Empty
    children[61]: Empty
    children[62]: Empty
    children[63]: Empty
    children[64]: Empty
    children[65]: Leaf with key=A 
    children[66]: Leaf with key=B 
    children[67]: Leaf with key=C 
    children[68]: Leaf with key=D 
    children[69]: Leaf with key=E 
    children[70]: Leaf with key=F 
    children[71]: Leaf with key=G 
    children[72]: Leaf with key=H 
    children[73]: Leaf with key=I 
    children[74]: Leaf with key=J 
    children[75]: Leaf with key=K 
    children[76]: Leaf with key=L 
    children[77]: Leaf with key=M 
    children[78]: Leaf with key=N 
    children[79]: Leaf with key=O 
    children[80]: Leaf with key=P 
    children[81]: Leaf with key=Q 
    children[82]: Leaf with key=R 
    children[83]: Leaf with key=S 
    children[84]: Leaf with key=T 
    children[85]: Leaf with key=U 
    children[86]: Leaf with key=V 
    children[87]: Leaf with key=W 
    children[88]: Empty
    children[89]: Empty
    children[90]: Empty
    children[91]: Empty
    children[92]: Empty
    children[93]: Empty
    children[94]: Empty
    children[95]: Empty
    children[96]: Empty
    children[97]: Leaf with key=a 
    children[98]: Leaf with key=b 
    children[99]: Leaf with key=c 
    children[100]: Leaf with key=d 
    children[101]: Leaf with key=e 
    children[102]: Leaf with key=f 
    children[103]: Leaf with key=g 
    children[104]: Leaf with key=h 
    children[105]: Leaf with key=i 
    children[106]: Leaf with key=j 
    children[107]: Leaf with key=k 
    children[108]: Leaf with key=l 
    children[109]: Leaf with key=m 
    children[110]: Leaf with key=n 
    children[111]: Leaf with key=o 
    children[112]: Leaf with key=p 
    children[113]: Leaf with key=q 
    children[114]: Leaf with key=r 
    children[115]: Leaf with key=s 
    children[116]: Leaf with key=t 
    children[117]: Leaf with key=u 
    children[118]: Leaf with key=v 
    children[119]: Leaf with key=w 
    children[120]: Leaf with key=x 
    children[121]: Leaf with key=y 
    children[122]: Leaf with key=z 
    children[123]: Empty
    children[124]: Empty
    children[125]: Empty
    children[126]: Empty
    children[127]: Empty
    children[128]: Empty
    children[129]: Empty
    children[130]: Empty
    children[131]: Empty
    children[132]: Empty
    children[133]: Empty
    children[134]: Empty
    children[135]: Empty
    children[136]: Empty
    children[137]: Empty
    children[138]: Empty
    children[139]: Empty
    children[140]: Empty
    children[141]: Empty
    children[142]: Empty
    children[143]: Empty
    children[144]: Empty
    children[145]: Empty
    children[146]: Empty
    children[147]: Empty
    children[148]: Empty
    children[149]: Empty
    children[150]: Empty
    children[151]: Empty
    children[152]: Empty
    children[153]: Empty
    children[154]: Empty
    children[155]: Empty
    children[156]: Empty
    children[157]: Empty
    children[158]: Empty
    children[159]: Empty
    children[160]: Empty
    children[161]: Empty
    children[162]: Empty
    children[163]: Empty
    children[164]: Empty
    children[165]: Empty
    children[166]: Empty
    children[167]: Empty
    children[168]: Empty
    children[169]: Empty
    children[170]: Empty
    children[171]: Empty
    children[172]: Empty
    children[173]: Empty
    children[174]: Empty
    children[175]: Empty
    children[176]: Empty
    children[177]: Empty
    children[178]: Empty
    children[179]: Empty
    children[180]: Empty
    children[181]: Empty
    children[182]: Empty
    children[183]: Empty
    children[184]: Empty
    children[185]: Empty
    children[186]: Empty
    children[187]: Empty
    children[188]: Empty
    children[189]: Empty
    children[190]: Empty
    children[191]: Empty
    children[192]: Empty
    children[193]: Empty
    children[194]: Empty
    children[195]: Empty
    children[196]: Empty
    children[197]: Empty
    children[198]: Empty
    children[199]: Empty
    children[200]: Empty
    children[201]: Empty
    children[202]: Empty
    children[203]: Empty
    children[204]: Empty
    children[205]: Empty
    children[206]: Empty
    children[207]: Empty
    children[208]: Empty
    children[209]: Empty
    children[210]: Empty
    children[211]: Empty
    children[212]: Empty
    children[213]: Empty
    children[214]: Empty
    children[215]: Empty
    children[216]: Empty
    children[217]: Empty
    children[218]: Empty
    children[219]: Empty
    children[220]: Empty
    children[221]: Empty
    children[222]: Empty
    children[223]: Empty
    children[224]: Empty
    children[225]: Empty
    children[226]: Empty
    children[227]: Empty
    children[228]: Empty
    children[229]: Empty
    children[230]: Empty
    children[231]: Empty
    children[232]: Empty
    children[233]: Empty
    children[234]: Empty
    children[235]: Empty
    children[236]: Empty
    children[237]: Empty
    children[238]: Empty
    children[239]: Empty
    children[240]: Empty
    children[241]: Empty
    children[242]: Empty
    children[243]: Empty
    children[244]: Empty
    children[245]: Empty
    children[246]: Empty
    children[247]: Empty
    children[248]: Empty
    children[249]: Empty
    children[250]: Empty
    children[251]: Empty
    children[252]: Empty
    children[253]: Empty
    children[254]: Empty
    children[255]: Empty
    terminate INPUT: [61]
    terminate OUTPUT: [61,00]
    find_child Node256: using key byte directly
    terminate INPUT: [6D]
    terminate OUTPUT: [6D,00]
    find_child Node256: using key byte directly
    terminate INPUT: [7A]
    terminate OUTPUT: [7A,00]
    find_child Node256: using key byte directly
    terminate INPUT: [41]
    terminate OUTPUT: [41,00]
    find_child Node256: using key byte directly
    terminate INPUT: [4D]
    terminate OUTPUT: [4D,00]
    find_child Node256: using key byte directly
    terminate INPUT: [57]
    terminate OUTPUT: [57,00]
    find_child Node256: using key byte directly
    |}]

let%expect_test "Test prefixes" =
  let tree = empty_tree in
  let keys = [
  	[Bytes.of_string "hello";Bytes.of_string"world"];
	[Bytes.of_string "yo";Bytes.of_string"earth"];
	[Bytes.of_string "yolo";Bytes.of_string"earth"];
	[Bytes.of_string "yol";Bytes.of_string"earth"];
	[Bytes.of_string "yoli";Bytes.of_string"earth"];
	[Bytes.of_string "yopo";Bytes.of_string"earth"];
  ] in

  let tree = List.fold_left (fun acc key ->
    let key_str = String.concat "" (List.map Bytes.to_string key) in
    Printf.printf "Inserting: %s\n%!" key_str;
    insert_tr acc key (Int64.of_int (String.length key_str))
  ) tree keys in

  Printf.printf "\nRoot node type: ";
  print_node_type tree.root;

  (* Verify all multi-byte keys *)
  List.iter (fun key ->
    let key_str = String.concat "" (List.map Bytes.to_string key) in
    match search_after_terminating tree.root key 0 with
    | Some v -> Printf.printf " Found '%s'  %Ld\n" key_str v
    | None -> Printf.printf " Key '%s' NOT FOUND\n" key_str
  ) keys;
  [%expect {|
    Inserting: helloworld
    Inserting: yoearth
    terminate INPUT: [68,77]
    terminate OUTPUT: [68,77,00]
    terminate INPUT: [79,65]
    terminate OUTPUT: [79,65,00]
    INSERT: Leaf case - existing key=helloworld , new key=yoearth 
    INSERT: Splitting leaf, creating Node4
    DEBUG: level=0, limit=0, new_level will be=0
    DEBUG new_node4: array length=4
      [0] = Empty âœ“
      [1] = Empty âœ“
      [2] = Empty âœ“
      [3] = Empty âœ“
    count_non_empty_children: found 0 non-empty in array of length 4
    count_non_empty_children: found 0 non-empty in array of length 4
    INSERT: shared prefix length=0
    INSERT: new_level=0
    INSERT: old_key length=3, new_key length=3
    INSERT: old leaf goes at byte=68
    INSERT: new leaf goes at byte=79
    DEBUG: old leaf key=helloworld , new leaf key=yoearth 
    DEBUG: Adding old leaf with key byte=68
    INSERT Leaf split: new_level=0, old_key_byte='hello' (byte=68), new_key_byte='yo' (byte=79)
    count_non_empty_children: found 0 non-empty in array of length 4
    add_child called: key=68 parent_size=0 nonempty_children=0 keys=[00,00,00,00]
    count_non_empty_children: found 0 non-empty in array of length 4
      parent size=0 nonempty=0
    count_non_empty_children: found 0 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 1 keys):
      [0]: 'hello' (len=5 bytes)
    add_child Node4: storing key byte=68 ('h') at idx=0
    Added child at 0 -> 0
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child returned: updated_keys=[68] nonempty_children=1
    DEBUG: Adding new leaf with key byte=79
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child called: key=79 parent_size=1 nonempty_children=1 keys=[68]
    count_non_empty_children: found 1 non-empty in array of length 4
      parent size=1 nonempty=1
    count_non_empty_children: found 1 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 2 keys):
      [0]: 'hello' (len=5 bytes)
      [1]: 'yo' (len=2 bytes)
    add_child Node4: storing key byte=79 ('y') at idx=1
    Added child at 1 -> 1
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child returned: updated_keys=[68,79] nonempty_children=2
    Inserting: yoloearth
    terminate INPUT: [79,65]
    terminate OUTPUT: [79,65,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'yolo' (first byte=79)
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child called: key=79 parent_size=2 nonempty_children=2 keys=[68,79]
    count_non_empty_children: found 2 non-empty in array of length 4
      parent size=2 nonempty=2
    count_non_empty_children: found 2 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 3 keys):
      [0]: 'hello' (len=5 bytes)
      [1]: 'yo' (len=2 bytes)
      [2]: 'yolo' (len=4 bytes)
    add_child Node4: storing key byte=79 ('y') at idx=2
    Added child at 2 -> 2
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child returned: updated_keys=[68,79,79] nonempty_children=3
    [  yolo = hello ][  yolo = yo ]Inserting: yolearth
    terminate INPUT: [79,65]
    terminate OUTPUT: [79,65,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'yol' (first byte=79)
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child called: key=79 parent_size=3 nonempty_children=3 keys=[68,79,79]
    count_non_empty_children: found 3 non-empty in array of length 4
      parent size=3 nonempty=3
    count_non_empty_children: found 3 non-empty in array of length 4
    add_child Node4: new_keys after insert (total 4 keys):
      [0]: 'hello' (len=5 bytes)
      [1]: 'yo' (len=2 bytes)
      [2]: 'yol' (len=3 bytes)
      [3]: 'yolo' (len=4 bytes)
    add_child Node4: storing key byte=79 ('y') at idx=2
    Added child at 2 -> 3
    count_non_empty_children: found 4 non-empty in array of length 4
    add_child returned: updated_keys=[68,79,79,79] nonempty_children=4
    [  yol = hello ][  yol = yo ][  yol = yolo ]Inserting: yoliearth
    terminate INPUT: [79,65]
    terminate OUTPUT: [79,65,00]
    find_child Node4/16: idx=255, array_length=4
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'yoli' (first byte=79)
    count_non_empty_children: found 4 non-empty in array of length 4
    add_child called: key=79 parent_size=4 nonempty_children=4 keys=[68,79,79,79]
    count_non_empty_children: found 4 non-empty in array of length 4
      parent size=4 nonempty=4
    count_non_empty_children: found 4 non-empty in array of length 4
    count_non_empty_children: found 0 non-empty in array of length 16
    count_non_empty_children: found 4 non-empty in array of length 16
    count_non_empty_children: found 4 non-empty in array of length 16
      parent size=4 nonempty=4
    count_non_empty_children: found 4 non-empty in array of length 16
    add_child Node4: storing key byte=79 ('y') at idx=3
    Added child at 3 -> Leaf_node
    count_non_empty_children: found 5 non-empty in array of length 16
    add_child returned: updated_keys=[68,79,79,79,79] nonempty_children=5
    [  yoli = hello ][  yoli = yo ][  yoli = yol ][  yoli = yolo ]Computed index = ÿ for key byte y
    Computed index = ÿ for key byte y
    Computed index = ÿ for key byte y
    Inserting: yopoearth
    terminate INPUT: [79,65]
    terminate OUTPUT: [79,65,00]
    find_child Node4/16: idx=255, array_length=16
    find_child Node4/16: not found (255)
    INSERT Inner_node: level=0, List.nth key level = 'yopo' (first byte=79)
    count_non_empty_children: found 5 non-empty in array of length 16
    add_child called: key=79 parent_size=5 nonempty_children=5 keys=[68,79,79,79,79]
    count_non_empty_children: found 5 non-empty in array of length 16
      parent size=5 nonempty=5
    count_non_empty_children: found 5 non-empty in array of length 16
    add_child Node4: storing key byte=79 ('y') at idx=5
    Added child at 5 -> Leaf_node
    count_non_empty_children: found 6 non-empty in array of length 16
    add_child returned: updated_keys=[68,79,79,79,79,79] nonempty_children=6
    [  yopo = hello ][  yopo = yo ][  yopo = yol ][  yopo = yoli ][  yopo = yolo ]Computed index = ÿ for key byte y

    Root node type: Searching in Node keys: [BYTE representation :[ hello]
    BYTE representation :[ yo]
    BYTE representation :[ yol]
    BYTE representation :[ yoli]
    BYTE representation :[ yolo]
    BYTE representation :[ yopo]
    ]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 3
     [ h]
    [ w]
    [  ]
    Level 0
    [  hello = hello ]Computed index =   for key byte h
    search: find_child returned: Leaf
    Search key h compared with h  Found 'helloworld'  10
    Searching in Node keys: [BYTE representation :[ hello]
    BYTE representation :[ yo]
    BYTE representation :[ yol]
    BYTE representation :[ yoli]
    BYTE representation :[ yolo]
    BYTE representation :[ yopo]
    ]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 3
     [ y]
    [ e]
    [  ]
    Level 0
    [  yo = hello ][  yo = yo ]Computed index =  for key byte y
    search: find_child returned: Leaf
    Search key y compared with y  Found 'yoearth'  7
    Searching in Node keys: [BYTE representation :[ hello]
    BYTE representation :[ yo]
    BYTE representation :[ yol]
    BYTE representation :[ yoli]
    BYTE representation :[ yolo]
    BYTE representation :[ yopo]
    ]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 3
     [ y]
    [ e]
    [  ]
    Level 0
    [  yolo = hello ][  yolo = yo ][  yolo = yol ][  yolo = yoli ][  yolo = yolo ]Computed index =  for key byte y
    search: find_child returned: Leaf
    Search key y compared with y  Found 'yoloearth'  9
    Searching in Node keys: [BYTE representation :[ hello]
    BYTE representation :[ yo]
    BYTE representation :[ yol]
    BYTE representation :[ yoli]
    BYTE representation :[ yolo]
    BYTE representation :[ yopo]
    ]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 3
     [ y]
    [ e]
    [  ]
    Level 0
    [  yol = hello ][  yol = yo ][  yol = yol ]Computed index =  for key byte y
    search: find_child returned: Leaf
    Search key y compared with y  Found 'yolearth'  8
    Searching in Node keys: [BYTE representation :[ hello]
    BYTE representation :[ yo]
    BYTE representation :[ yol]
    BYTE representation :[ yoli]
    BYTE representation :[ yolo]
    BYTE representation :[ yopo]
    ]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 3
     [ y]
    [ e]
    [  ]
    Level 0
    [  yoli = hello ][  yoli = yo ][  yoli = yol ][  yoli = yoli ]Computed index =  for key byte y
    search: find_child returned: Leaf
    Search key y compared with y  Found 'yoliearth'  9
    Searching in Node keys: [BYTE representation :[ hello]
    BYTE representation :[ yo]
    BYTE representation :[ yol]
    BYTE representation :[ yoli]
    BYTE representation :[ yolo]
    BYTE representation :[ yopo]
    ]
     prefix_match_index1 node key level 0  prefix_len 0

    Length of key 3
     [ y]
    [ e]
    [  ]
    Level 0
    [  yopo = hello ][  yopo = yo ][  yopo = yol ][  yopo = yoli ][  yopo = yolo ][  yopo = yopo ]Computed index =  for key byte y
    search: find_child returned: Leaf
    Search key y compared with y  Found 'yopoearth'  9

    === DEBUG: Children array contents ===
    children[0]: Leaf with key=helloworld 
    children[1]: Leaf with key=yoearth 
    children[2]: Leaf with key=yolearth 
    children[3]: Leaf with key=yoliearth 
    children[4]: Leaf with key=yoloearth 
    children[5]: Leaf with key=yopoearth 
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
    terminate INPUT: [68,77]
    terminate OUTPUT: [68,77,00]
    find_child Node4/16: idx=0, array_length=16
    find_child Node4/16: returning children[0]
      -> was Leaf
    terminate INPUT: [79,65]
    terminate OUTPUT: [79,65,00]
    find_child Node4/16: idx=1, array_length=16
    find_child Node4/16: returning children[1]
      -> was Leaf
    terminate INPUT: [79,65]
    terminate OUTPUT: [79,65,00]
    find_child Node4/16: idx=4, array_length=16
    find_child Node4/16: returning children[4]
      -> was Leaf
    terminate INPUT: [79,65]
    terminate OUTPUT: [79,65,00]
    find_child Node4/16: idx=2, array_length=16
    find_child Node4/16: returning children[2]
      -> was Leaf
    terminate INPUT: [79,65]
    terminate OUTPUT: [79,65,00]
    find_child Node4/16: idx=3, array_length=16
    find_child Node4/16: returning children[3]
      -> was Leaf
    terminate INPUT: [79,65]
    terminate OUTPUT: [79,65,00]
    find_child Node4/16: idx=5, array_length=16
    find_child Node4/16: returning children[5]
      -> was Leaf
    |}]
