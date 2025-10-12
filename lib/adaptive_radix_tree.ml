open Batteries
open Bigarray
open Types


exception EmptyKeys

module type Iterator =
sig
  val has_next: 'n list -> bool
  (* val next : 'n *)

end

 module RADIX ( Iter : Iterator ) = struct
    type 'a radix_node = 'a
    include MakeRadixNode (struct type 'a t = 'a radix_node end)

	let node4 = 0
	let  node16 = 1
	let  node48 = 2
	let  node256 = 3
	let leaf = 44

	let  node4min = 2
	let  node4max = 4

	let  node16min = node4max + 1
	let  node16max = 16

	let  node48min = node16max + 1
	let node48max = 48

	let node256min = node48max + 1
	let node256max = 256

	let max_prefix_len = 10


let char_list_to_byte_list cl =
    let bl  = [] in
    List.map ( fun c ->   bl @ [(Bytes.init 1 ( fun _ -> c )  )]) cl

let make_node_list nodemax node_type =
  let rec loop_while node_list j_dx =
      if j_dx < nodemax then(
		let b = Bytes.create max_prefix_len |> Bytes.to_seq |> List.of_seq in
		let b1 = Bytes.create node4max |> Bytes.to_seq |> List.of_seq in
	    let inn = (
		 Prefix((List.map List.hd (char_list_to_byte_list b)), 0 , 0), (*  Redundant *)
		 node_type,
         List.map List.hd (char_list_to_byte_list b1),
         CCArray.make nodemax Empty
		 ) in
		loop_while (node_list @ [Inner_node inn]) (j_dx + 1 );
      ) else CCArray.of_list node_list
  in
  loop_while [] 0

let new_node4 =

	let b = Bytes.create max_prefix_len |> Bytes.to_seq |> List.of_seq in
	let b1 = Bytes.create node4max |> Bytes.to_seq |> List.of_seq in
	let inn = (
		 (* Prefix ((List.map List.hd (char_list_to_byte_list b)), 0 , 0), (\*  Redundant *\) *)

		 Prefix ([], 0 , 0),
		 Node4 node4,
         (* List.map List.hd (char_list_to_byte_list b1), *)
         [],
		 make_node_list node4max (Node4 node4))
	in
	inn

let new_node16 : inner_node =
	let b = Bytes.create max_prefix_len |> Bytes.to_seq |> List.of_seq in
	let b1 = Bytes.create node16max |> Bytes.to_seq |> List.of_seq in
	let inn = (
		 Prefix ([], 0 , 0),
		 Node16 node16,
         [],
		 make_node_list node16max (Node16 node16))
	in
	inn

let new_node48 : inner_node =
	let b = Bytes.create max_prefix_len |> Bytes.to_seq |> List.of_seq in
	let b1 = Bytes.create node48max |> Bytes.to_seq |> List.of_seq in
	let inn = (
		 Prefix((List.map List.hd (char_list_to_byte_list b)), 0 , 0), (*  Redundant *)
		 Node48 node48,
        [],                                                           (* zero bytes.None is better *)
		 make_node_list node48max (Node48 node48))
	in
	inn

let new_node256 =
	let b = Bytes.create max_prefix_len |> Bytes.to_seq |> List.of_seq in
	let inn = (
		Prefix((List.map List.hd (char_list_to_byte_list b)), 0 , 0), (*  Redundant *)
		Node256 node256,                                              (* Keys can't be arbitrary *)
        [],                                                           (* zero bytes.None is better *)
		make_node_list node256max (Node256 node256))
	in
	inn

let trailing_zeros bitfield =

  let rec count c =
    if (Int32.logand (Int32.shift_right_logical bitfield c) (Int32.of_int 1 )) <> (Int32.of_int 1 ) then
      count (c + 1)
    else c
  in count 0

let index n key =
	match n with
	  | ( meta, node_type, keys, _) ->
	   match node_type with
       |Node4  node4 ->
         (match meta with
          | Prefix (l, size , i2) ->
            let rec loop_while j_dx=
             if j_dx < size   then(
			  if ((List.nth keys j_dx) == key) then
                 key
              else
                loop_while ( j_dx + 1 )
             )
			 else(
		        Bytes.make 1 (Char.chr 255)  (*TODO  Intended to indicate an exception now*)
             )
            in
            loop_while 0
         )

      |Node16  node16 ->
        ( match meta with
          | Prefix (l, size , i2) ->
		    let mask =  Int32.to_int (Int32.shift_left (Int32.of_int 1) size) - 1 in
            let bitfield = Array1.create Int32 c_layout 1 in
            bitfield.{0} <- Int32.of_int 0b00000000;
		    bitfield.{0} <- Int32.logand  bitfield.{0} (Int32.of_int  mask);
		    if (Int32.lognot bitfield.{0} ) == 0l then(
            let rec loop_while j_dx=
             if j_dx < size   then(
			  if (List.nth keys j_dx) == key then
				let bitfield  =  Int32.logor bitfield.{0}  (Int32.shift_left 1l j_dx) in ()
              else
                loop_while ( j_dx + 1 )
             )
            in
            loop_while 0;

		   if (Int32.lognot bitfield.{0} ) == 0l then(
			Bytes.make 1  (Char.chr (trailing_zeros bitfield.{0}))
           )
		   else(
		        Bytes.make 1 (Char.chr 255)  (*TODO  Intended to indicate an exception now*)
           )
          )
		   else
		        Bytes.make 1 (Char.chr 255)  (*TODO  Intended to indicate an exception now*)
       )

      |Node48  node48 ->
		let index = (List.nth keys (Bytes.get_int8  key 0)) in
		if (Bytes.get_int8 index 0) >  0 then
			Bytes.make 1 (Char.chr ((Bytes.get_int8 index 0) - 1))
		else
			Bytes.make 1 (Char.chr 255)

      |Node256  node256 ->
		key
let find_child n key =          (*  How do we check if n is None or Some ?*)
                                (* Is that check needed ? *)

   let index = index n key in

   match n with
	  | ( meta, node_type, keys, children ) ->
        match node_type with
        | Node4 _
        | Node16 _
        | Node48 _
        | Leaf _ ->
		if (Bytes.get_int8 index 0) >= 0 then
			Array.get children (Bytes.get_int8 index 0) (*  TODO Exception handler*)
        else Empty

	    | Node256 node256 ->
		 Array.get children (Bytes.get_int8 key 0) (*  TODO Exception handler*)

let count_non_empty_children children =
  Array.fold_left( fun acc c ->
              match c with | Empty ->
                   acc
                           | _ ->
                   acc + 1) 0 children

let grow (n : inner_node ) =
	match n with
	  | ( meta, node_type, keys, children ) ->
	   match node_type with
       | Node4 node4 ->
         let (  n_16meta, node_type,  n_16keys,  n_16children) = new_node16 in
           (match meta with
            | Prefix (l, i1, i2) ->
            let new_n_16keys =
            let rec loop_while j_dx modified_keys_acc =
            if j_dx < i1  then(
			 let child = Array.get children j_dx in
             let c = Array.set n_16children j_dx child in
                let modified_keys =
                List.mapi (fun i el -> if i = j_dx then
                                         (List.nth keys j_dx)
                                         else el) keys in (* TODO Array is mutable and needed here*)
                loop_while (j_dx + 1) modified_keys)
            else modified_keys_acc
            in
            loop_while 0 keys in
	        (
                   Prefix (l, count_non_empty_children children, i2),
		           Node16 node16,
                   new_n_16keys,
                   n_16children)
            )
	   | Node16 node16 ->     (*Create a Node48 and set values  *)
         Printf.printf "Grow node16\n";
         let (  n_48meta, node_type,  n_48keys,  n_48children) = new_node48 in
         (match meta with
          | Prefix (l, i1, i2) ->
           let index = 0 in
           let new_n_48keys =
           let rec loop_while j_dx idx modified_keys_acc=
            if j_dx < i1  then(
			 let child = Array.get children j_dx in
			 match child with
               | Empty -> loop_while ( j_dx + 1 ) ( index + 1 ) modified_keys_acc
               | _ ->
                let modified_keys = List.mapi (fun i el -> if i = j_dx then
                                         (Bytes.make 1 (Char.chr (idx + 1)))
                                         else el) keys in (* TODO Array is mutable and needed here*)
                let c = Array.set n_48children index child in
                loop_while ( j_dx + 1 ) ( idx + 1 )  modified_keys)
            else modified_keys_acc
          in
          loop_while 0 index  keys in
	             (
                 Prefix (l, count_non_empty_children children, i2),
		         Node48 node48,
                 new_n_48keys,
                 n_48children)
           )
       |Node48 node48 ->
         Printf.printf "Grow node48\n";
         let (  n_256meta, node_type,  n_256keys,  n_256children) = new_node256 in
         match meta with
          | Prefix (l, i1, i2) ->
           let rec loop_while i length_of_keys =
            let i_int = Bytes.get_int8 i 0 in
            let new_n_256keys =  n_256keys in
            if i_int  < length_of_keys  then(
			 let child = find_child n i in
			 match child with
               | Empty -> loop_while (Bytes.make 1 (Char.chr ( i_int + 1 ))) length_of_keys
               | _ ->
                Printf.printf "Grow node48 %d %d\n" i_int (Array.length n_256children);
                let c = Array.set n_256children i_int child in
                loop_while (Bytes.make 1 (Char.chr ( i_int + 1 ))) length_of_keys;
           ) else(
	             (
                 Prefix (l, count_non_empty_children children, i2),
		         Node48 node48,
                 new_n_256keys,
                 n_256children)
           )
          in
          loop_while (Bytes.make 1 (Char.chr 0)) (List.length keys)

let maxsize node_type =
	 match node_type with
      | Node4 _ -> node4max
      | Node16 _-> node16max
      | Node48 _-> node48max
      | Node256 _-> node256max


let rec add_child key parent child =


	match parent with
	  | ( meta, node_type, keys, children ) ->
          let Prefix(l, size, len) = meta in
    if (count_non_empty_children children)== maxsize node_type then(
            let grow_n = grow parent in
            add_child key grow_n child)
    else (
	   match node_type with
       | Node4 node4 ->
        let (  n_4meta, node_type,  n_4keys,  n_4children) = parent in
        (match meta with
          | Prefix (l, i1, i2) ->
        let idx =
            let rec loop_while id_x= (* TODO Check the spec. of 'compare' *)
            if id_x < List.length n_4keys && Bytes.compare key (List.nth n_4keys id_x) > 0 then(
                 loop_while (id_x + 1))
               else  id_x
             in
             loop_while 0;
             in
         (* The logic used for other nodes is dissimilar to this but a bug was causing *)
         (* infinite recursion. So this is used now. *)
         let (split_before_keys, split_after_keys) =
          let rec split_at_index i acc =  function
            | [] -> (List.rev acc, [])
            | h :: t as lst ->
              if i = 0 then (List.rev acc, lst)
              else split_at_index (i - 1) (h :: acc) t
          in
          split_at_index idx [] n_4keys
        in
        let new_n4_keys = split_before_keys @ [key] @ split_after_keys in
            let new1_n4_keys =
                (match n_4keys with
                  | [] -> [key]
                  |_::_ ->
                if( idx < List.length new_n4_keys ) then(
                 List.mapi (fun j el -> if j = idx then
                                        key
                                        else el) new_n4_keys(* TODO Array is mutable and needed here*)
                )
                else(
                   new_n4_keys
                 )) in

                if idx <= size && idx <
                    Array.length n_4children then
                    Array.set n_4children idx (child
                 );        (*  This child should be a parameter*)
	             (
                 Prefix (l, size + 1, i2), (* TODO Increment size of the parent and child properly*)
		         node_type,
                 new1_n4_keys,
                 n_4children)
        )
       | Node16 node16 ->
        let (  n_16meta, node_type,  n_16keys,  n_16children) = parent in
        (match meta with
          | Prefix (l, i1, i2) ->
		let idx = i1 in
        let bitfield = Array1.create Int32 c_layout 1 in
        bitfield.{0} <- Int32.of_int 0b00000000;
        let rec loop_while jdx=
            if idx < jdx then
			if List.nth keys jdx >= key then
		    bitfield.{0} <- Int32.logor  bitfield.{0} (Int32.shift_left (Int32.of_int 1) jdx)
			else
            loop_while (jdx + 1);
        in
        loop_while 0;
		let mask =  Int32.to_int (Int32.shift_left (Int32.of_int 1) i1) - 1 in
		bitfield.{0} <- Int32.logand  bitfield.{0} (Int32.of_int  mask);
        let idx =
		if (Int32.lognot bitfield.{0} ) == 0l then(
			 trailing_zeros bitfield.{0}
		) else idx in

        let n16_keys =
        let rec loop_while jdx modified_keys_acc =
            if jdx > idx then(
            if jdx < List.length keys && Bytes.compare key (List.nth keys (jdx - 1)) > 0 then(
                let c = Array.get n_16children (jdx - 1) in
                Array.set n_16children jdx c;
                let modified_keys =
                List.mapi (fun j el -> if jdx = j then
                           List.nth keys (jdx - 1)
                           else el) modified_keys_acc (* TODO Array is mutable and needed here*)
                in loop_while (jdx - 1) modified_keys)
            else loop_while (jdx - 1) modified_keys_acc)
            else modified_keys_acc
        in
        loop_while (Array.length children) keys in

         Printf.printf "Size is %d\n" size;
         if idx <= size && idx <
                    Array.length n_16children then
                    Array.set n_16children idx (child);        (*  This child should be a parameter*)
	             (
                 Prefix (l, size + 1, i2), (* TODO Increment size of the parent and child properly*)
		         node_type,
                 n_16keys,
                 n_16children)
        )

      | Node48 node48 ->
        let (  n_48meta, node_type,  n_48keys,  n_48children) = parent in
        match meta with
          | Prefix (l, i1, i2) ->
		    let idx = 0 in
            let rec loop_while idx=
            if idx < Array.length children then
              match (Array.get children idx) with
              | Empty -> loop_while (idx + 1);
              | _ -> loop_while idx;
            in
            loop_while idx;
            let n_48keys = List.mapi (fun i el -> if i = (Bytes.get_int8 key 1) then
                                       let k_incr =  ((Bytes.get_int8 key 1) + 1) in  (* increment the char *)
                                       let b_key = (Bytes.make 1 (Char.chr k_incr)) in
                                       b_key
                                  else el) keys in (* TODO Array is mutable and needed here*)
                if idx <= size && idx <
                    Array.length n_48children then
                    Array.set n_48children  (Bytes.get_int8 key 1)  (child);        (*  This child should be a parameter*)
	             (
                 Prefix (l, size + 1, i2),
		         node_type,
                 n_48keys,
                 n_48children)
       )

let rec minimum node =

	match node with
	  |Inner_node inn ->
       (match inn with
        | ( meta, node_type, keys, children )->
	    (match node_type with
	     | Node4 v |Node16 v ->
		     minimum (Array.get  children 0)
         | Node48 node48 ->
          let i =
            let rec loop_while idx =
            if Bytes.compare (List.nth keys idx)  (Bytes.make 1 (Char.chr 0)) == 0 then
              loop_while (idx + 1 )
            else
              idx
            in
            loop_while 0
            in
            let child = Array.get children   (Int.of_string (Bytes.to_string (List.nth keys i)) - 1)    in
            minimum child

        | Node256 node256 ->
            let i =
            let rec loop_while idx =
            if (Bytes.compare (List.nth keys idx) (Bytes.make 1  '\x00')) == 0 then
              loop_while (idx + 1 )
            else
              idx
            in
            loop_while 0
            in
            let child = Array.get children   (Int.of_string (Bytes.to_string (List.nth keys i)) - 1)    in
            minimum child
	    | Leaf l  -> node
        )
       )



let compare_keys key key1 =
  if List.length key <> List.length key1 then
    -1
  else
   let rec compare comp elem elem1 =
    match elem, elem1 with
      | [], [] -> comp
      | hd :: tl, hd1 :: tl1->
            let comp =  Bytes.compare hd hd1 in
            if comp == 0 then
              compare comp tl tl1
            else
              comp
      | _, _ -> raise EmptyKeys
    in compare 0 key key1

let  prefix_match_index1 inner_node key level =
  match inner_node with
	|Inner_node inn ->
    let id_x =
    (match inn with
	| ( meta, node_type, keys, children )->
        match meta with
          | Prefix (prefix, i1, prefix_len) ->
             let rec loop_while idx pref =
               if idx < prefix_len && (level + idx) < List.length key &&
                  (List.nth key (level + idx) == List.nth pref idx) then(

                 if idx == (max_prefix_len-1) then(
                     match (minimum inner_node) with
                       |Leaf l ->
                        match l with
                        |KeyValue kv ->
                         loop_while (idx + 1 ) (List.filteri (fun i _ -> i >= level &&
                                                                         i < (List.length kv.key )) kv.key)
                       |_ -> failwith "prefix_match_index1"
                 )
                 else loop_while (idx + 1 ) pref
                )
                else idx
             in
             loop_while 0 prefix
    )
    in id_x

let  prefix_match_index l kv level =
    match l with
    |KeyValue kv1 ->

	let limit = Int.min (List.length kv1.key)
                        (List.length kv.key) - level
    in
    let result =
    let rec loop_while i  =
      if i < limit then(
		if (List.nth kv1.key  (level + i)) !=
		   (List.nth kv.key  (level + i)) then(
        i
        )
        else
        loop_while (i + 1)
      )else i;
    in
    loop_while 0;
    in result

let copy key_list_src key_list_dest level =
   List.mapi (fun j el -> if j <= level then
                          el
                          else (List.nth  key_list_dest j)) key_list_src(* TODO Array is mutable*)

(*  Find the null byte or append it to the key if it is not found*)
let terminate key =
   let byte =  (Bytes.make 1 '\x00') in
	match (List.find (fun elt ->  if (Bytes.compare (Bytes.make 1 '\x00') elt == 0) then true else false) key) with
   | byte -> key
   | _ -> List.append key [(Bytes.make 1 '\x00')]


let rec insert (tr : tree) node key value level  =
  match node with
  | Empty ->
    let new_key = List.map( fun x -> x ) key in
    let kv = {key = new_key; value = value }in
    (true, KeyValue kv)
  | Leaf l ->
             match l with
              | leaf_node  ->
             match leaf_node with | KeyValue kv ->
             if compare_keys  kv.key  key == 0 then(
             kv.value <- value;
             );
             let kv = {key = key; value = value }in
             let new_key = List.map( fun x -> x ) key in
             let kv = {key = new_key; value = value }in
             let new_leaf = KeyValue kv in
             let limit = prefix_match_index l kv  level in
             let new_node = new_node4 in
             match new_node with
             | ( meta, node_type, keys, children )->
                 (match meta with
                   | Prefix (prefix, i1, prefix_len) ->
                     let copied_prefix = copy key prefix level in
                     let level = level + prefix_len in
                     let parent =
                       (
                       Prefix (copied_prefix, i1, limit),
                       node_type,
                       keys,
                       children) in
                     let updated_node = add_child (List.nth kv.key level) parent node in
                     let _ = add_child (List.nth key  level ) updated_node (Leaf new_leaf) in
                     (false,l)
                )
    | ( meta_in, node_type_in, keys_in, children_in ) as inn->
                 match meta_in with
                   | Prefix (prefix_in, i1_in, prefix_len_in) ->
                      if prefix_len_in != 0 then(
                        let prefix_match_result = prefix_match_index1 node key level in
                        if prefix_match_result != prefix_len_in then
                          let new_node = new_node4 in

                          (match new_node with
                          | ( meta, node_type, keys, children )->
                            (match meta with
                              | Prefix (new_prefix, i1, prefix_len) ->

                                let copied_prefix = copy prefix_in new_prefix prefix_match_result in

                                let new_node4=
                                  (
                                  Prefix (copied_prefix, i1,  prefix_match_result),
                                  node_type,
                                  keys,
                                  children) in
                                  let modified_node =
                                  if prefix_len < max_prefix_len then(
                                      let _ = add_child (List.nth prefix_in prefix_match_result) new_node4 node in
                                      let copied_prefix = copy prefix_in (List.filteri (fun i _ -> i >= (prefix_match_result+1) && i < (List.length prefix_in )) prefix_in)  (Int.min prefix_len_in max_prefix_len) in
                                      let new_node4=
                                               (
                                               Prefix (copied_prefix, i1_in,  prefix_len_in - (prefix_match_result + 1)) ,
                                               node_type_in,
                                               keys_in,
                                               children_in)
                                  in
                                  let kv = {key = key; value = value }in
                                  let new_leaf = KeyValue kv in
                                  let _ = add_child (List.nth key (level + prefix_match_result)) new_node4 (Leaf new_leaf) in
                                  (false, Leaf new_leaf)
                                  )
                                  else(
                                      let prefix_len_in = prefix_len_in - (prefix_match_result + 1) in
                                      match (minimum node) with
                                        |Leaf l ->
                                         match l with
                                         |KeyValue kv ->
                                      let _ = add_child (List.nth kv.key (level + prefix_match_result)) new_node4 node in
                                      let copied_prefix = copy prefix_in (List.filteri (fun i _ -> i >= (prefix_match_result + level + 1) && i < (List.length kv.key )) kv.key)  (Int.min prefix_len_in max_prefix_len) in
                                      let new_node4=
                                               (
                                               Prefix (copied_prefix, i1_in,  prefix_len_in) ,
                                               node_type_in,
                                               keys_in,
                                               children_in)
                                         in
                                         let kv = {key = key; value = value }in
                                         let new_leaf = KeyValue kv in
                                         let _ = add_child (List.nth key (level + prefix_match_result)) new_node4 (Leaf new_leaf) in
                                         (false, Leaf new_leaf)
                                  ) in
                                         ()
                               )
                               )
             );

             let level = level + prefix_len_in in

             let next = find_child inn (List.nth  key level ) in
             match next with
             | Empty  -> let _ = add_child (List.nth  key level ) inn (Leaf new_leaf) in  (false,new_leaf)
             | _ -> insert tr  next key value (level+1)
        | _ -> failwith "Unknown pattern"

let insert_tree tree key value =
	let key = terminate key in
	let updated_tree = insert tree tree.root key value 0 in
	match (updated_tree) with
	|(true,leaf_node)  -> leaf_node


end

module type RADIXOperator = sig
  type 'a radix_node = 'a
  include  module type of MakeRadixNode ( struct type 'a t = 'a radix_node end )
  val new_node4 : meta * node_type * bytes list * node array
  val new_node16 : meta * node_type * bytes  list * node array
  val add_child : bytes -> meta * node_type * bytes   list * node array ->
                  node ->  meta * node_type * bytes   list * node array
  val insert_tree :  tree -> Bytes.t list ->  int64 -> leaf_node
end

module RADIXOp =
RADIX(struct

  let  has_next l=
     true


 end)
