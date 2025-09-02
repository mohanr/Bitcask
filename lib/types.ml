open Segment


type data_store ={
	dirpath : string;
	last_offset : int Atomic.t;
	mu        :  Eio.Mutex.t;
	segments  : int64 SegmentMap.t;
}
module type RadixNode = sig
type 'a t
end

 module MakeRadixNode ( RadixNode : RadixNode ) = struct
    type meta =
    | Prefix of (Bytes.t list)  * int * int
    [@@deriving show]
    and
	leaf_node =
		Key of Bytes.t list
    and
	inner_node =
		meta *
		node_type *
		Bytes.t list *
		children
    and
    node_type =
      Node4 of int
    | Node16 of int
    | Node48 of int
    | Node256 of int
    | Leaf of int
    and
	node =
	  | Inner_node of inner_node
	  | Leaf of leaf_node
      | Empty
    and
	level =
		Node of  node *
		int
    and
	iterator =
		Tree of  tree *
		node *
		int *
		level list
    and
	tree =
		Root of node
    and
    children = node CCArray.t
      (* [@printer *)
      (*   fun fmt arr -> fprintf fmt "%a" (CCArray.pp pp_node) arr] *)
    [@@deriving show] (* only one call to `deriving show` is not enough *)
end
