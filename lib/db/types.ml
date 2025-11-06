open Containers

module  Blockoffset = struct
    type t = int64

    let compare offset offset1 =
      Int64.compare offset offset1
end

module BlockOffsetMap =CCMap.Make(Blockoffset)

module  Inflight_wal_vector = CCVector

type blockoffset_pair = {startoffset : int64; endoffset :  int64 ; path : string }
type blocks =  Blockoffset of blockoffset_pair BlockOffsetMap.t
type segment = {
	closed       :      bool;
	mutable current_block_number :int64;
	mutable current_block_offset :int64;
	blocks           :  blocks
}

module  Segmentsmap = struct
  type t = string
  let compare dirpath dirpath1 =
    String.compare dirpath dirpath1
end

module SegmentMap = CCMap.Make(Segmentsmap)

type data_store ={
	dirpath : string;
	last_offset : int Atomic.t;
	mu        :  Eio.Mutex.t;
	segments  : segment SegmentMap.t;
}

module  Walmap = struct
  type t = int32
  let compare segment_id1 segment_id1 =
    Int32.compare segment_id1 segment_id1
end

module Write_Ahead_Map = CCMap.Make(Walmap)
type wal= {
	mu        :  Eio.Mutex.t;
	existing_segments  : segment Write_Ahead_Map.t;
	writes_in_flight  : Bytes.t Inflight_wal_vector.vector;
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
	   KeyValue of keyvaluepair
    and
    keyvaluepair = {
	  key : Bytes.t list;
      mutable value   : int64
    }
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
	tree =
		{root : node; size : int}
    and
    children = node CCArray.t
      (* [@printer *)
      (*   fun fmt arr -> fprintf fmt "%a" (CCArray.pp pp_node) arr] *)
    [@@deriving show] (* only one call to `deriving show` is not enough *)
end

module type RADIXOperator = sig
  type 'a radix_node = 'a
  include  module type of MakeRadixNode ( struct type 'a t = 'a radix_node end )
  val new_node4 : unit -> meta * node_type * bytes list * node array
  val new_node16 : unit -> meta * node_type * bytes  list * node array
  val add_child : bytes -> meta * node_type * bytes   list * node array ->
                  node ->  meta * node_type * bytes   list * node array
  val insert_tree :  tree -> Bytes.t list ->  int64 ->  node
  val search_with_log_handler : node -> Bytes.t list  -> int -> int64 option
end
