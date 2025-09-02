open Bitcask__Wal_store.DataEntryOp


type key_block_offset = {
	offset   :   int64;
	block_number :int64;
	size     :   int64;
	path     :   string;
	timestamp   :   int64;
}

module  Segmentsmap = struct
  type t = string
  let compare dirpath dirpath1 =
    String.compare dirpath dirpath1
end

module SegmentMap = CCMap.Make(Segmentsmap)



module  Blockoffset = struct
    type t = int64

    let compare x x1 =          (*  TODO *)
      0
end

module BlockOffsetMap =CCMap.Make(Blockoffset)


type segment = {
	closed       :      bool;
	current_block_number :int64;
	current_block_offset :int64;
	blocks           :  blocks
}
and
blockoffset_pair = {startoffset : int64; endoffset :  int64 }
and
blocks =  Blockoffset of blockoffset_pair BlockOffsetMap.t

let create_new_segment block_offset_pair segment =

    let m =
    BlockOffsetMap.empty
    |> BlockOffsetMap.add (Int64.of_int 0)
			  ({ startoffset = block_offset_pair.offset;
				endoffset = Int64.add block_offset_pair.offset block_offset_pair.size })
    in
    {
	closed       =      false;
	current_block_number = (Int64.of_int 0);
	current_block_offset = (Int64.of_int 0);
	blocks           =  Blockoffset m
	}

module type SegmentOperator =
sig
  val create_new_segment : blockoffset_pair BlockOffsetMap.t -> segment
end
