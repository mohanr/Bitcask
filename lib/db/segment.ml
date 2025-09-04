open Wal_store.DataEntryOp
open CCMap
open Types

exception Block_Not_Found of string
exception Segment_Not_Found of string

module Segment = struct

type key_block_offset = {
	offset   :   int64;
	mutable block_number :int64;
	size     :   int64;
	path     :   string;
	timestamp   :   int64;
}
let get_segment (db : data_store ) path =
  SegmentMap.get path db.segments

let create_new_segment (* block_offset_pair *) =

    let m =
    BlockOffsetMap.empty   (*  The block offset is empty to start with*)
    (* |> BlockOffsetMap.add (Int64.of_int 0) *)
		(* 	  ({ startoffset = block_offset_pair.offset; *)
		(* 		endoffset = Int64.add block_offset_pair.offset block_offset_pair.size }) *)
    in
    {
	closed       =      false;
	current_block_number = (Int64.of_int 0);
	current_block_offset = (Int64.of_int 0);
	blocks           =  Blockoffset m
	}

let get_segment_block (db : Types.data_store) path block_number =
	let segment = SegmentMap.find path db.segments in
	let block = BlockOffsetMap.find block_number
                          (match segment.blocks with
                            | Blockoffset  b -> b) in
  block

let set_segment_block segment block_number new_segment_block =
  BlockOffsetMap.add block_number new_segment_block
                          (match segment.blocks with
                            | Blockoffset  b -> b)

let set_segment db path segment =
	let _ = SegmentMap.add path segment db.segments in
  ()

let update_segment db key_block_offset segment =
	let segment_block = get_segment_block db key_block_offset.path segment.current_block_number in
	let new_segment_block =
      {
        startoffset = segment_block.startoffset ;
        endoffset = Int64.add key_block_offset.offset  key_block_offset.size;
        path = segment_block.path
      }
  in
	let _ = set_segment_block segment segment.current_block_number new_segment_block in
	if Int64.compare (Int64.add segment.current_block_offset key_block_offset.size) (Int64.of_int 1024) <= 0 then begin (* TODO Check 1024 *)
		key_block_offset.block_number <- segment.current_block_number;
		segment.current_block_offset <- Int64.add segment.current_block_offset  key_block_offset.size;
	end else begin
		segment.current_block_number <-  Int64.add segment.current_block_number  (Int64.of_int 1);
		let _ = BlockOffsetMap.add segment.current_block_number
    {
			startoffset = key_block_offset.offset;
			endoffset =  Int64.add key_block_offset.offset key_block_offset.size;
			path = key_block_offset.path
		}
     (match segment.blocks with
      | Blockoffset  b -> b) in
    key_block_offset.block_number <- segment.current_block_number;
		segment.current_block_offset <- key_block_offset.size;

 	let _ = set_segment db key_block_offset.path segment in ()
  end
end

module type SegmentOperator =
sig
  val create_new_segment : blockoffset_pair BlockOffsetMap.t -> Types.segment
  val get_segment : Types.data_store -> string -> Types.segment
end
