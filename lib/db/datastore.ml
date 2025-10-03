open Containers
open Eio.Std
open Bitcask__Wal_store.DataEntryOp
open Bitcask__Adaptive_radix_tree
open CCMap
open Segment.Segment
open Types


module type WalOperator =
sig
  val create_entry_map : data_store -> int -> int -> int64 EntryMap.t
  val get_last_offset : data_store ->int
  val set_last_offset : data_store -> int -> unit
end

module Database (Wal : WalOperator)  = struct


let create_entry_map = Wal.create_entry_map

let deleted_flag = 98


type data_store = Types.data_store


let create_data_store dirpath  =
  let segment_map =
    SegmentMap.empty in
  let m =
    SegmentMap.add dirpath (create_new_segment (* key_block_offset *)) in
     {
          dirpath;
          last_offset =  Atomic.make 0;
          mu        =   Eio.Mutex.create();
          segments  = segment_map;
      }
end

module WalOp =
struct

type data_store = Types.data_store

let deleted_flag = 98

let open_wal  =
	{
	  writes_in_flight  = Inflight_wal_vector.create();
	  existing_segments  = Write_Ahead_Map.empty;
    mu        =   Eio.Mutex.create();
	}



let get_last_offset (db : data_store) =
  Atomic.get db.last_offset

let set_last_offset (db : data_store ) offset =
  Atomic.set db.last_offset offset

let create_entry_map  (db : data_store ) k v =
    let m =
    EntryMap.empty
    |> EntryMap.add "deleted"   (Int64.of_int deleted_flag)
    |> EntryMap.add "offset"    Int64.(of_int (get_last_offset db))
    |> EntryMap.add "key_size"   (Int64.of_int 8)
    |> EntryMap.add "value_size" (Int64.of_int 8)
    |> EntryMap.add "key"       (Int64.of_int k)
    |> EntryMap.add "value"     (Int64.of_int v) in
     m

let  setkey_value_offset_block db key_block_offset path  =

  (*  TODO Exception handler*)

  match (get_segment db path) with
  | Some s ->
		update_segment db key_block_offset s
  | None ->
		set_segment db key_block_offset.path create_new_segment

   let store_entry db k v =
     let m = create_entry_map db k v in
     Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      Fiber.fork ~sw (fun () ->
       (* Lock for reads and writes *)
       Eio.Mutex.use_rw ~protect:true db.mu  (fun () ->
          create_entry m env
       )
      )
end

module type DATASTOREOperator = sig
  type data_store = Types.data_store
  val create_data_store : string -> data_store
  val create_entry_map  : data_store  -> int -> int -> int64 EntryMap.t
end

module DatabaseOp =
Database(WalOp)
