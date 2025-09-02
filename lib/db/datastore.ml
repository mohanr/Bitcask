open Containers
open Eio.Std
open Bitcask__Wal_store.DataEntryOp
open Bitcask__Adaptive_radix_tree
open Bitcask__Segment
open CCMap
open Segment
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
  let m =
    SegmentMap.empty
    |> SegmentMap.add dirpath (Int64.of_int deleted_flag)
 in
     {
          dirpath;
          last_offset =  Atomic.make 0;
          mu        =   Eio.Mutex.create();
          segments  = m;
      }
end


module WalOp =
struct

type data_store = Types.data_store

let deleted_flag = 98
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
