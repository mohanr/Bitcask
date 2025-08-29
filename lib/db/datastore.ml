open Containers
open Eio.Std
open Bitcask__Wal_store.DataEntryOp
open Bitcask__Segment


module type WalOperator =
sig
  val create_entry_map : int -> int -> int64 EntryMap.t
end

module Database (Wal : WalOperator) = struct

let create_entry_map = Wal.create_entry_map


end

let deleted_flag = 98

let mutex = Eio.Mutex.create()
let last_offset = Atomic.make 0

let get_last_offset =
  Atomic.get last_offset

let set_last_offset offset =
  Atomic.set last_offset offset



module DatabaseOp =
Database(struct


   let create_entry_map k v =
    let m =
    EntryMap.empty
    |> EntryMap.add "deleted"   (Int64.of_int deleted_flag)
	|> EntryMap.add "offset"    (Int64.of_int get_last_offset)
	|> EntryMap.add "key_size"   (Int64.of_int 8)
	|> EntryMap.add "value_size" (Int64.of_int 8)
	|> EntryMap.add "key"       (Int64.of_int k)
	|> EntryMap.add "value"     (Int64.of_int v) in
    m

   let store_entry k v =
     let m = create_entry_map k v in
     Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      Fiber.fork ~sw (fun () ->
       (* Lock for reads and writes *)
       Eio.Mutex.use_rw ~protect:true mutex (fun () ->
          create_entry m env
       )
      )
end)
