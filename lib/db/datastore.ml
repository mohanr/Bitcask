open Containers
open Eio.Std
open Bitcask__Wal_store.DataEntryOp


module type WalOperator =
sig
  val create_entry_map : int -> int -> int64 EntryMap.t
end

module Database (Wal : WalOperator) = struct

type 'a kv = { key : Bytes.t list; value : 'a }


type key_block_offset = {
	offset   :   int64;
	block_number :int64;
	size     :   int64;
	path     :   string;
	timestamp   :   int64;
}


module BlockMap = CCMap.Make(Entrykeyvalue)
type blocks =  Blockoffset_pair_key of string BlockMap.t

type segment = {
	closed       :      bool;
	currentBlockNumber :int64;
	currentBlockOffset :int64;
	path              : string;
	blocks           :  blocks
}
and
blockoffset_pair = {startoffset : int64; endoffset :  int64; filePath  :  string} module  Blockoffset_pair_key = struct type t = blockoffset_pair

    let compare offset_pair offset_pair1 =
      match offset_pair, offset_pair1 with
      | _ , _
       ->  if Int64.compare  offset_pair.startoffset  offset_pair1.startoffset  > 0
             then -1
           else if  Int64.compare offset_pair.startoffset  offset_pair1.startoffset < 0
             then 1 else 0
 end
let create_entry_map = Wal.create_entry_map
module BlockOffsetMap =CCMap.Make(Blockoffset_pair_key)
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
