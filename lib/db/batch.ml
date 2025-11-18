open Bigarray
open Bitcask__Datastore
open Bitcask__Wal_store.DataEntryOp
open Bitcask__Snowflake
open Containers
open Bitcask__Murmurhash
open Eio.Std
open Eio
open Utils
open Bin_prot.Std
open Bigstring

exception ErrorBatchCommitted
exception ErrorBatchRolledback

type wal_record = {
	key      : string;
	value    : string ;
	batch_id : int64;
    wal_type     : record_type
}
and record_type = | WalRecord | WalRecordEnd
[@@deriving bin_io]

module  Inflightmap = struct
  type t = Int32.t
  let compare v v1 =
     Int32.compare  v v1
end

module  Inflight_vector = CCVector

module InflightMap = CCMap.Make(Inflightmap)

type batch = {
	db        :       (module DATASTOREOperator);
	writes_in_flight  : wal_record Inflight_vector.vector;
	mutable writes_in_flight_fast_access : wal_record InflightMap.t;
	mu        :  Eio.Mutex.t;
	committed    :    bool;
	rolledback   :    bool;
}

 (* A new batch *)
let   newbatch  (module M : DATASTOREOperator) =
 let node = create_snowflake_node (Int64.of_int 0) in
 let id = generate node in
    {
	db        =       (module M : DATASTOREOperator);
	writes_in_flight  = Inflight_vector.create();
	writes_in_flight_fast_access = InflightMap.empty;
    mu        =   Eio.Mutex.create();
	committed    =    false;
	rolledback   =    true;
}

let check_for_inflight_writes b key  =

    let hash_of_key = murmurhash key (Int32.of_int (Bytes.length key)) (Int32.of_int 0) in
    InflightMap.find_opt hash_of_key b.writes_in_flight_fast_access

let  store_inflight_writes b key wal_record =
	let _ = CCVector.push b.writes_in_flight wal_record in
    let hash_of_key = murmurhash  key (Int32.of_int (Bytes.length key)) (Int32.of_int 0) in
    b.writes_in_flight_fast_access <- b.writes_in_flight_fast_access
                                      |> InflightMap.add  hash_of_key wal_record

let batch b key value =

   Eio.Switch.run @@ fun sw ->
   Fiber.fork ~sw (fun () ->
   Eio.Mutex.use_rw ~protect:true b.mu (fun () ->
	 (* Writes in flight *)
	match (check_for_inflight_writes b key) with
   | Some _  -> ()
   | None  ->
        let node = create_snowflake_node (Int64.of_int 0) in
        let id = generate node in
        let wal_record =
         {
            key      = Bytes.to_string key;
            value    = value;
	        batch_id      =   (match id with | Ok v -> v | Error _ -> failwith "Unable to get snowflake id");
            wal_type = WalRecord
          } in
        store_inflight_writes b key wal_record
   )
   )

let  commit b env =

   Eio.Switch.run @@ fun sw ->
   Fiber.fork ~sw (fun () ->
   Eio.Mutex.use_rw ~protect:true b.mu (fun () ->

	if b.committed then
		raise ErrorBatchCommitted;


  let any_wal_record =  CCVector.get b.writes_in_flight 0 in
  let batch_id = any_wal_record.batch_id in
  let buffer = Buffer.create 200 in
  let write_to_stdout sink bytes = (*  TODO Why do we need a sink ?*)
    Eio.Buf_write.with_flow sink @@ fun bw ->
    Eio.Buf_write.bytes bw bytes
  in
   let _ = CCVector.fold ( fun buffer x ->

       let buf = Bin_prot.Utils.bin_dump ~header:false bin_writer_wal_record x in
       let _ = write_to_stdout (Eio.Flow.buffer_sink buffer) (to_bytes buf) in
       buffer
       ) buffer b.writes_in_flight
  in
       let buf = Bin_prot.Utils.bin_dump ~header:false bin_writer_wal_record (
                                     {
                                       key = String.empty;
                                       value = String.empty;
                                       batch_id = batch_id;
                                       wal_type = WalRecordEnd

                                     } ) in
       write_to_stdout (Eio.Flow.buffer_sink buffer) (to_bytes buf);
       (* Write to WAL *)
       (* write *)
       (*   (Buffer.to_bytes buffer) env *)
  )
  )
