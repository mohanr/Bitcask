open Bigarray
open Bitcask__Datastore
open Bitcask__Snowflake
open Containers


type wal_record = {
	key      : Bytes.t;
	value    : Bytes.t ;
	batch_id : int64
}

module  Inflightmap = struct
  type t = int
  let compare v v1 =
    Int.compare v v1
end

module  Inflight_vector = CCVector

module InflightMap = CCMap.Make(Inflightmap)

type batch = {
	db        :       DatabaseOp.data_store;
	writes_in_flight  : wal_record Inflight_vector.vector;
	writes_in_flight_fast_access : int64 InflightMap.t;
	mu        :  Eio.Mutex.t;
	committed    :    bool;
	rolledback   :    bool;
	batchId      :   int64;
}

 (* A new batch *)
let   newbatch db =
 let node = create_snowflake_node (Int64.of_int 0) in
 let id = generate node in
    {
	db        =       db;
	writes_in_flight  = Inflight_vector.create();
	writes_in_flight_fast_access = InflightMap.empty;
    mu        =   Eio.Mutex.create();
	committed    =    false;
	rolledback   =    true;
	batchId      =   (match id with | Ok v -> v | Error _ -> failwith "Unable to get snowflake id");
}
