open Bigarray
open Bitcask__Datastore
open Bitcask__Snowflake


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

module InflightMap = CCMap.Make(Inflightmap)

type batch = {
	db        :       DatabaseOp.data_store;
	writes_in_flight  : (wal_record, wal_record, c_layout)  Array1.t;
	writes_in_flight_fast_access : int64 InflightMap.t;
	mu        :  Eio.Mutex.t;
	committed    :    bool;
	rolledback   :    bool;
	batchId      :   node;
}
