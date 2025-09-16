open Bigarray
open Bitcask__Datastore
open Bitcask__Snowflake
open Containers
open Bitcask__Murmurhash
open Eio.Std


type wal_record = {
	key      : Bytes.t;
	value    : Bytes.t ;
	batch_id : int64
}

module  Inflightmap = struct
  type t = Bytes.t
  let compare v v1 =
    Bytes.compare v v1
end

module  Inflight_vector = CCVector

module InflightMap = CCMap.Make(Inflightmap)

type batch = {
	db        :       DatabaseOp.data_store;
	writes_in_flight  : wal_record Inflight_vector.vector;
	mutable writes_in_flight_fast_access : wal_record InflightMap.t;
	mu        :  Eio.Mutex.t;
	committed    :    bool;
	rolledback   :    bool;
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
}

let check_for_inflight_writes b key  =

    InflightMap.find_opt key b.writes_in_flight_fast_access

let  store_inflight_writes b key wal_record =
	let _ = CCVector.push b.writes_in_flight wal_record in
    let hash_of_key = murmurhash key (Int32.of_int (Bytes.length key)) (Int32.of_int 0) in
    b.writes_in_flight_fast_access <- b.writes_in_flight_fast_access |> InflightMap.add (Bytes.of_string (Int32.to_string hash_of_key)) wal_record

let batch b put key value =

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
            key      = key;
            value    = value;
	        batch_id      =   (match id with | Ok v -> v | Error _ -> failwith "Unable to get snowflake id");
          } in
        store_inflight_writes b key wal_record
   )
   )
