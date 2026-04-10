open Bitcask__Datastore.DatabaseOp
open Bitcask__Wal_store.DataEntryOp
open Bitcask__Adaptive_radix_tree.RADIXOp
open Bitcask__Batch
open Bigstring

module type KEYVALUE = sig
  type key
  type value
  type flow
  val key : key
  val value : value
  val flow_type : flow
end

module type DataFlow = sig
  type value
  val flow : value
end

let make (type f) flow : (module DataFlow with type value = f) =
  (module struct
    type value  = f
    let flow = flow
  end)

let create_keyvalue  (type t1) (type t) (type f) k v flow  =
  let module Key_value = struct
    type key = t
    type value = t1
    type flow = f
    let key =  k
    let value = v
    let flow_type = flow
  end in
  (module Key_value : KEYVALUE with type key = t and type value = t1 and type flow = f)

let write_to_stdout stdout bytes =
  Eio.Buf_write.with_flow stdout @@ fun bw ->
  Eio.Buf_write.bytes bw bytes

let test_write_entry buffer (module M : KEYVALUE with type key = string list and type value = int and type flow = Eio_mock.Flow.t) =
  let new_db = create_data_store "~/Documents/rays/Bitcask/bitcask/" in
  let entry = entry_handler (create_entry_map new_db M.key M.value)  in
  let buf = Bin_prot.Utils.bin_dump ~header:false bin_writer_entry entry in
  let _ = write_to_stdout (Eio.Flow.buffer_sink buffer) (to_bytes buf)
  in
  buffer


let%expect_test "Test Set and Get keys"=
 Eio_main.run @@ fun _env ->
 let buffer = Buffer.create 200 in
 let mockwalwriter =            (*  First-class module *)
  (create_keyvalue ["M"] 4  (Eio_mock.Flow.make "mock-stdout")
    : (module KEYVALUE with type key = string list and type value = int and type flow = Eio_mock.Flow.t)) in
  let buffer = test_write_entry buffer mockwalwriter in
  let bytes = Buffer.to_bytes buffer in
  let buf = Bigstring.of_bytes bytes  in
  let pos = ref 0 in
  let entry = bin_read_entry buf ~pos_ref:pos in
  Printf.printf "Crc %lu\n" entry.checksum;
  Printf.printf "Key %s\n" (String.concat String.empty entry.key );
  Printf.printf "Value %s\n" entry.value;
  [%expect {|
    CRC :[ 8 ]
    Key :[ 8 ]
    Value :[ 1 ]
    Deleted flag :[ 2 ]
    Offset :[ 8 ]
    Size :[ 8 ]
    Timestamp :[ 8 ]
    Key Size :[ 4 ]
    Value Size :[ 8 ]
    Crc 1667700190
    Key M
    Value 4
    |}]

let%expect_test "Check sizes to decide offsets"=
 Eio_main.run @@ fun _env ->
 let buffer = Buffer.create 200 in
 let mockwalwriter =            (*  First-class module *)
  (create_keyvalue ["M"] 4  (Eio_mock.Flow.make "mock-stdout")
    : (module KEYVALUE with type key = string list and type value = int and type flow = Eio_mock.Flow.t)) in

  let buffer = test_write_entry buffer mockwalwriter in
  let bytes = Buffer.to_bytes buffer in
  let buf = Bigstring.of_bytes bytes  in
  let pos = ref 0 in
  let _ = bin_read_entry buf ~pos_ref:pos in
  Printf.printf  "%s" "Checking sizes";
  [%expect {|
    CRC :[ 8 ]
    Key :[ 8 ]
    Value :[ 1 ]
    Deleted flag :[ 2 ]
    Offset :[ 8 ]
    Size :[ 8 ]
    Timestamp :[ 8 ]
    Key Size :[ 4 ]
    Value Size :[ 8 ]
    Checking sizes
    |}]

module DatabaseOp = Bitcask__Datastore.DatabaseOp

let empty_tree =
    let root = Empty in
    {root = root; size = 0}

let%expect_test "Batch commit and index"=
Eio_main.run @@ fun _env ->
  let new_batch = newbatch (module  DatabaseOp) in
  let () =  batch new_batch (Bytes.make 1 (Char.chr 1) )  "2" in
  commit new_batch empty_tree;
  [%expect {|
    Generating Snowflake Id
    Generating Snowflake Id
     node.step is set to 0
     node.time is set from 0 to 486914206
     node.step is set to 0
     node.time is set from 0 to 486914206
    Length of WAL buffer 10Inserting key: 
    Searching leavesSearch key  compared with   Found ''  2
    |}]
