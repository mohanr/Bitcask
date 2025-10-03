open Bitcask__Datastore.DatabaseOp
open Bitcask__Wal_store.DataEntryOp
open Bitcask__Batch

module type KEYVALUE = sig
  type key_value
  type flow
  val key : key_value
  val value : key_value
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

let create_keyvalue  (type t) (type f) k v flow  =
  let module Key_value = struct
    type key_value = t
    type flow = f
    let key =  k
    let value = v
    let flow_type = flow
  end in
  (module Key_value : KEYVALUE with type key_value = t and type flow = f)

let write_to_stdout stdout bytes =
  Eio.Buf_write.with_flow stdout @@ fun bw ->
  Eio.Buf_write.bytes bw bytes

let test_write_entry buffer (module M : KEYVALUE with type key_value = int and type flow = Eio_mock.Flow.t) =
  let new_db = create_data_store "~/Documents/rays/Bitcask/bitcask/" in
  let bytes = (Marshal.to_bytes (entry_handler (create_entry_map new_db M.key M.value))  []) in
  Fmt.pr " Length %d\n"   (Bytes.length bytes);
  let _ = write_to_stdout (Eio.Flow.buffer_sink buffer) bytes
  in
  buffer


let%expect_test "Test Set and Get keys"=
 Eio_main.run @@ fun _env ->
 let buffer = Buffer.create 200 in
 let mockwalwriter =            (*  First-class module *)
  (create_keyvalue 1 2  (Eio_mock.Flow.make "mock-stdout")
    : (module KEYVALUE with type key_value = int and type flow = Eio_mock.Flow.t)) in
  let buffer = test_write_entry buffer mockwalwriter in
  let bytes = Buffer.to_bytes buffer in
  let entry = Marshal.from_bytes bytes 0 in
  Fmt.pr " %a\n"   pp_entry entry ;
  [%expect {|
    CRC :[ 8 ]
    Key :[ 8 ]
    Value :[ 8 ]
    Deleted flag :[ 8 ]
    Offset :[ 8 ]
    Size :[ 8 ]
    Timestamp :[ 8 ]
    Key Size :[ 4 ]
    Value Size :[ 8 ]
     Length 100

    { Wal_store.Entry.checksum = 1215298682l;
      key = "\002\000\000\000\000\000\000\000";
      value = "\002\000\000\000\000\000\000\000";
      deleted = "\002\000\000\000\000\000\000\000"; offset = 2L; size = 36L;
      tstamp = 4793104302625L; keysize = 2L; valuesize = 2L }
    |}]

let%expect_test "Check sizes to decide offsets"=
 Eio_main.run @@ fun _env ->
 let buffer = Buffer.create 200 in
 let mockwalwriter =            (*  First-class module *)
  (create_keyvalue 1 2  (Eio_mock.Flow.make "mock-stdout")
    : (module KEYVALUE with type key_value = int and type flow = Eio_mock.Flow.t)) in
  let buffer = test_write_entry buffer mockwalwriter in
  let bytes = Buffer.to_bytes buffer in
  let _ = Marshal.from_bytes bytes 0 in
  Printf.printf  "%s" "Checking sizes";
  [%expect {|
    CRC :[ 8 ]
    Key :[ 8 ]
    Value :[ 8 ]
    Deleted flag :[ 8 ]
    Offset :[ 8 ]
    Size :[ 8 ]
    Timestamp :[ 8 ]
    Key Size :[ 4 ]
    Value Size :[ 8 ]
     Length 100
    Checking sizes
    |}]

module DatabaseOp = Bitcask__Datastore.DatabaseOp

let%expect_test "Batch commit and index"=
Eio_main.run @@ fun env ->
  let new_batch = newbatch (module  DatabaseOp) in
  let () =  batch new_batch (Bytes.make 1 (Char.chr 1))  (Bytes.make 1 (Char.chr 2)) in
  commit new_batch env;
  [%expect {|
     node.step is set to 0
     node.time is set from 0 to 470674519
     node.step is set to 0
     node.time is set from 0 to 470674519
    75 bytes written
    |}]
