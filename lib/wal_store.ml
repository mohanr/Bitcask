open Base
open Checkseum
open Stdlib
open Eio.Std
open Mtime_clock
open Containers
open Utils
open Hex
open Utils
open Mtime_clock
open Marshal
open Effect
open Effect.Deep

module type WalWriter =
sig
  val write : ?file_path:string -> bytes -> Eio_unix.Stdenv.base -> unit
  val read :   unit-> string -> Eio_unix.Stdenv.base -> string
end

module Entry  (Wal : WalWriter) = struct

type entry = {
  checksum : int32;
	key      : Bytes.t;
	value    : Bytes.t ;
	deleted  : Bytes.t;
	offset   :   int64;
	size  :  int64;
	tstamp  :    int64;
	keysize  :   int64;
	valuesize :  int64;
}
[@@deriving show]

let calculate_sizes entry =
  Fmt.pr "CRC :[ %d ]\n"  (Bytes.length (int32tobytes entry.checksum 8));
  Fmt.pr "Key :[ %d ]\n" (Bytes.length entry.key);
  Fmt.pr "Value :[ %d ]\n" (Bytes.length entry.value );
  Fmt.pr "Deleted flag :[ %d ]\n" (Bytes.length entry.deleted);
  Fmt.pr "Offset :[ %d ]\n"  (Bytes.length (int64tobytes entry.offset 8));
  Fmt.pr "Size :[ %d ]\n" (Bytes.length (int64tobytes entry.size 8));
  Fmt.pr "Timestamp :[ %d ]\n" (Bytes.length (int64tobytes entry.tstamp 8));
  Fmt.pr "Key Size :[ %d ]\n" (Bytes.length (int64tobytes entry.keysize 4));
  Fmt.pr "Value Size :[ %d ]\n" (Bytes.length (int64tobytes entry.valuesize 8));
module  Entrykeyvalue = struct
  type t = string
  let compare x x1 =          (*  TODO *)
    0
end

let read_entry file_path env = Wal.read() file_path env
let write  data env = Wal.write  data env

module EntryMap = CCMap.Make(Entrykeyvalue)

let current_time_ns () =
    let time = Mtime_clock.now () in
    Mtime.to_uint64_ns time


type _ Effect.t +=
  | Check_sizes : entry -> entry Effect.t

let compose_entry map =
  let entry_bytes =
    Bytes.cat
    (Bytes.cat
      (Bytes.cat (Bytes.cat (int64tobytes (EntryMap.find "keysize" map) 8)
                            (int64tobytes (EntryMap.find "valuesize" map) 8))
                 (int64tobytes (EntryMap.find "tstamp" map) 8))
      (int64tobytes (EntryMap.find "key" map) 8))
   (int64tobytes (EntryMap.find "value" map) 8)
  in
  let new_entry = {
    checksum = Crc32.to_int32 (Crc32.digest_bytes entry_bytes 0 (Bytes.length entry_bytes) Crc32.default)  ;
		key = int64tobytes (EntryMap.find "key" map ) 8;
		value = int64tobytes(EntryMap.find "value" map) 8 ;
	  deleted  = int64tobytes(EntryMap.find "deleted" map) 8 ;
    offset   = EntryMap.find "offset" map ;
    size  =  Int64.of_int (5 + 1 + 10 + 10 + 10); (*  TODO The values are not validated*)
    tstamp  =   current_time_ns ();
    keysize  =  EntryMap.find "key_size" map ;
    valuesize = EntryMap.find "value_size" map ;
	} in
  ignore ( perform (Check_sizes new_entry) );
  new_entry

let create_entry map env =
  let new_entry = compose_entry map
    in
    Wal.write (Marshal.to_bytes new_entry [])  env


let entry_handler  map =
  match_with (fun () -> compose_entry map )
  ()
  {
    retc =
      (fun result ->  result);
    exnc = (fun e -> raise e);
    effc =
      (fun (type c) (e : c Effect.t) ->
        match e with
        | Check_sizes entry ->
            Some
              (fun (k : (c, _) continuation) ->
              let result = calculate_sizes entry in
              continue k entry
              )
      | _ -> None
      );
 }


end

module WalWriter = struct


let write ?(file_path="/Users/anu/Documents/rays/Bitcask/bitcask"

) (data : bytes) env =
  let ( / ) = Eio.Path.( / ) in
  let path = Eio.Stdenv.fs env  in

  let p = path / file_path / "wal.log" in

  Eio.Path.with_open_out ~append:true ~create:(`If_missing 0o600) p (fun f ->
  try
   let bytes_written =
   Eio.Flow.single_write f  [Cstruct.of_bytes
                               data]
   in Printf.printf "%d bytes written" bytes_written
  with
  |  ex -> traceln "%a" Eio.Exn.pp ex
  )

let read () file_path env =
let ( / ) = Eio.Path.( / ) in
  let path = Eio.Stdenv.fs env in
  let p = path / file_path in
  let lines =
  if  (Sys.file_exists file_path) then
   Eio.Path.load p
  else
     raise Not_found
  in lines

end

module DataEntryOp = Entry( WalWriter )
