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
open Bin_prot.Std
open Types

module type WalWriter =
sig
  val write : ?file_path:string -> bytes -> Eio_unix.Stdenv.base -> unit
  val read :   unit-> string -> Eio_unix.Stdenv.base -> string
end

module Entry  (Wal : WalWriter) = struct

let read_entry file_path env = Wal.read file_path env
let write  data env = Wal.write  data env
type entry = {
  checksum : int32;
	key      : string list;
	value    : string ;
	deleted  : string;
	offset   :   int64;
	size  :  int64;
	tstamp  :    int64;
	keysize  :   int64;
	valuesize :  int64;
}
[@@deriving bin_io]

let calculate_sizes entry map =
  Fmt.pr "CRC :[ %d ]\n"  (Bytes.length (int32tobytes entry.checksum 8));
  let k = match (EntryMap.find "key" map) with
                          | ListValue l -> (Int.mul (List.length l) 8)
                          | IntValue _ -> 0
                          in
  Fmt.pr "Key :[ %d ]\n" k;
  Fmt.pr "Value :[ %d ]\n" (String.length entry.value );
  Fmt.pr "Deleted flag :[ %d ]\n" (String.length entry.deleted);
  Fmt.pr "Offset :[ %d ]\n"  (Bytes.length (int64tobytes entry.offset 8));
  Fmt.pr "Size :[ %d ]\n" (Bytes.length (int64tobytes entry.size 8));
  Fmt.pr "Timestamp :[ %d ]\n" (Bytes.length (int64tobytes entry.tstamp 8));
  Fmt.pr "Key Size :[ %d ]\n" (Bytes.length (int64tobytes entry.keysize 4));
  Fmt.pr "Value Size :[ %d ]\n" (Bytes.length (int64tobytes entry.valuesize 8))


let current_time_ns () =
    let time = Mtime_clock.now () in
    Mtime.to_uint64_ns time


type _ Effect.t +=
  | Check_sizes : entry -> entry Effect.t

let match_value map key =
          match (EntryMap.find key map) with
               | ListValue _ -> 0L
               | IntValue i -> i

let compose_entry map =
  let tstamp  =   current_time_ns () in
  let buffer = Buffer.create 40 in
    Buffer.add_int64_ne buffer
      (match_value map "key_size");
    Buffer.add_int64_ne buffer
      (match_value map "value_size");
    Buffer.add_int64_ne buffer
      tstamp ;
    Buffer.add_bytes buffer
          (match (EntryMap.find "key" map) with
               | ListValue l -> Bytes.of_string ( String.concat  String.empty l)
               | IntValue _ -> Bytes.of_string String.empty);
    Buffer.add_int64_ne buffer
        (match_value map "value");
  let new_entry = {
    checksum = Crc32.to_int32 (Crc32.digest_bytes
                                 (Buffer.to_bytes buffer) 0
                    (Bytes.length (Buffer.to_bytes buffer)) Crc32.default)  ;
		key =  (match (EntryMap.find "key" map) with
                 | ListValue l ->  l
                 | IntValue _ -> []);
		value = Int64.to_string(match_value map "value") ;
	  deleted  = Int64.to_string(match_value map "deleted") ;
    offset   = match_value map "offset" ;
    size  =  Int64.of_int (5 + 1 + 10 + 10 + 10); (*  TODO The values are not validated*)
    tstamp  =   tstamp;
    keysize  =  match_value map "key_size" ;
    valuesize = match_value map "value_size" ;
	} in
  ignore ( perform (Check_sizes new_entry) );
  new_entry

let create_entry map env =
  let new_entry = compose_entry map
    in
    let buf = Bin_prot.Utils.bin_dump ~header:false bin_writer_entry new_entry in
    Wal.write (Bigstring.to_bytes buf)  env


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
              let result = calculate_sizes entry map in (*  Remove 'map' parameter*)
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
