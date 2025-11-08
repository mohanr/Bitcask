open Types
open Bitcask__Wal_store.DataEntryOp

module type DATASTOREOperator = sig
type data_store = Types.data_store
  val create_data_store : string -> data_store
  val create_entry_map  : data_store  -> string list -> int -> entry_map_value EntryMap.t
end

module DatabaseOp  : DATASTOREOperator
