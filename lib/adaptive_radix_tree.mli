open Types

module type RADIXOperator = sig
  type 'a radix_node = 'a
  include  module type of MakeRadixNode ( struct type 'a t = 'a radix_node end )
  val new_node4 : meta * node_type * bytes list * node array
  val new_node16 : meta * node_type * bytes  list * node array
  val add_child : bytes -> meta * node_type * bytes   list * node array ->
                  node ->  meta * node_type * bytes   list * node array
  val insert_tree :  tree -> Bytes.t list ->  int64 -> node
  val search_after_terminating : node -> Bytes.t list  -> int -> int64 option

end

module RADIXOp : RADIXOperator
