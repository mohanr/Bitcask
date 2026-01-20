(* open Bitcask__Snowflake *)

(* let%expect_test _= *)
(* (\* https://github.com/daypack-dev/timere/blob/main/examples/date_time.ml *\) *)

(* let em = epoch_millis () in *)
(* Fmt.pr "%a@." (Timedesc.Timestamp.pp  ()) em; *)
(* let timedesc = Timedesc.make_exn ~tz:(Timedesc.Time_zone.make_exn "UTC") ~year:2010 ~month:11 ~day:4 ~hour:1 ~minute:42 ~second:54 () in *)
(* let time_as_float = (Timedesc.Span.sub  (Timedesc.Timestamp.now()) (Timedesc.to_timestamp_single timedesc) ) in *)
(* Fmt.pr "%f\n"  (Timedesc.Timestamp.to_float_s time_as_float); *)
(* Fmt.pr "%f\n"  (Timedesc.to_timestamp_float_s_single timedesc); *)
(* Fmt.pr "%f"  (Timedesc.to_timestamp_float_s_single (Timedesc.now())); *)

(* [%expect {| *)
(*   2010 Nov 04 01:42:54 +00:00:00 *)
(*   474636063.958851 *)
(*   1288834974.000000 *)
(*   1763471037.958871 *)
(*   |}] *)

(* let%expect_test "Test Monotonic clock"= *)
(*       Printf.printf "%L" Int64.(to_int (Mtime.Span.to_uint64_ns monotonic_clock)); *)
(*     (\* [%expect {| 4912999253692 |}] *\) *)
(*     [%expect {| 3556458848692 |}] *)

(* let%expect_test "Time Since"= *)
(*     let  millis  =  time_since () in *)
(*     Fmt.pr "%Ld" millis; *)
(*     [%expect {| 474636063 |}] *)

(* let%expect_test "Test Duplicate ID"= *)

(* 	let node = create_snowflake_node (Int64.of_int 0) in *)
(* 	(\* for _ = 0 to 10 do *\) *)

(* 		let _id = generate node in *)
(*         (); *)
(*     (\* done; *\) *)
(*   [%expect {| *)
(*     node.step is set to 0 *)
(*     node.time is set from 0 to 470674519 *)
(*     |}] *)
