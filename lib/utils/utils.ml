open Bytes
open Int64

module Utils = struct

let int64tobytes v  l : Bytes.t =
	let b   = Bytes.create l in
    for i = 0 to l - 1 do
        let f = 8 * i in
        let byte = Int64.to_int (logand (shift_right_logical v  f) 0xffL) in
        Bytes.set b i (Char.chr byte)
    done;
    b

let int32tobytes v  l : Bytes.t =
	let b   = Bytes.create l in
    for i = 0 to l - 1 do
        let f = 8 * i in
        let byte = Int32.to_int (Int32.logand (Int32.shift_right_logical v  f) 0xffl) in
        Bytes.set b i (Char.chr byte)
    done;
    b
end
