open! Core

type 'a t = { mutable len : int; mutable arr : 'a array; default : 'a }
[@@deriving sexp]

let to_array t = Array.sub t.arr ~pos:0 ~len:t.len

let sexp_of_t f t =
  sexp_of_t f { len = t.len; arr = to_array t; default = t.default }

let of_array arr ~default = { len = Array.length arr; arr; default }

let create ~cap ~default =
  { len = 0; arr = Array.init cap ~f:(fun _ -> default); default }

let at t n = t.arr.(n)
let set t n v = t.arr.(n) <- v

let remove t n =
  assert (n >= 0);
  assert (n < t.len);
  t.arr.(n) <- t.arr.(t.len - 1);
  t.len <- t.len - 1

let push t v =
  (if Int.equal t.len (Array.length t.arr) then
   let new_len = Int.max (Array.length t.arr * 2) 16 in
   let arr =
     Array.init new_len ~f:(fun i -> if i < t.len then t.arr.(i) else t.default)
   in
   t.arr <- arr);
  t.arr.(t.len) <- v;
  t.len <- t.len + 1

let extend t l = List.iter l ~f:(fun v -> push t v)
let is_empty t = Int.equal t.len 0
let length t = t.len

let find t ~f =
  Array.findi t.arr ~f:(fun i v -> if i < t.len then f v else false)
  |> Option.map ~f:snd
