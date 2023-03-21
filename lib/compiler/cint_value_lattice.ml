open! Core
module CInt = Act.CInt

let max_cint_of_width bits = CInt.(sub (pow two (of_int bits)) one)

module MM = struct
  type t = { (* incl *) min : CInt.t; (* incl *) max : CInt.t }
  [@@deriving sexp, equal]

  let create ~min ~max =
    assert (CInt.le min max);
    { min; max }

  let create_const v = create ~min:v ~max:v
  let create_bitwidth bits = create ~min:CInt.zero ~max:(max_cint_of_width bits)
  let get_const_val { min; max } = if CInt.eq min max then Some min else None

  let union c1 c2 =
    let min = CInt.min c1.min c2.min in
    let max = CInt.max c1.max c2.max in
    create ~min ~max

  let inter_exn c1 c2 =
    let min = CInt.max c1.min c2.min in
    let max = CInt.min c1.max c2.max in
    create ~min ~max

  (* math *)
  let zero = create_const CInt.zero
  let one = create_const CInt.one

  let add al bl =
    let min = CInt.add al.min bl.min in
    let max = CInt.add al.max bl.max in
    create ~min ~max

  let sub_no_wrap al bl =
    let sub_clamped_at_0 x y =
      if CInt.(y >= x) then CInt.zero else CInt.sub x y
    in
    let min = sub_clamped_at_0 al.min bl.max in
    let max = sub_clamped_at_0 al.max bl.min in
    create ~min ~max

  let mul al bl =
    let min = CInt.mul al.min bl.min in
    let max = CInt.mul al.max bl.max in
    create ~min ~max

  let div al bl =
    let min = CInt.div al.min (CInt.max CInt.one bl.max) in
    let max = CInt.div al.max (CInt.max CInt.one bl.min) in
    create ~min ~max

  let mod_ al bl =
    let min = CInt.zero in
    let max = CInt.min al.max bl.max in
    create ~min ~max

  let lshift al bl =
    let min = CInt.left_shift al.min ~amt:bl.min in
    let max = CInt.left_shift al.max ~amt:bl.max in
    create ~min ~max

  let rshift al bl =
    let min = CInt.right_shift al.min ~amt:bl.max in
    let max = CInt.right_shift al.max ~amt:bl.min in
    create ~min ~max

  let bit_and al bl =
    let min = CInt.zero in
    let max = CInt.min al.max bl.max in
    create ~min ~max

  let bit_or al bl =
    let min = CInt.min al.min bl.min in
    let max =
      let bitwidth = Int.max (CInt.bitwidth al.max) (CInt.bitwidth bl.max) in
      CInt.min (CInt.add al.max bl.max) (max_cint_of_width bitwidth)
    in
    create ~min ~max

  let bit_xor al bl =
    let min = CInt.min al.min bl.min in
    let max =
      let bitwidth = Int.max (CInt.bitwidth al.max) (CInt.bitwidth bl.max) in
      CInt.min (CInt.add al.max bl.max) (max_cint_of_width bitwidth)
    in
    create ~min ~max

  let clip el ~bits =
    let min = CInt.zero in
    let max = CInt.min el.max (max_cint_of_width bits) in
    create ~min ~max

  let concat els =
    List.fold els ~init:(0, CInt.zero, CInt.zero)
      ~f:(fun (acc, min_tot, max_tot) (lat, bits) ->
        let lat = clip lat ~bits in
        ( acc + bits,
          CInt.add min_tot (CInt.left_shift lat.min ~amt:(CInt.of_int acc)),
          CInt.add max_tot (CInt.left_shift lat.max ~amt:(CInt.of_int acc)) ))
    |> fun (_, min, max) -> create ~min ~max

  let log2_one_hot el =
    let min = CInt.div el.min CInt.two |> CInt.bitwidth |> CInt.of_int in
    let max = CInt.bitwidth el.max |> CInt.of_int in
    create ~min ~max

  let eq al bl =
    if CInt.eq al.min al.max && CInt.eq bl.min bl.max && CInt.eq al.min bl.min
    then one
    else if CInt.lt al.max bl.min && CInt.gt al.min bl.max then zero
    else create_bitwidth 1

  let ne al bl =
    if CInt.eq al.min al.max && CInt.eq bl.min bl.max && CInt.eq al.min bl.min
    then zero
    else if CInt.lt al.max bl.min && CInt.gt al.min bl.max then one
    else create_bitwidth 1

  let lt al bl =
    if CInt.lt al.max bl.min then one
    else if CInt.ge al.min bl.max then zero
    else create_bitwidth 1

  let le al bl =
    if CInt.le al.max bl.min then one
    else if CInt.gt al.min bl.max then zero
    else create_bitwidth 1

  let gt al bl =
    if CInt.gt al.min bl.max then one
    else if CInt.le al.max bl.min then zero
    else create_bitwidth 1

  let ge al bl =
    if CInt.ge al.min bl.max then one
    else if CInt.lt al.max bl.min then zero
    else create_bitwidth 1

  let eq0 al =
    if CInt.le al.max CInt.zero then one
    else if CInt.gt al.min CInt.zero then zero
    else create_bitwidth 1
end

module Bit = struct
  type t = One | Zero | Unknown [@@deriving sexp, equal]
end

module Bit_arr : sig
  type t [@@deriving sexp, equal]

  val of_array : Bit.t array -> t

  (* val get : t -> int -> Bit.t *)
  val length : t -> int
  val to_array : ?len:int -> t -> Bit.t array
  val max_val : t -> CInt.t
  val min_val : t -> CInt.t
  val pos_vals : t -> CInt.t list
end = struct
  (* higher bits are implicitly zero *)
  type t = Bit.t array [@@deriving sexp, equal]

  let length t = Array.length t

  let get t i =
    assert (i >= 0);
    if i < Array.length t then t.(i) else Bit.Zero

  let of_array t =
    let n =
      Array.mapi t ~f:(fun i b -> (i, b))
      |> Array.filter ~f:(fun (_, b) ->
             match b with Bit.Zero -> false | _ -> true)
      |> Array.map ~f:fst
      |> Array.max_elt ~compare:Int.compare
      |> Option.value ~default:(-1)
    in
    Array.sub t ~pos:0 ~len:(n + 1)

  let to_array ?len t =
    let len = Option.value len ~default:(length t) in
    assert (len >= length t);
    Array.init len ~f:(fun i -> get t i)

  let max_val t =
    Array.mapi t ~f:(fun i b ->
        match b with
        | Bit.Zero -> CInt.zero
        | Unknown | One -> CInt.(pow two (of_int i)))
    |> Array.fold ~init:CInt.zero ~f:CInt.add

  let min_val t =
    Array.mapi t ~f:(fun i b ->
        match b with
        | Bit.Zero | Unknown -> CInt.zero
        | One -> CInt.(pow two (of_int i)))
    |> Array.fold ~init:CInt.zero ~f:CInt.add

  let pos_vals t =
    let rec h bits =
      match bits with
      | [] -> [ CInt.zero ]
      | bit :: bits -> (
          let l = h bits in
          let on_l = List.map l ~f:(fun x -> CInt.(mul two x |> add one)) in
          let off_l = List.map l ~f:(fun x -> CInt.(mul two x)) in
          match bit with
          | Bit.One -> on_l
          | Zero -> off_l
          | Unknown -> on_l @ off_l)
    in
    h (to_array t |> Array.to_list)
end

module BB = struct
  type t = Bit_arr.t [@@deriving sexp, equal]

  let create_const v =
    Array.init (CInt.bitwidth v) ~f:(fun i ->
        if
          CInt.right_shift v ~amt:(CInt.of_int i)
          |> CInt.bit_and CInt.one |> CInt.eq CInt.one
        then Bit.One
        else Zero)
    |> Bit_arr.of_array

  let create_bitwidth bits =
    Array.init bits ~f:(fun _ -> Bit.Unknown) |> Bit_arr.of_array

  let create_min_max ~min ~max =
    assert (CInt.le min max);
    if CInt.equal min max then create_const min
    else CInt.bitwidth max |> create_bitwidth

  let get_const_val t =
    let min, max = (Bit_arr.min_val t, Bit_arr.max_val t) in
    if CInt.eq min max then Some min else None

  let union c1 c2 =
    let len = Int.max (Bit_arr.length c1) (Bit_arr.length c2) in
    let c1 = Bit_arr.to_array ~len c1 in
    let c2 = Bit_arr.to_array ~len c2 in
    Array.zip_exn c1 c2
    |> Array.map ~f:(fun (b1, b2) ->
           match (b1, b2) with
           | Bit.One, Bit.One -> Bit.One
           | Zero, Zero -> Zero
           | _ -> Unknown)
    |> Bit_arr.of_array

  let inter_exn c1 c2 =
    let len = Int.max (Bit_arr.length c1) (Bit_arr.length c2) in
    let c1 = Bit_arr.to_array ~len c1 in
    let c2 = Bit_arr.to_array ~len c2 in
    Array.zip_exn c1 c2
    |> Array.map ~f:(fun (b1, b2) ->
           match (b1, b2) with
           | One, One -> Bit.One
           | Zero, Zero -> Zero
           | One, Zero | Zero, One -> failwith "intersection is empty"
           | Unknown, Zero | Zero, Unknown -> Zero
           | Unknown, One | One, Unknown -> One
           | Unknown, Unknown -> Unknown)
    |> Bit_arr.of_array

  let lshift' c1 ~amt =
    let c1 = Bit_arr.to_array c1 |> Array.to_list in
    let zeros = List.init amt ~f:(fun _ -> Bit.Zero) in
    zeros @ c1 |> Array.of_list |> Bit_arr.of_array

  let rshift' c1 ~amt =
    if amt > Bit_arr.length c1 then create_const CInt.zero
    else
      Bit_arr.to_array c1
      |> Array.sub ~pos:amt ~len:(Bit_arr.length c1 - amt)
      |> Bit_arr.of_array

  let add c1 c2 =
    let len = 1 + Int.max (Bit_arr.length c1) (Bit_arr.length c2) in
    let c1 = Bit_arr.to_array ~len c1 in
    let c2 = Bit_arr.to_array ~len c2 in
    let carry, bits =
      Array.zip_exn c1 c2
      |> Array.fold_map ~init:Bit.Zero ~f:(fun carry (b1, b2) ->
             let min_v b = match b with Bit.One -> 1 | Unknown | Zero -> 0 in
             let max_v b = match b with Bit.One | Unknown -> 1 | Zero -> 0 in
             let min = min_v carry + min_v b1 + min_v b2 in
             let max = max_v carry + max_v b1 + max_v b2 in
             let b =
               if Int.equal min max then
                 if Int.bit_and min 1 |> Int.equal 0 then Bit.Zero else Bit.One
               else Unknown
             in
             let carry =
               if min >= 2 then Bit.One else if max <= 1 then Zero else Unknown
             in
             (carry, b))
    in
    assert (Bit.equal carry Zero);
    Bit_arr.of_array bits

  let sub_no_wrap al _ =
    let length = Bit_arr.length al in
    create_bitwidth length

  let mul al bl =
    let length = Bit_arr.length al + Bit_arr.length bl in
    create_bitwidth length

  let div al _ =
    let length = Bit_arr.length al in
    create_bitwidth length

  let mod_ al bl =
    let length = Int.min (Bit_arr.length al) (Bit_arr.length bl) in
    create_bitwidth length

  let bit_and c1 c2 =
    let len = Int.max (Bit_arr.length c1) (Bit_arr.length c2) in
    let c1 = Bit_arr.to_array ~len c1 in
    let c2 = Bit_arr.to_array ~len c2 in
    Array.zip_exn c1 c2
    |> Array.map ~f:(fun (b1, b2) ->
           match (b1, b2) with
           | One, One -> Bit.One
           | _, Zero | Zero, _ -> Zero
           | _ -> Unknown)
    |> Bit_arr.of_array

  let bit_or c1 c2 =
    let len = Int.max (Bit_arr.length c1) (Bit_arr.length c2) in
    let c1 = Bit_arr.to_array ~len c1 in
    let c2 = Bit_arr.to_array ~len c2 in
    Array.zip_exn c1 c2
    |> Array.map ~f:(fun (b1, b2) ->
           match (b1, b2) with
           | One, _ | _, One -> Bit.One
           | Zero, Zero -> Zero
           | _ -> Unknown)
    |> Bit_arr.of_array

  let bit_xor c1 c2 =
    let len = Int.max (Bit_arr.length c1) (Bit_arr.length c2) in
    let c1 = Bit_arr.to_array ~len c1 in
    let c2 = Bit_arr.to_array ~len c2 in
    Array.zip_exn c1 c2
    |> Array.map ~f:(fun (b1, b2) ->
           match (b1, b2) with
           | One, Zero | Zero, One -> Bit.One
           | Zero, Zero | One, One -> Zero
           | _ -> Unknown)
    |> Bit_arr.of_array

  let lshift c1 c2 =
    (* This protects us from allocating super large arrays. It is probably a bug
       if this ever happens *)
    assert (Bit_arr.length c2 < 16);
    if CInt.lt (Bit_arr.max_val c2) (CInt.of_int 1024) then
      Bit_arr.pos_vals c2
      |> List.map ~f:CInt.to_int_exn
      |> List.map ~f:(fun amt -> lshift' c1 ~amt)
      |> List.reduce_exn ~f:union
    else
      let length = Bit_arr.length c1 + Int.pow 2 (Bit_arr.length c2) - 1 in
      create_bitwidth length

  let rshift c1 c2 =
    Bit_arr.pos_vals c2
    |> List.map ~f:CInt.to_int_exn
    |> List.map ~f:(fun amt -> rshift' c1 ~amt)
    |> List.reduce_exn ~f:union

  let clip el ~bits =
    let len = Int.max bits (Bit_arr.length el) in
    let el = Bit_arr.to_array ~len el in
    Array.sub el ~pos:0 ~len:bits |> Bit_arr.of_array

  let concat els =
    List.map els ~f:(fun (lat, bits) ->
        clip lat ~bits |> Bit_arr.to_array ~len:bits)
    |> Array.concat |> Bit_arr.of_array

  let log2_one_hot el =
    let length = Bit_arr.length el |> Int.ceil_log2 in
    create_bitwidth length

  let eq _ _ = create_bitwidth 1
  let ne _ _ = create_bitwidth 1
  let lt _ _ = create_bitwidth 1
  let le _ _ = create_bitwidth 1
  let gt _ _ = create_bitwidth 1
  let ge _ _ = create_bitwidth 1
  let eq0 _ = create_bitwidth 1
end

type t = { m : MM.t; b : BB.t } [@@deriving sexp, equal]

let create (m : MM.t) (b : BB.t) =
  let b' = BB.create_min_max ~min:m.min ~max:m.max in
  let m' = MM.create ~min:(Bit_arr.min_val b) ~max:(Bit_arr.max_val b) in
  let m = MM.inter_exn m m' in
  let b = BB.inter_exn b b' in
  { m; b }

let get_const_val t =
  match MM.get_const_val t.m with
  | Some v -> Some v
  | None -> BB.get_const_val t.b

(* math *)
let add c1 c2 = create (MM.add c1.m c2.m) (BB.add c1.b c2.b)

let sub_no_wrap c1 c2 =
  create (MM.sub_no_wrap c1.m c2.m) (BB.sub_no_wrap c1.b c2.b)

let mul c1 c2 = create (MM.mul c1.m c2.m) (BB.mul c1.b c2.b)
let div c1 c2 = create (MM.div c1.m c2.m) (BB.div c1.b c2.b)
let mod_ c1 c2 = create (MM.mod_ c1.m c2.m) (BB.mod_ c1.b c2.b)
let lshift c1 c2 = create (MM.lshift c1.m c2.m) (BB.lshift c1.b c2.b)
let rshift c1 c2 = create (MM.rshift c1.m c2.m) (BB.rshift c1.b c2.b)
let bit_and c1 c2 = create (MM.bit_and c1.m c2.m) (BB.bit_and c1.b c2.b)
let bit_or c1 c2 = create (MM.bit_or c1.m c2.m) (BB.bit_or c1.b c2.b)
let bit_xor c1 c2 = create (MM.bit_xor c1.m c2.m) (BB.bit_xor c1.b c2.b)
let clip c1 ~bits = create (MM.clip c1.m ~bits) (BB.clip c1.b ~bits)
let log2_one_hot c1 = create (MM.log2_one_hot c1.m) (BB.log2_one_hot c1.b)
let eq c1 c2 = create (MM.eq c1.m c2.m) (BB.eq c1.b c2.b)
let ne c1 c2 = create (MM.ne c1.m c2.m) (BB.ne c1.b c2.b)
let lt c1 c2 = create (MM.lt c1.m c2.m) (BB.lt c1.b c2.b)
let le c1 c2 = create (MM.le c1.m c2.m) (BB.le c1.b c2.b)
let gt c1 c2 = create (MM.gt c1.m c2.m) (BB.gt c1.b c2.b)
let ge c1 c2 = create (MM.ge c1.m c2.m) (BB.ge c1.b c2.b)
let eq0 c1 = create (MM.eq0 c1.m) (BB.eq0 c1.b)

let concat els =
  let els_m = List.map els ~f:(fun (el, bits) -> (el.m, bits)) in
  let els_b = List.map els ~f:(fun (el, bits) -> (el.b, bits)) in
  create (MM.concat els_m) (BB.concat els_b)

(* public ops *)

let union c1 c2 = create (MM.union c1.m c2.m) (BB.union c1.b c2.b)

let create_bitwidth bitwidth =
  create (MM.create_bitwidth bitwidth) (BB.create_bitwidth bitwidth)

let create_const c = create (MM.create_const c) (BB.create_const c)

let eval_rewrite_expr e ~of_var =
  let rec f e =
    let lat, e =
      match e with
      | F_expr.Var v -> (of_var v, F_expr.Var v)
      | Const c -> (create_const c, Const c)
      | Add (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (add al bl, Add (a, b))
      | Sub_no_wrap (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (sub_no_wrap al bl, Sub_no_wrap (a, b))
      | Mul (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (mul al bl, Mul (a, b))
      | Div (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (div al bl, Div (a, b))
      | Mod (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (mod_ al bl, Mod (a, b))
      | LShift (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (lshift al bl, LShift (a, b))
      | RShift (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (rshift al bl, RShift (a, b))
      | BitAnd (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (bit_and al bl, BitAnd (a, b))
      | BitOr (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (bit_or al bl, BitOr (a, b))
      | BitXor (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (bit_xor al bl, BitXor (a, b))
      | Clip (e, bits) ->
          let el, e = f e in
          (clip el ~bits, Clip (e, bits))
      | Concat es ->
          let els, es =
            List.map es ~f:(fun (e, bits) ->
                let el, e = f e in
                ((el, bits), (e, bits)))
            |> List.unzip
          in
          (concat els, Concat es)
      | Log2OneHot e ->
          let el, e = f e in
          (log2_one_hot el, Log2OneHot e)
      | Eq (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (eq al bl, Eq (a, b))
      | Ne (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (ne al bl, Ne (a, b))
      | Lt (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (lt al bl, Lt (a, b))
      | Le (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (le al bl, Le (a, b))
      | Gt (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (gt al bl, Gt (a, b))
      | Ge (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          (ge al bl, Ge (a, b))
      | Eq0 a ->
          let al, a = f a in
          (eq0 al, Eq0 a)
    in
    let lat : t = lat in
    let e = match get_const_val lat with None -> e | Some v -> Const v in
    (lat, e)
  in
  f e

let eval_expr e ~of_var = eval_rewrite_expr e ~of_var |> fst
let rewrite_expr e ~of_var = eval_rewrite_expr e ~of_var |> snd
