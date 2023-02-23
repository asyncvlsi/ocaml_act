open! Core
module CInt = Act.CInt

type t = { (* incl *) min : CInt.t; (* incl *) max : CInt.t }
[@@deriving sexp, equal]

let max_cint_of_width bits = CInt.(sub (pow two (of_int bits)) one)
let create ~min ~max = { min; max }
let create_const v = create ~min:v ~max:v
let zero = create_const CInt.zero
let one = create_const CInt.one
let create_bitwidth bits = create ~min:CInt.zero ~max:(max_cint_of_width bits)

let union c1 c2 =
  let min = CInt.min c1.min c2.min in
  let max = CInt.max c1.max c2.max in
  create ~min ~max

let eval_rewrite_expr e ~of_var =
  let rec f e =
    let lat, e =
      match e with
      | Expr.Var v -> (of_var v, Expr.Var v)
      | Const c -> (create_const c, Const c)
      | Add (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let min = CInt.add al.min bl.min in
          let max = CInt.add al.max bl.max in
          let nn = Expr.Add (a, b) in
          (create ~min ~max, nn)
      | Sub_no_wrap (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let min =
            if CInt.(bl.max >= al.min) then CInt.zero
            else CInt.sub al.min bl.max
          in
          let max = CInt.sub al.max bl.min in
          let nn = Expr.Sub_no_wrap (a, b) in
          (create ~min ~max, nn)
      | Mul (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let min = CInt.mul al.min bl.min in
          let max = CInt.mul al.max bl.max in
          let nn = Expr.Mul (a, b) in
          (create ~min ~max, nn)
      | Div (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let min = CInt.div al.min (CInt.max CInt.one bl.max) in
          let max = CInt.div al.max (CInt.max CInt.one bl.min) in
          let nn = Expr.Div (a, b) in
          (create ~min ~max, nn)
      | Mod (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let min = CInt.zero in
          let max = CInt.min al.max bl.max in
          let nn = Expr.Mod (a, b) in
          (create ~min ~max, nn)
      | LShift (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let min = CInt.left_shift al.min ~amt:bl.min in
          let max = CInt.left_shift al.max ~amt:bl.max in
          let nn = Expr.LShift (a, b) in
          (create ~min ~max, nn)
      | RShift (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let min = CInt.right_shift al.min ~amt:bl.max in
          let max = CInt.right_shift al.max ~amt:bl.min in
          let nn = Expr.RShift (a, b) in
          (create ~min ~max, nn)
      | BitAnd (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let min = CInt.zero in
          let max = CInt.min al.max bl.max in
          let nn = Expr.BitAnd (a, b) in
          (create ~min ~max, nn)
      | BitOr (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let min = CInt.min al.min bl.min in
          let max =
            let bitwidth =
              Int.max (CInt.bitwidth al.max) (CInt.bitwidth bl.max)
            in
            CInt.min (CInt.add al.max bl.max) (max_cint_of_width bitwidth)
          in
          let nn = Expr.BitOr (a, b) in
          (create ~min ~max, nn)
          (* let (al, a), (bl, b) = (f a, f b) in
              let lat =
               if CInt.eq al.min al.max && CInt.eq bl.min bl.max then
                 {
                   min = CInt.bit_or al.min bl.min;
                   max = CInt.bit_or al.min bl.min;
                 }
             in
             (lat,  (a, b)) *)
      | BitXor (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let min = CInt.min al.min bl.min in
          let max =
            let bitwidth =
              Int.max (CInt.bitwidth al.max) (CInt.bitwidth bl.max)
            in
            CInt.min (CInt.add al.max bl.max) (max_cint_of_width bitwidth)
          in
          let nn = Expr.BitXor (a, b) in
          (create ~min ~max, nn)
          (*
             let (al, a), (bl, b) = (f a, f b) in
             let lat =
               {
                 min = CInt.zero;
                 max =
                   (let bitwidth =
                      Int.max (CInt.bitwidth al.max) (CInt.bitwidth bl.max)
                    in
                    CInt.min (CInt.add al.max bl.max) (max_cint_of_width bitwidth));
               }
             in
             (lat, BitXor (a, b)) *)
      | Clip (e, bits) ->
          let el, e = f e in
          let min = CInt.zero in
          let max = CInt.min el.max (max_cint_of_width bits) in
          let nn = Expr.Clip (e, bits) in
          (create ~min ~max, nn)
      | Eq (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let lat =
            if
              CInt.eq al.min al.max && CInt.eq bl.min bl.max
              && CInt.eq al.min bl.min
            then one
            else if CInt.lt al.max bl.min && CInt.gt al.min bl.max then zero
            else create_bitwidth 1
          in
          (lat, Eq (a, b))
      | Ne (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let lat =
            if
              CInt.eq al.min al.max && CInt.eq bl.min bl.max
              && CInt.eq al.min bl.min
            then zero
            else if CInt.lt al.max bl.min && CInt.gt al.min bl.max then one
            else create_bitwidth 1
          in
          (lat, Ne (a, b))
      | Lt (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let lat =
            if CInt.lt al.max bl.min then one
            else if CInt.ge al.min bl.max then zero
            else create_bitwidth 1
          in
          (lat, Lt (a, b))
      | Le (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let lat =
            if CInt.le al.max bl.min then one
            else if CInt.gt al.min bl.max then zero
            else create_bitwidth 1
          in
          (lat, Le (a, b))
      | Gt (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let lat =
            if CInt.gt al.min bl.max then one
            else if CInt.le al.max bl.min then zero
            else create_bitwidth 1
          in
          (lat, Gt (a, b))
      | Ge (a, b) ->
          let (al, a), (bl, b) = (f a, f b) in
          let lat =
            if CInt.ge al.min bl.max then one
            else if CInt.lt al.max bl.min then zero
            else create_bitwidth 1
          in
          (lat, Ge (a, b))
      | Eq0 a ->
          let al, a = f a in
          let lat =
            if CInt.le al.max CInt.zero then one
            else if CInt.gt al.min CInt.zero then zero
            else create_bitwidth 1
          in
          (lat, Eq0 a)
    in
    let e = if CInt.eq lat.min lat.max then Expr.Const lat.min else e in
    (lat, e)
  in
  f e

let eval_expr e ~of_var = eval_rewrite_expr e ~of_var |> fst
let rewrite_expr e ~of_var = eval_rewrite_expr e ~of_var |> snd
