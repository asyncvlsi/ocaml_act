open! Core
open! Act

(* $MDX part-begin=recursive_buffer_example *)
let split ~dtype i1 o1 o2 =
  let var1 = Var.create dtype in
  let b1 = Var.create CBool.dtype ~init:false in
  Chp.loop
    [
      Chp.read i1 var1;
      Chp.if_else
        Expr.(var b1)
        [ Chp.send_var o1 var1 ]
        [ Chp.send_var o2 var1 ];
      CBool.Chp.toggle b1;
    ]
  |> Process.of_chp ~iports:[ i1.u ] ~oports:[ o1.u; o2.u ]

let merge ~dtype i1 i2 o1 =
  let var1 = Var.create dtype in
  let b1 = Var.create CBool.dtype ~init:false in
  Chp.loop
    [
      Chp.if_else Expr.(var b1) [ Chp.read i1 var1 ] [ Chp.read i2 var1 ];
      Chp.send o1 Expr.(var var1);
      CBool.Chp.toggle b1;
    ]
  |> Process.of_chp ~iports:[ i1.u; i2.u ] ~oports:[ o1.u ]

let rec buff ~depth ~dtype i1 o1 =
  (if depth <= 0 then failwith "depth too low"
   else if Int.equal depth 1 then
     let chan1 = Chan.create dtype in
     let chan2 = Chan.create dtype in
     [ split ~dtype i1 chan1.w chan2.w; merge ~dtype chan1.r chan2.r o1 ]
   else
     let chan1a = Chan.create dtype in
     let chan1b = Chan.create dtype in
     let chan2a = Chan.create dtype in
     let chan2b = Chan.create dtype in

     [
       split ~dtype i1 chan1a.w chan2a.w;
       buff ~dtype ~depth:(depth - 1) chan1a.r chan1b.w;
       buff ~dtype ~depth:(depth - 1) chan2a.r chan2b.w;
       merge ~dtype chan1b.r chan2b.r o1;
     ])
  |> Process.of_procs ~iports:[ i1.u ] ~oports:[ o1.u ]

let buff ~depth ~dtype =
  let i = Chan.create dtype in
  let o = Chan.create dtype in
  let top_process = buff ~depth ~dtype i.r o.w in
  (top_process, i.w, o.r)

let%expect_test "test" =
  let process, i, o = buff ~depth:3 ~dtype:(CInt.dtype ~bits:9) in
  let sim = Sim.simulate process in

  let l =
    [ 1; 5; 9; 33; 123; 258; 500; 7; 9; 4; 5 ] |> List.map ~f:CInt.of_int
  in
  List.iter l ~f:(fun v -> Sim.send sim i v);
  Sim.wait' sim ();
  List.iter l ~f:(fun v -> Sim.read sim o v);
  Sim.wait' sim ();
  [%expect{|
    (Ok ())
    (Ok ()) |}]
(* $MDX part-end *)
