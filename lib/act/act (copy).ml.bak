open! Core

module CInt = struct
  type t = int [@@deriving sexp]

  let of_int i = i
  let l_of_l l = l
end

module CBool = struct
  type t = bool [@@deriving sexp]
end

module Bits = struct
  include Int

  let of_int t = t
end

module DType = struct
  type 'a t = { max_bits : Bits.t } [@@deriving sexp]

  let untype { max_bits } = { max_bits }
  let new_int ~max_bits = { max_bits }
  let new_bool () = { max_bits = Bits.of_int 1 }
end

let next_id_val = ref 1

module Id = struct
  include Int

  let new_ () =
    let v = !next_id_val in
    incr next_id_val;
    v
end

module Context = struct
  module Id : sig
    type t [@@deriving sexp]

    val new_ : unit -> t
  end =
    Id

  type t = { parent : Id.t option; id : Id.t } [@@deriving sexp]

  let create_child_context t = { parent = Some t.id; id = Id.new_ () }
  let create_root () = { parent = None; id = Id.new_ () }
end

module VC = struct
  type 'a t = { ctx : Context.t; id : Id.t; dtype : 'a DType.t }
  [@@deriving sexp, fields]

  let new_bool ctx = { ctx; id = Id.new_ (); dtype = DType.new_bool () }
  let new_of_dtype ctx dtype = { ctx; id = Id.new_ (); dtype }
  let dtype { ctx = _; id = _; dtype } = dtype
  let untype { ctx; id; dtype } = { ctx; id; dtype = DType.untype dtype }
end

module Var = struct
  include VC
end

module Chan = struct
  module R = struct
    include VC

    let get_writable_pair t = t
  end

  module W = struct
    include VC

    let _get_readable_pair t = t
  end
end

module Expr = struct
  type 'a t =
    | Const of 'a
    | Var of 'a Var.t
    | Map of Any.t t * (Any.t -> 'a)
    | Map2 of Any.t t * Any.t t * (Any.t -> Any.t -> 'a)
    | BoolNot of CBool.t t
  [@@deriving sexp_of]

  let untype t : Any.t t = Obj.magic t
  let of_var v = Var v
  let const c = Const c
  let map a ~f = Map (untype a, Obj.magic f)
  let map2 a b ~f = Map2 (untype a, untype b, Obj.magic f)
  let not b = BoolNot b

  let _all_vars e =
    let rec all_vars e =
      match e with
      | Const _ -> []
      | Var var -> [ Var.untype var ]
      | Map (a, _) -> all_vars (untype a)
      | Map2 (a, b, _) -> all_vars (untype a) @ all_vars (untype b)
      | BoolNot e -> all_vars (untype e)
    in
    all_vars (untype e)

  let _all_chans _ = []
end

module Chp = struct
  type t =
    | Loop of t list
    | Seq of t list
    | Read of (Any.t Chan.R.t * Any.t Var.t)
    | Send of (Any.t Chan.W.t * Any.t Expr.t)
    | BoolSelect of CBool.t Var.t * t list * t list
    | Assign of (Any.t Var.t * Any.t Expr.t)
    | Log of string Expr.t
  [@@deriving sexp_of]

  let loop stmts = Loop stmts
  let seq stmts = Seq stmts
  let read chan var = Read (Chan.R.untype chan, Var.untype var)
  let send chan expr = Send (Chan.W.untype chan, Expr.untype expr)
  let send_var chan var = send chan (Expr.of_var var)
  let select_bool var ~f = BoolSelect (var, f false, f true)
  let log expr = Log expr

  let flip var =
    Assign (Var.untype var, Expr.of_var var |> Expr.not |> Expr.untype)

  let hum_print_chp t = print_s [%sexp (t : t)]
end

module Node = struct
  module D = struct
    type t = {
      label : string;
      ctx : Context.t;
      in_ : Any.t Chan.R.t list;
      out : (Any.t Chan.W.t * Any.t Chan.R.t) list;
    }
    [@@deriving sexp_of]
  end

  type t =
    | Chp of D.t * Chp.t
    | Cluster of D.t * t list
    | NullInst of Context.t
    | Tie of (Context.t * Any.t Chan.R.t * Any.t Chan.W.t)
  [@@deriving sexp_of]

  module type Port_type = sig
    module W : sig
      type t
    end

    type t

    val to_list : t -> Any.t Chan.R.t list
    val to_zipped_list : t -> W.t -> (Any.t Chan.R.t * Any.t Chan.W.t) list
    val get_writable_pair : t -> W.t
  end

  let create_chp (type i o ow) (module I : Port_type with type t = i)
      (module O : Port_type with type t = o and type W.t = ow) ctx ~label ~in_
      ~out ~f =
    let out_w : ow = O.get_writable_pair out in
    let chp_ctx = Context.create_child_context ctx in
    let chp = f chp_ctx ~in_ ~out:out_w in
    (* TODO validate stuff *)
    let node =
      let d =
        {
          D.label;
          ctx = chp_ctx;
          in_ = I.to_list in_;
          out = O.to_zipped_list out out_w;
        }
      in
      Chp (d, chp)
    in
    (out, node)

  let create_cluster (type i o ow) (module I : Port_type with type t = i)
      (module O : Port_type with type t = o and type W.t = ow) ctx ~label ~in_
      ~out ~f =
    let out_w = O.get_writable_pair out in
    let cluster_ctx = Context.create_child_context ctx in
    let nodes = f cluster_ctx ~in_ ~out:out_w in
    (* TODO validate stuff *)
    let node =
      let d =
        {
          D.label;
          ctx = cluster_ctx;
          in_ = I.to_list in_;
          out = O.to_zipped_list out out_w;
        }
      in
      Cluster (d, nodes)
    in
    (out, node)

  let null_inst ctx = NullInst ctx
  let tie ctx r w = Tie (ctx, Chan.R.untype r, Chan.W.untype w)
end

module A0 = struct
  type t = Context.t

  let new_ ctx = ctx

  module W = struct
    type t = Context.t

    let new_ ctx = ctx
  end

  let get_writable_pair t = t
  let to_list _ = []
  let to_zipped_list _ _ = []
end

module A1 = struct
  module CInt = struct
    type t = CInt.t Chan.R.t

    let dtype t = Chan.R.dtype t
    let new_of_dtype ctx dtype = Chan.R.new_of_dtype ctx dtype

    module W = struct
      type t = CInt.t Chan.W.t

      let dtype t = Chan.W.dtype t
      let new_of_dtype ctx dtype = Chan.W.new_of_dtype ctx dtype
    end

    let get_writable_pair t = Chan.R.get_writable_pair t
    let to_list t = [ Chan.R.untype t ]
    let to_zipped_list t w = [ (Chan.R.untype t, Chan.W.untype w) ]
  end
end

module A2 = struct
  module CInt = struct
    type t = { a0 : CInt.t Chan.R.t; a1 : CInt.t Chan.R.t }

    let dtype t = Chan.R.dtype t.a0 (* TODO assert both the same *)

    let new_of_dtype ctx dtype =
      let a0 = Chan.R.new_of_dtype ctx dtype in
      let a1 = Chan.R.new_of_dtype ctx dtype in
      { a0; a1 }

    module W = struct
      type t = { a0 : CInt.t Chan.W.t; a1 : CInt.t Chan.W.t }

      let new_of_dtype ctx dtype =
        let a0 = Chan.W.new_of_dtype ctx dtype in
        let a1 = Chan.W.new_of_dtype ctx dtype in
        { a0; a1 }

      let dtype t = Chan.W.dtype t.a0 (* TODO assert both the same *)
    end

    let get_writable_pair t =
      let a0 = Chan.R.get_writable_pair t.a0 in
      let a1 = Chan.R.get_writable_pair t.a1 in
      { W.a0; a1 }

    let to_list t = [ Chan.R.untype t.a0; Chan.R.untype t.a1 ]

    let to_zipped_list t w =
      [
        (Chan.R.untype t.a0, Chan.W.untype w.W.a0);
        (Chan.R.untype t.a1, Chan.W.untype w.a1);
      ]
  end
end

module Sim = struct
  module D = struct
    type t

    let new_r _ _ = failwith "unimplemented"
    let new_w _ _ = failwith "unimplemented"
  end

  type t [@@deriving sexp_of]

  let sim _ = failwith "unimplemented"
  let send _ _ _ = failwith "unimplemented"
  let read _ _ _ = failwith "unimplemented"
  let send_and_wait _ _ _ = failwith "unimplemented"
  let read_and_wait _ _ _ = failwith "unimplemented"
  let wait _ = failwith "unimplemented"
  let d_test _ ~inputs:_ ~expected:_ = failwith "unimplemented"
end

(*
   module Sim = struct
     (*
        To run a simulation, first we quish the whole program down to a simplified flat
        piece of chp. We pull the information to help with logging out to the side to keep
        this small and efficient. It might be worth rewriting this in rust eventually. At
        the moment, the flattening is primarily to help with simplifying the simulation code
     *)

     module Flat_chp = struct
       module Idx = Int
       module Mut_idx = struct type t = { mutable v:  int }  end

   module Node = struct
       type t =
         | Read of (Any.t Chan.R.t * Any.t Var.t)  * (* next *) Mut_idx.t
         | Send of (Any.t Chan.W.t * Any.t Expr.t) * (* next *) Mut_idx.t
         | Assign of (Any.t Var.t * Any.t Expr.t)  * (* next *) Mut_idx.t
         | Log of string Expr.t                    * (* next *) Mut_idx.t
         | IfElse of CBool.t Expr.t                * (* true *) Mut_idx.t  * (* false *) Mut_idx.t
         | IfElseJoin of                           * (* next *) Mut_idx.t
         | Par of                                  * (* left *) Mut_idx.t  * (* right *) Mut_idx.t
         | ParJoin of int ref                      * (* next *) Mut_idx.t
         | Unreachable
         | End
       [@@deriving sexp]
   end

   module Node_arr = struct type t =

       (* TODO use a vec *)
       type t = { nodes: Node.t list; chp: Chp.t}
       [@@deriving sexp]

       (* returns start, after_ref *)
       let of_chp chp renames=
         let nodes = ref [ ref End ] in
         let new_node () = nodes := nodes @ [ref End]; List.last_exn nodes, List.length nodes  - 1 in
         let rec of_chp chp =
           let of_l l =
             let l = List.map l ~f:(fun stmt -> of_chp stmt) in
             let slid_zip l =
               if List.is_empty l then [] else (
               List.zip_exn
                 (List.take l (List.length l - 1) |> List.map ~f:snd)
                 (List.tl_exn l |> List.map ~f:fst)
             ) in
             slid_zip l |> List.iter ~f:(fun (e1, s2) -> (List.nth l e1) := n);
             (List.hd_exn l |> fst, List.last_exn l |> snd)
           in
           let rename_v var = var (* TODO *) in
           let rename_c chan = chan (* TODO *) in
           let rename_e expr = expr (* TODO *) in
           match chp with
           | Chp.Read (chan, var) ->
               let n, ni = new_node () in
               n := Read (rename_c chan, rename_v var, 0)
               (ni, ni)
           | Send (chan, expr) ->
               let n, ni = new_node () in
               n := Send (rename_c chan, expr, 0)
               (ni, ni)
           | Assign (var, expr) ->
               let n, ni = new_node () in
               n := Assign (rename_v var, rename_e expr, 0)
               (ni, ni)
           | Log expr ->
               let n, ni = new_node () in
               n := Log (rename_e expr, 0)
               (ni, ni)
           | Loop stmts -> let hd, after_ref = of_l stmts in after_ref := hd;
               hd, ref End
           | Seq stmts -> of_l stmts
           | BoolSelect (var, b1, b2) ->
               let b1_hd, b1_after_ref = of_l b1 in
               let b2_hd, b2_after_ref = of_l b2 in
               let guard = Expr.of_var (rename_v var) in
               let split = IfElse (guard, ref b1_hd, ref b2_hd) in
               let en = ref End in
               let join = IfElseJoin en in
               b1_after_ref := join;
               b2_after_ref := join;
               split, en
   in { n = of_chp chp; chp}
   (*
       let rec all_vars chp =
         let of_l l = Array.to_list l |> List.concat_map ~f:all_vars in
         match chp with
         | Chp.Loop stmt -> all_vars stmt
         | Seq stmts -> of_l stmts
         | Par stmts -> of_l stmts
         | Read (_, var) -> [ var ]
         | Send (_, expr) -> Expr.all_vars expr
         | SelectBool (expr, b1, b2) ->
             Expr.all_vars expr @ all_vars b1 @ all_vars b2
         | Assign (var, expr) -> var :: Expr.all_vars expr
         | Log expr -> Expr.all_vars expr

       let rec all_chans chp =
         let of_l l = Array.to_list l |> List.concat_map ~f:all_chans in
         match chp with
         | Loop stmt -> all_chans stmt
         | Seq stmts -> of_l stmts
         | Par stmts -> of_l stmts
         | Read (chan, _) -> [ chan ]
         | Send (chan, expr) -> chan :: Expr.all_chans expr
         | SelectBool (expr, b1, b2) ->
             Expr.all_chans expr @ all_chans b1 @ all_chans b2
         | Assign (_, expr) -> Expr.all_chans expr
         | Log expr -> Expr.all_chans expr *)
     end

     module Index_tree = struct
       module State = struct
         type t = Done | Index of int [@@deriving sexp]
       end

       type t = {
         mutable parent : t option; [@sexp.opaque]
         mutable state : State.t;
         mutable children : t array;
       }
       [@@deriving sexp]
     end

     module Var_table : sig
       type t [@@deriving sexp]

       val new_ : Id.Set.t -> t
       val _find_exn : t -> 'a Var.t -> 'a option
       val _set_exn : t -> 'a Var.t -> 'a -> unit
     end = struct
       type t = { ids : Id.Set.t; tbl : Any.t option Id.Table.t [@sexp.opaque] }
       [@@deriving sexp]

       let new_ ids =
         {
           ids;
           tbl =
             Set.to_list ids
             |> List.map ~f:(fun id -> (id, None))
             |> Id.Table.of_alist_exn;
         }

       let _find_exn (type a) (t : t) (var : a Var.t) =
         let value : Any.t option = Hashtbl.find_exn t.tbl (Var.id var) in
         match value with
         | None -> None
         | Some value ->
             let value : a = Obj.magic value in
             Some value

       let _set_exn (type a) (t : t) (var : a Var.t) (value : a) =
         let value : Any.t = Obj.magic value in
         Hashtbl.set t.tbl ~key:(Var.id var) ~data:(Some value)
     end

     module Chan_table : sig
       type t [@@deriving sexp]

       val new_ : Id.Set.t -> t
     end = struct
       type t = { ids : Id.Set.t; tbl : Any.t option Id.Table.t [@sexp.opaque] }
       [@@deriving sexp]

       let new_ ids =
         {
           ids;
           tbl =
             Set.to_list ids
             |> List.map ~f:(fun id -> (id, None))
             |> Id.Table.of_alist_exn;
         }
     end

     module D = struct
       type t [@@deriving sexp]

       let new_r _ _ = failwith "unimplemented"
       let new_w _ _ = failwith "unimplemented"
     end

     type t = {
       program : Flat_chp.t;
       mutable pc : Index_tree.t;
       mutable vars : Var_table.t;
       mutable chans : Chan_table.t;
     }
     [@@deriving sexp]

     (* This all uses the fact the read an write end of a channel us the same id at the moment *)
     let sim node =
       let ios =
         match node with
         | Node.Chp (d, _) | Cluster (d, _) -> d.in_ @ List.map d.out ~f:fst
         | NullInst _ -> []
         | Tie (_, i, o) -> [ i; Chan.W.get_readable_pair o ]
       in
       let io_ids = List.map ios ~f:Chan.R.id |> Id.Set.of_list in
       let flat_nodes =
         let rec f node =
           match node with
           | Node.Cluster (_, nodes) -> List.concat_map nodes ~f
           | _ -> [ node ]
         in
         f node
       in
       let id_to_raw_cluster_id =
         let ties =
           List.filter_map flat_nodes ~f:(function
             | Node.Tie (_, i, o) -> Some (i, o)
             | _ -> None)
         in
         let ids =
           List.concat_map ties ~f:(fun (a, b) -> [ a; b ])
           |> List.map ~f:Chan.R.id |> Id.Set.of_list |> Set.to_list
         in
         let tbl = List.map ids ~f:(fun a -> (a, a)) |> Id.Table.of_alist_exn in
         (* TODO error if multiple things tied to same side of same thing. *)
         List.iter ties ~f:(fun (i, o) ->
             let iv = Hashtbl.find_exn tbl (Chan.R.id i) in
             let ov = Hashtbl.find_exn tbl (Chan.R.id o) in
             match Id.equal iv ov with
             | true -> ()
             | false ->
                 List.iter ids ~f:(fun id -> Hashtbl.set tbl ~key:id ~data:iv));
         tbl
       in
       let cluster_list =
         Hashtbl.to_alist id_to_raw_cluster_id
         |> List.map ~f:(fun (a, b) -> (b, a))
         |> Id.Map.of_alist_multi |> Map.data
         |> List.filter ~f:(fun l -> List.length l >= 1)
         |> List.map ~f:Id.Set.of_list
       in
       let renames =
         List.concat_map cluster_list ~f:(fun cluster ->
             let id =
               match Set.inter cluster io_ids |> Set.to_list with
               | [] -> Set.to_list cluster |> List.hd_exn
               | [ id ] -> id
               | _ ->
                   failwith "the same cluster contains multiple ids in the io list"
             in
             Set.to_list cluster |> List.map ~f:(fun cid -> (cid, id)))
         |> Id.Map.of_alist_exn
       in
       (* check we dont rename any top level io ports *)
       Set.iter io_ids ~f:(fun id ->
           match Map.find renames id with
           | None -> ()
           | Some nid -> assert (Id.equal id nid));
       (* TODO contexts are currently wrong, and fixing them will probably involve makeing read and
          write have different ids, making this code not quite right *)
       let flat_chp =
         let flat_chps =
           List.filter_map flat_nodes ~f:(function
             | Node.Cluster _ -> (* TODO handel channels across boundary *) None
             | Node.Chp (_, chp) ->
                 (* TODO handel channels across boundary *)
                 Some (Flat_chp.of_chp chp renames)
             | _ -> None)
           |> Array.of_list
         in
         Flat_chp.Par flat_chps
       in
       let pc = { parent = None; Index_tree.state = Index 0; children = [||] } in
       let all_vars =
         Flat_chp.all_vars flat_chp |> List.map ~f:Var.id |> Id.Set.of_list
       in
       let vars = Var_table.new_ all_vars in
       let all_chans =
         Flat_chp.all_chans flat_chp |> List.map ~f:Var.id |> Id.Set.of_list
       in
       let chans = Chan_table.new_ all_chans in
       { program = flat_chp; pc; vars; chans }

     (* returns wether any progress was made *)
     let step t =
       let all_nodes =
         let rec f (n : Index_tree.t) =
           n :: (Array.to_list n.children |> List.concat_map ~f)
         in
         f t.pc
       in
       let stepable_nodes =
         List.filter all_nodes ~f:(fun n -> Array.length n.children |> Int.equal 0)
       in
       (* TODO select a random stepable node *)
       let idx = List.hd stepable_nodes in
       (* TODO remove gaurds on variables *)
       (* match node with
          | Flat_chp.Loop stmt -> all_vars stmt
          | Seq stmts -> of_l stmts
          | Par stmts -> of_l stmts
          | Read (_, var) -> [ var ]
          | Send (_, expr) -> Expr.all_vars expr
          | SelectBool (expr, b1, b2) ->
                Expr.all_vars expr @ all_vars b1 @ all_vars b2
          | Assign (var, expr) -> var :: Expr.all_vars expr
          | Log expr -> Expr.all_vars expr

          let ct = List.length all_nodes in *)
       print_s [%sexp (t.pc : Index_tree.t)];
       print_s [%sexp (ct : int)];
       print_s [%sexp (stepable_nodes : Index_tree.t list)]

     let wait t = step t
     let send _ _ _ = failwith "unimplemented"
     let read _ _ _ = failwith "unimplemented"
     let send_and_wait _ _ _ = failwith "unimplemented"
     let read_and_wait _ _ _ = failwith "unimplemented"
     let d_test _ ~inputs:_ ~expected:_ = failwith "unimplemented"
   end
*)
(* open! Core

   module Var = struct
     type 'dk t
   end

   module Chan = struct
     module R = struct
       type 'dk t [@@deriving sexp]
     end

     module W = struct
       type 'dk t [@@deriving sexp]
     end

     module P = struct
       type 'dk t = { r : 'dk R.t; w : 'dk W.t } [@@deriving sexp]
     end
   end

   module Expr = struct
     type 'o t

     let of_var = failwith "unimplemented"
     let of_cint = failwith "unimplemented"
     let of_cbool = failwith "unimplemented"
     let map = failwith "unimplemented"
     let mapv = failwith "unimplemented"
     let map2 = failwith "unimplemented"
   end

   module Chp_stmt = struct
     type t

     module GuardedBranch = struct
       type t
     end

     let seq = failwith "unimplemented"
     let par = failwith "unimplemented"
     let assign = failwith "unimplemented"
     let read = failwith "unimplemented"
     let send = failwith "unimplemented"
     let loop = failwith "unimplemented"
     let do_while = failwith "unimplemented"
     let select = failwith "unimplemented"
     let assert_ = failwith "unimplemented"
     let log = failwith "unimplemented"
     let branch = failwith "unimplemented"
   end

   module Bits = struct
     type t

     let of_int = failwith "unimplemented"
   end

   module CBool = struct
     type t

     module Var = struct
       type nonrec t = t Var.t

       let new_ = failwith "unimplemented"
     end

     module Chan = struct
       module R = struct
         type nonrec t = t Chan.R.t

         let new_ = failwith "unimplemented"
       end

       module W = struct
         type nonrec t = t Chan.W.t

         let new_ = failwith "unimplemented"
       end
     end
   end

   module CInt = struct
     type t

     module Var = struct
       type nonrec t = t Var.t

       let new_ = failwith "unimplemented"
     end

     module Chan = struct
       module R = struct
         type nonrec t = t Chan.R.t

         let new_ = failwith "unimplemented"
       end

       module W = struct
         type nonrec t = t Chan.W.t

         let new_ = failwith "unimplemented"
       end
     end
   end
*)
