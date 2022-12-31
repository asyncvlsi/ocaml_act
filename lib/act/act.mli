open! Core

module CInt : sig
  type t

  val of_int : int -> t
  val l_of_l : int list -> t list
end

module CBool : sig
  type t
end

module Bits : sig
  type t

  val of_int : int -> t
end

module DType : sig
  type 'a t

  val new_int : max_bits:Bits.t -> CInt.t t
  val new_bool : unit -> bool t
end

module Context : sig
  type t

  val create_root : unit -> t
end

module Var : sig
  type 'a t

  val new_bool : Context.t -> CBool.t t
  val new_of_dtype : Context.t -> 'a DType.t -> 'a t
  val dtype : 'a t -> 'a DType.t
end

module Chan : sig
  module R : sig
    type 'a t

    val new_bool : Context.t -> CBool.t t
    val new_of_dtype : Context.t -> 'a DType.t -> 'a t
    val dtype : 'a t -> 'a DType.t
  end

  module W : sig
    type 'a t

    val new_bool : Context.t -> CBool.t t
    val new_of_dtype : Context.t -> 'a DType.t -> 'a t
    val dtype : 'a t -> 'a DType.t
  end
end

module Expr : sig
  type 'a t

  val of_var : 'a Var.t -> 'a t
  val const : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
end

module Chp : sig
  type t

  val loop : t list -> t
  val read : 'a Chan.R.t -> 'a Var.t -> t
  val send : 'a Chan.W.t -> 'a Expr.t -> t
  val send_var : 'a Chan.W.t -> 'a Var.t -> t
  val select_bool : CBool.t Var.t -> f:(bool -> t list) -> t
  val flip : CBool.t Var.t -> t
  val log : string Expr.t -> t
  val hum_print_chp : t -> unit
end

module Node : sig
  type t

  module type Port_type = sig
    module W : sig
      type t
    end

    type t

    val to_list : t -> Any.t Chan.R.t list
    val to_zipped_list : t -> W.t -> (Any.t Chan.R.t * Any.t Chan.W.t) list
    val get_writable_pair : t -> W.t
  end

  val create_chp :
    (module Port_type with type t = 'i) ->
    (module Port_type with type t = 'o and type W.t = 'ow) ->
    Context.t ->
    label:string ->
    in_:'i ->
    out:'o ->
    f:(Context.t -> in_:'i -> out:'ow -> Chp.t) ->
    'o * t

  val create_cluster :
    (module Port_type with type t = 'i) ->
    (module Port_type with type t = 'o and type W.t = 'ow) ->
    Context.t ->
    label:string ->
    in_:'i ->
    out:'o ->
    f:(Context.t -> in_:'i -> out:'ow -> t list) ->
    'o * t

  val null_inst : Context.t -> t
  val tie : Context.t -> 'a Chan.R.t -> 'a Chan.W.t -> t
end

module A0 : sig
  type t

  val new_ : Context.t -> t

  module W : sig
    type t

    val new_ : Context.t -> t
  end

  val to_list : t -> Any.t Chan.R.t list
  val to_zipped_list : t -> W.t -> (Any.t Chan.R.t * Any.t Chan.W.t) list
  val get_writable_pair : t -> W.t
end

module A1 : sig
  module CInt : sig
    type t = CInt.t Chan.R.t

    val dtype : t -> CInt.t DType.t
    val new_of_dtype : Context.t -> CInt.t DType.t -> t

    module W : sig
      type t = CInt.t Chan.W.t

      val new_of_dtype : Context.t -> CInt.t DType.t -> t
      val dtype : t -> CInt.t DType.t
    end

    val to_list : t -> Any.t Chan.R.t list
    val to_zipped_list : t -> W.t -> (Any.t Chan.R.t * Any.t Chan.W.t) list
    val get_writable_pair : t -> W.t
  end
end

module A2 : sig
  module CInt : sig
    type t = { a0 : CInt.t Chan.R.t; a1 : CInt.t Chan.R.t }

    val dtype : t -> CInt.t DType.t
    val new_of_dtype : Context.t -> CInt.t DType.t -> t

    module W : sig
      type t = { a0 : CInt.t Chan.W.t; a1 : CInt.t Chan.W.t }

      val new_of_dtype : Context.t -> CInt.t DType.t -> t
      val dtype : t -> CInt.t DType.t
    end

    val to_list : t -> Any.t Chan.R.t list
    val to_zipped_list : t -> W.t -> (Any.t Chan.R.t * Any.t Chan.W.t) list
    val get_writable_pair : t -> W.t
  end
end

module Sim : sig
  module D : sig
    type t

    val new_r : 'a Chan.R.t -> 'a list -> t
    val new_w : 'a Chan.W.t -> 'a list -> t
  end

  type t [@@deriving sexp]

  val sim : Node.t -> t
  val send : t -> 'a Chan.W.t -> 'a -> unit
  val read : t -> 'a Chan.R.t -> 'a -> unit
  val send_and_wait : t -> 'a Chan.W.t -> 'a -> unit
  val read_and_wait : t -> 'a Chan.R.t -> 'a -> unit
  val wait : t -> unit
  val d_test : t -> inputs:D.t list -> expected:D.t list -> unit
end
(*
   module CBool0 : sig
     type t

     val not : t -> t
   end

   module CInt0 : sig
     type t
   end

   module Chan : sig
     type t

     module R : sig
       type nonrec 'dk t = { ut : t } [@@deriving sexp]
     end

     module W : sig
       type nonrec 'dk t = { ut : t } [@@deriving sexp]
     end

     module P : sig
       type 'dk t = { r : 'dk R.t; w : 'dk W.t } [@@deriving sexp]
     end
   end

   module Node_inst : sig
     type t

     val null_inst : t
     val of_subnodes : t list -> t
     val of_chp : 'a -> t
   end

   module Prs : sig
     module Block : sig
       type t

       val print : t -> unit
     end
   end

   module Chp : sig
     module Var : sig
       type 'dk t
     end

     module Expr : sig
       type 'o t

       val of_var : 'dk Var.t -> 'dk t
       val of_cint : CInt0.t -> CInt0.t t
       val of_cbool : CBool0.t -> CBool0.t t
       val map : 'i t -> f:('i -> 'o) -> 'o t
       val mapv : 'i Var.t -> f:('i -> 'o) -> 'o t
       val map2 : 'i1 t -> 'i2 t -> f:('i1 -> 'i2 -> 'o) -> 'o t
     end

     module Stmt : sig
       type t

       module GuardedBranch : sig
         type t
       end

       val seq : t list -> t
       val par : t list -> t
       val assign : 'data Var.t -> 'data Expr.t -> t
       val flip : CBool0.t Var.t -> t
       val read : 'data Chan.R.t -> 'data Var.t -> t
       val send : 'data Chan.W.t -> 'data Expr.t -> t
       val loop : t list -> t
       val do_while : t list -> guard:CBool0.t Expr.t -> t
       val select : GuardedBranch.t list -> t
       val select_bool : CBool0.t Var.t -> f:(bool -> t list) -> t
       val assert_ : CBool0.t Expr.t -> t
       val log : string Expr.t -> t
       val branch : guard:CBool0.t Expr.t -> t list -> GuardedBranch.t
     end

     module Block : sig
       type t

       val of_chp : Stmt.t -> ports:Chan.t list -> t Or_error.t
       val of_chp_exn : Stmt.t -> ports:Chan.t list -> t
       val print : t -> unit
       val to_production_rules : t -> optimize_hints:[ `None ] -> Prs.Block.t
     end
   end

   module Bits : sig
     type t [@@deriving equal, compare]

     val of_int : int -> t
     val max : t -> t -> t
   end

   module CBool : sig
     type t = CBool0.t

     val not : t -> t

     module Var : sig
       type nonrec t = t Chp.Var.t

       val new_ : unit -> t
     end

     module Chan : sig
       module R : sig
         type nonrec t = t Chan.R.t

         val new_ : unit -> t
       end

       module W : sig
         type nonrec t = t Chan.W.t

         val new_ : unit -> t
       end
     end
   end

   module CInt : sig
     type t = CInt0.t

     val of_int : int -> t
     val l_of_l : int list -> t list

     module Var : sig
       type nonrec t = t Chp.Var.t

       val new_ : mb:Bits.t -> unit -> t
     end

     module Chan : sig
       module R : sig
         type nonrec t = t Chan.R.t

         val new_ : mb:Bits.t -> unit -> t
         val mb : t -> Bits.t
       end

       module W : sig
         type nonrec t = t Chan.W.t

         val new_ : mb:Bits.t -> unit -> t
       end

       module P : sig
         type nonrec t = t Chan.P.t

         val new_ : mb:Bits.t -> unit -> t
       end
     end
   end

   module Sim : sig
     module D : sig
       type t

       val new_r : 'a Chan.R.t -> 'a list -> t
       val new_w : 'a Chan.W.t -> 'a list -> t
     end

     type t

     val sim : Node_inst.t -> t
     val send : t -> 'a Chan.W.t -> 'a -> unit
     val read : t -> 'a Chan.R.t -> 'a -> unit
     val send_and_wait : t -> 'a Chan.W.t -> 'a -> unit
     val read_and_wait : t -> 'a Chan.R.t -> 'a -> unit
     val wait : t -> unit
     val d_test : t -> inputs:D.t list -> expected:D.t list -> unit
   end
*)
