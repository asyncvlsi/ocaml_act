open! Core

module Id : sig
  include Identifiable

  val create : unit -> t
end = struct
  include Int

  let next_int = ref 0

  let create () =
    let id = !next_int in
    incr next_int;
    id
end

module T = struct
  module U = struct
    module Id = Id

    module D = struct
      type t = {
        dtype : Any.t Dtype.Ir.t;
        creation_code_pos : Code_pos.t;
        init : Any.t option;
      }
    end

    module T = struct
      type t = {
        id : Id.t;
        d : (D.t[@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
      }
      [@@deriving hash, compare, equal, sexp]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)

    let create dtype init creation_code_pos =
      let id = Id.create () in
      let dtype = Dtype.Ir.untype dtype in
      let init = Any.Option.of_magic init in
      { id; d = { dtype; creation_code_pos; init } }
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  let create ?loc ?init (dtype : 'a Dtype.Wrap.t) : 'a t =
    let dtype = Dtype.Ir.unwrap dtype in
    (match init with
    | None -> ()
    | Some init -> (
        let init_layout = Dtype.Ir.max_layout_of dtype init in
        match Dtype.Ir.fits_into_dtype init_layout ~into:dtype with
        | true -> ()
        | false ->
            failwith
              [%string
                "Trying to initialize a variable of dtype %{Layout.sexp_of_t \
                 (Dtype.Ir.layout dtype)#Sexp} with a value of max_layout \
                 %{Layout.sexp_of_t init_layout#Sexp}."]));

    { u = U.create dtype init (Code_pos.value_or_psite loc) }
end

module Wrap = struct
  include T
end

module Ir = struct
  include T

  let unwrap (t : 'a t) = t
  let untype (t : 'a t) : U.t = t.u
  let untype' = untype
end
