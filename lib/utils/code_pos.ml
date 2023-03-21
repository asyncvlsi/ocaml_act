open! Core

type t = Caml.Printexc.location = {
  filename : string;
  line_number : int;
  start_char : int;
  end_char : int;
}
[@@deriving sexp, hash, equal, compare]

let dummy_loc =
  { filename = ""; line_number = -1; start_char = -1; end_char = -1 }

let get_call_site_at_idx ~idx =
  Caml.Printexc.get_callstack (idx + 1)
  |> Caml.Printexc.raw_backtrace_entries
  |> Array.concat_map ~f:(fun e ->
         Caml.Printexc.backtrace_slots_of_raw_entry e |> Option.value_exn)
  |> (fun arr -> arr.(idx))
  |> Caml.Printexc.Slot.location |> Option.value_exn

let library_filenames =
  String.Set.of_list
    [
      "lib/act/code_pos.ml";
      "lib/utils/code_pos.ml";
      "lib/act/act.ml";
      "lib/act/ir.ml";
      "lib/act/cint.ml";
      "lib/act/cbool.ml";
      "lib/act/chan.ml";
      "lib/act/var.ml";
      "lib/act/chp.ml";
      "lib/sim/sim.ml";
      "list.ml";
      "src/list0.ml";
      "collector/expect_test_collector.ml";
      "runtime-lib/runtime.ml";
    ]

let is_library filename = Set.mem library_filenames filename

let psite ?(depth = 0) () =
  let res = ref None in
  let idx = ref 0 in
  let depth = ref depth in
  while Option.is_none !res do
    let cp = get_call_site_at_idx ~idx:!idx in
    if not (is_library cp.filename) then
      if Int.equal !depth 0 then res := Some cp else decr depth;
    incr idx
  done;
  Option.value_exn !res

let value_or_psite ?(depth = 0) o = Option.value o ~default:(psite ~depth ())
