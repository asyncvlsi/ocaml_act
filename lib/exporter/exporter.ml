open! Core
open! Act

type t = Chp_exporter.t
type dflow = Dflow_exporter.t

let create = Chp_exporter.create
let create_dflow = Dflow_exporter.create
let simple_ir_sim = Dflow_exporter.simple_ir_sim
let stf_sim = Dflow_exporter.stf_sim
