open! Core
open! Ochp
open! Ochp.Act

type t = A | B | C

include CEnum.S with type t := t
