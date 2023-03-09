open! Core
open! Act

type t = A | B | C

include CEnum.S with type t := t
