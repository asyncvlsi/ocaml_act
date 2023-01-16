open! Core
open! Act

type t = A | B | C

include Enum.S with type t := t
