open Core_kernel

type t = int * int [@@deriving compare, sexp, equal] [@@inline]

include Comparable.S with type t := t

val of_string_exn : string -> t
val to_string : t -> string
