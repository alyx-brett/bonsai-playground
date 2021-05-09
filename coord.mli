open Core_kernel

type t = Int.t * Int.t [@@deriving compare, sexp, equal]

include Comparable.S with type t := t

val of_string_exn : string -> t
val to_string : t -> string
