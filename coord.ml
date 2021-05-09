open Core_kernel
open! Import

module T = struct
  type t = Int.t * Int.t [@@deriving compare, sexp, equal]
end

include T
include Comparable.Make (T)

let to_string t = Sexp.to_string @@ [%sexp_of: t] t
let of_string_exn str = Sexp.of_string_conv_exn str [%of_sexp: t]
