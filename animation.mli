open Core_kernel
open Bonsai_web

module Circle : sig
  type t [@@deriving sexp, equal]

  val step : t -> [ `End | `Cont of t ]
  val create : centre:Coord.t -> t

  include Comparable.S with type t := t

  val rasterise_map : unit list Map.t Incr.t -> unit list Coord.Map.t Map.t Incr.t

  module PP : sig
    type t = int * Coord.t [@@inline] [@@deriving sexp, equal, compare]

    include Comparable.S with type t := t
  end

  val rasterise_preprocess : t -> PP.t
  val rasterise : PP.t -> unit list Coord.Map.t
end
