module Circle : sig
  type t [@@deriving sexp, equal]

  val step : t -> [ `End | `Cont of t ]
  val create : centre:Coord.t -> t
  val rasterise : t -> Coord.Set.t
end
