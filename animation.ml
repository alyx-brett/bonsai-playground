open Core_kernel
open! Import

(* https://rosettacode.org/wiki/Bitmap/Midpoint_circle_algorithm#OCaml *)
let raster_circle ~c:(x0, y0) ~r =
  let new_tiles x y =
    let open List in
    cartesian_product [ 1; -1 ] [ 1; -1 ]
    |> cartesian_product [ x, y; y, x ]
    |> map ~f:T2.((*lol*) uncurry (map2 ~f:( * ) >+> map2 (x0, y0) ~f:( + )))
  in
  let rec loop x y m accum =
    let y, m = if m > 0 then y - 1, m - (8 * y) else y, m in
    let accum = new_tiles x y @ accum in
    if x <= y
    then (
      let x = x + 1 in
      let m = m + (8 * x) + 4 in
      loop x y m accum)
    else accum
  in
  loop 0 r (5 - (4 * r)) []
;;

module Circle = struct
  module T = struct
    type t =
      { centre : Coord.t
      ; radius : int
      ; duration : int
      ; elapsed : int
      }
    [@@deriving sexp, equal, compare, fields]
  end

  include T
  include Comparable.Make (T)

  let create ~centre =
    { centre; radius = Random.int_incl 5 15; duration = 60; elapsed = 0 }
  ;;

  let step t =
    match t.duration - t.elapsed with
    | 0 -> `End
    | _ -> `Cont { t with elapsed = t.elapsed + 1 }
  ;;

  module PP = struct
    module T = struct
      type t = int * Coord.t [@@inline] [@@deriving sexp, equal, compare]
    end

    include T
    include Comparable.Make (T)
  end

  let scaled_rad t = t.radius * t.elapsed / t.duration
  let rasterise_preprocess t = scaled_rad t, t.centre

  let rasterise (r, c) =
    raster_circle ~c ~r |> List.map ~f:(fun a -> a, ()) |> Coord.Map.of_alist_multi
  ;;

  let rasterise_map map_of_t =
    let open Incr in
    Map.mapi map_of_t ~f:(fun ~key ~data:_ -> rasterise_preprocess key)
    |> Map.map ~f:rasterise
  ;;
end
