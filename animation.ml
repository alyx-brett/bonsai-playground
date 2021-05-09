open Core_kernel
open! Import

module Circle = struct
  type t =
    { centre : Coord.t
    ; radius : float
    ; duration : int
    ; elapsed : int
    }
  [@@deriving sexp, equal]

  let create ~centre =
    { centre; radius = Random.float_range 5. 10.; duration = 60; elapsed = 0 }
  ;;

  let scaled_rad t = Float.iround_down_exn (t.radius *. (t.elapsed // t.duration))

  let step t =
    match t.duration - t.elapsed with
    | 0 -> `End
    | _ -> `Cont { t with elapsed = t.elapsed + 1 }
  ;;

  (* https://rosettacode.org/wiki/Bitmap/Midpoint_circle_algorithm#OCaml *)
  let raster_circle ~c:(x0, y0) ~r =
    let rec loop x y m accum =
      let open List in
      let accum =
        cartesian_product [ 1; -1 ] [ 1; -1 ]
        |> cartesian_product [ x, y; y, x ]
        |> map ~f:T2.((*lol*) uncurry (map2 ~f:( * ) >+> map2 (x0, y0) ~f:( + )))
        |> ( @ ) accum
      in
      let y, m = if m > 0 then y - 1, m - (8 * y) else y, m in
      if x <= y
      then (
        let x = x + 1 in
        let m = m + (8 * x) + 4 in
        loop x y m accum)
      else Coord.Set.of_list accum
    in
    loop 0 r (5 - (4 * r)) []
  ;;

  let rasterise t = raster_circle ~c:t.centre ~r:(scaled_rad t)
end
