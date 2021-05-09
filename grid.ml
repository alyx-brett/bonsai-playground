open Core_kernel
open! Import

type 'a t =
  { size : int
  ; data : 'a Coord.Map.t
  }

let initial_render_state n ~f =
  List.init n ~f:Fn.id
  |> twice List.cartesian_product
  |> List.map ~f:(fun k -> k, f k)
  |> Coord.Map.of_alist_exn
;;

let create n ~f = { size = n; data = initial_render_state n ~f }

let set t ~key ~data =
  let test = Int.between ~low:0 ~high:(t.size + 1) in
  { t with
    data =
      (match T2.(map ~f:test key |> both_bool) with
      | true -> Coord.Map.set t.data ~key ~data
      | false -> t.data)
  }
;;

let data t = t.data
