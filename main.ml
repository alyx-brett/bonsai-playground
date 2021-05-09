[@@@warning "-33"]

open Import
open Core_kernel
open Js_of_ocaml
open Bonsai_web
open Bonsai.Let_syntax

let char_size = 15
let grid_size = 40
let compose2 f g a b = f (g a b)
let ( >+> ) g f = compose2 f g
let twice f a = f a a

module Pixel = struct
  module Coordinate = struct
    type t = int * int [@@deriving sexp, equal]
  end
end

module Unicode_character = struct
  type t = string [@@inline]
end

module Coord = struct
  module T = struct
    type t = Int.t * Int.t [@@deriving compare, sexp, equal]
  end

  include T
  include Comparable.Make (T)

  let to_string t = Sexp.to_string @@ [%sexp_of: t] t
  let of_string_exn str = Sexp.of_string_conv_exn str [%of_sexp: t]
end

type char_render_model = { char : Unicode_character.t }

module Animation = struct
  module type S = sig
    type t

    val increment : t -> [ `End | `Cont of t ]
    val draw : t -> Vdom.Node.t
  end

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

    let increment t =
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
end

module Grid = struct
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

  let map2d t ~per_row ~per_value =
    let indices = List.init t.size ~f:Fn.id in
    List.map indices ~f:(fun rown ->
        List.map indices ~f:(fun columnn ->
            let coord = rown, columnn in
            Coord.Map.find_exn t.data coord |> per_value coord)
        |> per_row)
  ;;
end

module State = struct
  type t = { animations : Animation.Circle.t list } [@@deriving sexp, equal]

  let on_frame t =
    { t with
      animations =
        List.filter_map t.animations ~f:(fun animation ->
            match Animation.Circle.increment animation with
            | `End -> None
            | `Cont a -> Some a)
    }
  ;;

  let on_click t ~coord =
    { t with animations = Animation.Circle.create ~centre:coord :: t.animations }
  ;;

  let initial = { animations = [] }

  let draw t =
    List.fold
      ~init:(Grid.create grid_size ~f:(Fn.const { char = "&nbsp;" }))
      t.animations
      ~f:(fun accum ->
        Animation.Circle.rasterise
        >> Coord.Set.fold ~init:accum ~f:(fun accum key ->
               Grid.set accum ~key ~data:{ char = "." }))
  ;;
end
[@@ocaml.warning "-23"]

module Action = struct
  type t =
    [ `Increment_frame
    | `Click of Pixel.Coordinate.t
    ]
  [@@deriving sexp_of, variants]

  let to_handler = function
    | `Increment_frame -> State.on_frame
    | `Click coord -> State.on_click ~coord
  ;;
end

module Event_handlers = struct
  let on_frame broadcast =
    Bonsai.Edge.after_display (broadcast >>| ( |> ) `Increment_frame)
  ;;

  let on_click ~broadcast =
    Vdom.Attr.on_mousedown (fun e ->
        Firebug.console##log e;
        match
          let%bind.Option tgt = e##.target |> Js.Opt.to_option in
          tgt##getAttribute (Js.string "data-coord") |> Js.Opt.to_option
        with
        | None -> Ui_event.Ignore
        | Some coord_str ->
          coord_str |> Js.to_string |> Coord.of_string_exn |> Action.click |> broadcast)
  ;;

  let tile_handlers coord = Vdom.Attr.[ create "data-coord" (Coord.to_string coord) ]
end

let render_char coord char_model =
  Vdom.Node.(
    inner_html
      ~tag:"span"
      (Event_handlers.tile_handlers coord
      @ [ Vdom.Attr.style
            Css_gen.(
              display `Inline_block @> width (`Px char_size) @> height (`Px char_size))
        ])
      ~this_html_is_sanitized_and_is_totally_safe_trust_me:char_model.char)
;;

let render_everything render_input =
  Bonsai.assoc (module Coord) render_input ~f:(Bonsai.Value.map2 ~f:render_char >+> return)
;;

let app_component =
  let%sub state_machine =
    Bonsai.state_machine0
      [%here]
      (module State)
      (module Action)
      ~default_model:State.initial
      ~apply_action:(fun ~inject:_ ~schedule_event:_ -> Fn.flip Action.to_handler)
  in
  let broadcast = state_machine >>| snd in
  let%sub _ = Event_handlers.on_frame broadcast in
  let%sub rendered_nodes =
    return (state_machine >>| fst >>| State.draw >>| fun t -> t.Grid.data)
  in
  let%sub actual_nodes =
    Bonsai.assoc
      (module Coord)
      rendered_nodes
      ~f:(Bonsai.Value.map2 ~f:render_char >+> return)
  in
  return
    (let%map broadcast = broadcast
     and actual_nodes = actual_nodes in
     let open Vdom.Node in
     Vdom.Node.div
       [ Event_handlers.on_click ~broadcast
       ; Vdom.Attr.style
           Css_gen.(
             font_family [ "monospace" ]
             @> width (`Px (grid_size * char_size))
             @> height (`Px (grid_size * char_size))
             @> position `Absolute
             @> overflow `Hidden)
       ]
       (Map.data actual_nodes))
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" app_component
;;
