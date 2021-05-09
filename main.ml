open Import
open Core_kernel
open Js_of_ocaml
open Bonsai_web
open Bonsai.Let_syntax

let char = function
  | `Blank -> "&nbsp;"
  | `Placeholder -> "."
;;

module State = struct
  type t = { animations : Animation.Circle.t list } [@@deriving sexp, equal]

  let on_frame t =
    { t with
      animations =
        List.filter_map t.animations ~f:(fun animation ->
            match Animation.Circle.step animation with
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
      ~init:(Grid.create grid_size ~f:(Fn.const `Blank))
      t.animations
      ~f:(fun accum ->
        Animation.Circle.rasterise
        >> Coord.Set.fold ~init:accum ~f:(fun accum key ->
               Grid.set accum ~key ~data:`Placeholder))
  ;;
end
[@@ocaml.warning "-23"]

module Action = struct
  type t =
    [ `Increment_frame
    | `Click of Coord.t
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

  let coord_tag = "data-coord"

  let handle ~broadcast eventtype action =
    eventtype (fun e ->
        Firebug.console##log e;
        match
          let%bind.Option tgt = e##.target |> Js.Opt.to_option in
          tgt##getAttribute (Js.string coord_tag) |> Js.Opt.to_option
        with
        | None -> Ui_event.Ignore
        | Some coord_str ->
          coord_str |> Js.to_string |> Coord.of_string_exn |> action |> broadcast)
  ;;

  let doc_attrs broadcast =
    let handle = handle ~broadcast in
    Vdom.Attr.[ handle on_mousedown Action.click ]
  ;;

  let tile_attr coord = Vdom.Attr.create coord_tag (Coord.to_string coord)
end

module Tile = struct
  (* type t = { char : Unicode_character.t } *)

  let render coord t =
    Vdom.Node.inner_html
      ~tag:"span"
      [ Event_handlers.tile_attr coord
      ; Vdom.Attr.style
          Css_gen.(
            display `Inline_block @> width (`Px char_size) @> height (`Px char_size))
      ]
      ~this_html_is_sanitized_and_is_totally_safe_trust_me:(char t)
  ;;
end

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
  let%sub rendered_nodes = return (state_machine >>| fst >>| State.draw >>| Grid.data) in
  let%sub actual_nodes =
    Bonsai.assoc
      (module Coord)
      rendered_nodes
      ~f:(Bonsai.Value.map2 ~f:Tile.render >+> return)
  in
  return
    (let%map broadcast = broadcast
     and actual_nodes = actual_nodes in
     Vdom.Node.div
       (Event_handlers.doc_attrs broadcast
       @ [ Vdom.Attr.style
             Css_gen.(
               font_family [ "monospace" ]
               @> width (`Px (grid_size * char_size))
               @> height (`Px (grid_size * char_size))
               @> position `Absolute
               @> overflow `Hidden)
         ])
       (Map.data actual_nodes))
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" app_component
;;
