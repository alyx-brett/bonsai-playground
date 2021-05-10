open Core_kernel
open Import
open Bonsai.Let_syntax

let char = function
  | `Blank -> "" (* "&nbsp;" *)
  | `Placeholder -> "."
;;

module Action = struct
  type t =
    [ `Increment_frame
    | `Mousedown of Coord.t
    | `Mouseenter of Coord.t
    | `Mouseup
    ]
  [@@deriving sexp_of, variants]
end

module Event_handlers = struct
  let coord_tag = "data-coord"
  let stop a = Vdom.Event.Many [ a; Vdom.Event.Stop_propagation ]

  let doc_attrs broadcast =
    let handle eventtype action =
      let get_coord (e : Dom_html.mouseEvent Js.t) =
        let%bind.Option tgt = e##.target |> Js.Opt.to_option in
        let%map.Option coord_str =
          tgt##getAttribute (Js.string coord_tag) |> Js.Opt.to_option
        in
        coord_str |> Js.to_string |> Coord.of_string_exn
      in
      eventtype (fun e ->
          debug `Mouse_event e;
          match get_coord e with
          | None -> Ui_event.Ignore
          | Some c -> action c |> broadcast |> stop)
    in
    Vdom.Attr.
      [ handle on_mousedown Action.mousedown; handle on_mouseover Action.mouseenter ]
  ;;

  let tile_attr coord = Vdom.Attr.create coord_tag (Coord.to_string coord)

  let global broadcast =
    let%sub _ = Bonsai.Edge.after_display (broadcast >>| ( |> ) `Increment_frame) in
    return
      (let%map broadcast = broadcast in
       Dom_html.window##.onmouseup
         := Dom.handler (fun e ->
                debug `Mouse_up e;
                Vdom.Event.Expert.handle e (broadcast Action.mouseup |> stop);
                Js._true);
       broadcast)
  ;;
end

module State = struct
  module Mouse = struct
    type t =
      | Down
      | Up
    [@@deriving sexp, equal]
  end

  type t =
    { animations : unit Animation.Circle.Map.t
    ; mouse : Mouse.t
    }
  [@@deriving sexp, equal]

  let on_frame t =
    { t with
      animations =
        Map.filter_mapi t.animations ~f:(fun ~key ~data:() ->
            match Animation.Circle.step key with
            | `End -> None
            | `Cont a -> Some a)
    }
  ;;

  let start_circle t ~coord =
    match t.mouse with
    | Up -> t
    | Down ->
      { t with
        animations =
          Animation.Circle.(Map.add_exn t.animations ~key:(create ~centre:coord) ~data:())
      }
  ;;

  let on_click t ~coord = { t with mouse = Down } |> start_circle ~coord

  let handle_action t = function
    | `Increment_frame -> on_frame t
    | `Mousedown coord -> on_click t ~coord
    | `Mouseenter coord -> start_circle t ~coord
    | `Mouseup -> { t with mouse = Up }
  ;;

  let initial = { animations = Map.empty; mouse = Up }

  module Tile : sig
    module Input : sig
      type t =
        { content : string
        ; coord : Coord.t
        }
      [@@deriving sexp]
    end

    include Vdom.Node.Widget.S with module Input := Input
  end = struct
    let name = "tile"

    type dom = Dom_html.element

    module Input = struct
      type t =
        { content : string
        ; coord : Coord.t
        }
      [@@deriving sexp]
    end

    module State = struct
      type t = unit [@@deriving sexp]
    end

    let create (input : Input.t) =
      debug `Widget_create input;
      ( ()
      , Vdom.Node.inner_html
          ~tag:"span"
          [ Event_handlers.tile_attr input.coord ]
          ~this_html_is_sanitized_and_is_totally_safe_trust_me:input.content
        |> Vdom.Node.to_dom )
    ;;

    let update ~prev_input:_ ~(input : Input.t) ~state:() ~(element : dom Js.t) =
      debug `Widget_redraw element;
      let () = element##.innerHTML := Js.string input.content in
      (), element
    ;;

    let destroy ~prev_input:_ ~state:() ~element =
      debug `Destroy_widget element;
      assert false
    ;;
  end

  let create_tile = unstage @@ Vdom.Node.widget_of_module (module Tile)

  (* general strategy;
     - for each tile, calculate everything that wants to draw to it 
     - order those possible drawings by importance (layer * recentness)
     - choose the most important and render it
  *)
  let draw_incr (t : t Incr.t) : Vdom.Node.t Coord.Map.t Incr.t =
    let open Animation in
    let open Incr in
    let background = Grid.create grid_size ~f:(Fn.const `Blank) |> Grid.data |> return in
    let to_draw =
      let circles =
        t
        >>| fun t ->
        Circle.Map.mapi t.animations ~f:(fun ~key ~data:() ->
            Circle.rasterise_preprocess key)
      in
      Map.mapi circles ~f:(fun ~key ~data:_ -> Circle.rasterise key)
      |> Map.transpose (module Coord)
      |> Map.map ~f:(fun _units_by_circle ->
             (* let first_tile = 
              *   Circle.Map.nth units_by_circle 1 |> Option.value_exn |>
              *   in *)
             `Placeholder)
    in
    Map.merge background to_draw ~f:(fun ~key:_ -> function
      | `Both (_, t) | `Left t -> Some t
      | `Right _ -> None)
    |> Map.mapi ~f:(fun ~key ~data -> create_tile { content = char data; coord = key })
  ;;
end
[@@ocaml.warning "-23"]

let app_component =
  let%sub state_machine =
    Bonsai.state_machine0
      [%here]
      (module State)
      (module Action)
      ~default_model:State.initial
      ~apply_action:(fun ~inject:_ ~schedule_event:_ state action ->
        State.handle_action state action)
  in
  let%sub broadcast = state_machine >>| snd |> Event_handlers.global in
  let%sub rendered_nodes =
    state_machine >>| fst |> Bonsai.Incr.compute ~f:State.draw_incr
  in
  return
    (let%map broadcast = broadcast
     and actual_nodes = rendered_nodes in
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
