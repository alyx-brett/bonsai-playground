[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-23"]

open! Core_kernel
open Js_of_ocaml
open Bonsai_web
open Bonsai.Let_syntax

module Pixel = struct
  module Coordinate = struct
    type t = int * int [@@deriving sexp, equal]
  end
end

module Animation = struct
  type t =
    { increment : unit -> [ `End | `Cont of t ]
    ; draw : unit -> Vdom.Node.t
    }

  module type S = sig
    type t

    val increment : t -> [ `End | `Cont of t ]
    val draw : t -> Vdom.Node.t
  end

  let rec create : type a. (module S with type t = a) -> a -> t =
   fun m x ->
    let (module X) = m in
    { increment =
        (fun () ->
          match X.increment x with
          | `End -> `End
          | `Cont next -> `Cont (create m next))
    ; draw = (fun () -> X.draw x)
    }
 ;;

  module Circle = struct
    type t =
      { centre : Pixel.Coordinate.t
      ; radius : float
      ; duration : int
      ; remaining : int
      }
    [@@deriving sexp, equal]

    let create ~centre =
      { centre; radius = Random.float_range 10. 50.; duration = 60; remaining = 60 }
    ;;

    let increment t =
      match t.remaining with
      | 0 -> `End
      | _ -> `Cont { t with remaining = t.remaining - 1 }
    ;;

    let draw t =
      let scaled_rad =
        `Px_float (t.radius *. 2. *. ((t.duration - t.remaining) // t.duration))
      in
      Vdom.Node.div
        [ Vdom.Attr.style
            Css_gen.(
              border ~width:(`Px 1) ~color:(`Name "black") ~style:`Solid ()
              @> border_radius (`Percent (Percent.of_percentage 50.))
              @> position `Absolute
              @> top (`Px (snd t.centre))
              @> left (`Px (fst t.centre))
              @> create ~field:"transform" ~value:"translate(-50%, -50%)"
              @> height scaled_rad
              @> width scaled_rad)
        ]
        []
    ;;
  end
end

module App = struct
  let name = "app"

  module Model = struct
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

    let on_click t coord =
      { t with animations = Animation.Circle.create ~centre:coord :: t.animations }
    ;;

    let increment_frame_counter t () = assert false
    let default = { animations = [] }
  end

  module Action = struct
    type t =
      [ `Increment_frame
      | `Click of Pixel.Coordinate.t
      ]
    [@@deriving sexp_of]
  end

  let apply_action ~inject:_ ~schedule_event:_ model : Action.t -> Model.t = function
    | `Increment_frame -> Model.on_frame model
    | `Click coord -> Model.on_click model coord
  ;;

  let draw t ~handle_action =
    let on_click =
      Vdom.Attr.on_click (fun e ->
          Firebug.console##log e;
          match
            Option.both
              (e##.pageX |> Js.Optdef.to_option)
              (e##.pageY |> Js.Optdef.to_option)
          with
          | None -> Ui_event.Ignore
          | Some coord -> handle_action @@ `Click coord)
    in
    List.map t.Model.animations ~f:Animation.Circle.draw
    |> Vdom.Node.div
         [ on_click
         ; Vdom.Attr.style
             Css_gen.(
               width Length.percent100
               @> height Length.percent100
               @> position `Absolute
               @> overflow `Hidden)
         ]
  ;;
end

let app_component =
  let%sub state_machine =
    Bonsai.state_machine0
      [%here]
      (module App.Model)
      (module App.Action)
      ~default_model:App.Model.default
      ~apply_action:App.apply_action
  in
  let%sub _ =
    Bonsai.Edge.after_display
      (let%map _, handle_action = state_machine in
       handle_action `Increment_frame)
  in
  return
  @@ let%map model, handle_action = state_machine in
     App.draw model ~handle_action
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" app_component
;;
