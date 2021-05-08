[@@@ocaml.warning "-27"]

[@@@ocaml.warning "-23"]

open! Core_kernel
open Bonsai_web
open Bonsai.Let_syntax

let component = Bonsai.const (Vdom.Node.text "hello world")

module Pixel = struct
  module Coordinate = struct
    type t = int * int [@@deriving sexp]
  end
end

module Animation = struct
  module type S = sig
    type t
    val increment : t -> [`End | `Cont of t]
    val draw : t -> Vdom.Node.t
  end

  module Circle = struct
    type t = {centre : Pixel.Coordinate.t; radius : float; duration : int; remaining : int}

    let create ~centre =
      { centre; radius = Random.float_range 10. 50.;
        duration= 60; remaining = 60}

    let increment t = {t with remaining = t.remaining -1}

    let draw t =
      let scaled_rad = `Px @@ t.radius * 2 * ((duration - remaining) // duration) in
      Vdom.Node.( div [
        style
        Css_gen.(border ~width:(`Px 1) ~color:(`Name "black") ~style:`Solid () @>
                 border_radius (`Percent @@ Percent.of_percentage 50.)
                 @> position `Absolute
                 @> top (`Px (snd t.centre))
                 @> left (`Px (fst t.centre))
                 @> create ~field:"transform" ~value:"translate(-50%, -50%)"
                   @> height scaled_rad @>width scaled_rad)
      ] [])
                      end
end


module App = struct
  let name = "app"

  module Model = struct
    type t = { frame_number : int } [@@deriving sexp, equal]

    let increment_frame_counter t () =
      { t with frame_number = t.frame_number + 1 }

    let default = { frame_number = 0 }
  end

  module Action = struct
    type t = [ `Increment_frame | `Click of Pixel.Coordinate.t ]
    [@@deriving sexp_of]
  end

  let apply_action ~inject:_ ~schedule_event:_ model : Action.t -> Model.t =
    function
    | `Increment_frame -> Model.increment_frame_counter model ()
    | `Click coord -> 
end

let app_component =
  let%sub state_machine =
    Bonsai.state_machine0 [%here]
      (module App.Model)
      (module App.Action)
      ~default_model:App.Model.default ~apply_action:App.apply_action
  in
  let%sub _ =
    Bonsai.Edge.after_display
      (let%map _, handle_action = state_machine in
       handle_action `Increment_frame)
  in
  return
  @@ let%map model, handle_action = state_machine in
     Vdom.Node.(div [] [ text @@ Int.to_string model.App.Model.frame_number ])

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app"
    app_component
