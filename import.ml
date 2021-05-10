open Core_kernel

module T2 = struct
  include Tuple.T2

  let ( >. ) (x, y) (fx, fy) = fx x, fy y
  let ( <. ) f_ t = t >. f_
  let both_opt t = (uncurry Option.both) t
  let both_bool t = (uncurry ( && )) t
end

let compose2 f g a b = f (g a b)
let ( >+> ) g f = compose2 f g
let twice f a = f a a

let debug at message =
  if match at with
     | `Destroy_widget | `Mouse_event | `Mouse_up -> false
     | `Widget_redraw -> false
     | `Widget_create -> true
  then Js_of_ocaml.Firebug.console##log (at, message)
;;

include Composition_infix
include Js_of_ocaml
include Bonsai_web

module CONFIG_TO_REMOVE = struct
  let char_size = 15
  let grid_size = 40
end

include CONFIG_TO_REMOVE
