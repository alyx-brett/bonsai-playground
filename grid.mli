type 'a t

val create : int -> f:(Coord.t -> 'a) -> 'a t
val set : 'a t -> key:Coord.t -> data:'a -> 'a t
val data : 'a t -> 'a Coord.Map.t
