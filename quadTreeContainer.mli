type t
val empty : Vector.t -> Vector.t -> t
val add : Ball.t -> int -> t -> t
val remove : Ball.t -> t -> t
val iter : (Ball.t -> unit) -> t -> unit
val map : (Ball.t -> unit) -> int -> t -> t

val is_colliding : Ball.t -> t -> bool