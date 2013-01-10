type cont
type t = Vector.t * Vector.t * cont
val empty : Vector.t -> Vector.t -> t
val add : Ball.t -> t -> t
val remove : Ball.t -> t -> t
val iter : (Ball.t -> unit) -> t -> unit
val map : (Ball.t -> Ball.t) -> t -> t
val resize : Vector.t -> Vector.t -> t -> t

val is_colliding : Ball.t -> t -> bool

val iterate_solve_collisions : (Ball.t -> Ball.t -> Ball.t * Ball.t) -> t -> t

