type t = Ball.t list
val empty : unit -> t
val add : Ball.t -> t -> t
val iter : (Ball.t -> unit) -> t -> unit
val map : (Ball.t -> Ball.t) -> t -> t

val is_colliding : Ball.t -> t -> bool

val iterate_solve_collisions : (Ball.t -> Ball.t -> Ball.t * Ball.t) -> t -> t
