type t = Ball.t list
val empty : unit -> t
val add : Ball.t -> t -> t
val iter : (Ball.t -> unit) -> t -> unit
val map : (Ball.t -> Ball.t) -> t -> t

