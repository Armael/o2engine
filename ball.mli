type t = {
  id : int;
  pos : Vector.t;
  speed : Vector.t;
  radius : float ;
  mass : float
}

val create : unit -> t
val is_colliding : t -> t -> bool
val print : t -> unit
