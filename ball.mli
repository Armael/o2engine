type t = {
  id : int;
  pos : Vector.t;
  speed : Vector.t;
  radius : float;
  mass : float;
  color : Color.t
}

val create : unit -> t
val print : t -> unit

(* Teste si deux balles sont entrées en collision *)
val is_colliding : t -> t -> bool
