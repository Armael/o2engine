module V = Vector

type t = {
  id : int;
  pos : V.t;
  speed : V.t;
  radius : float ;
  mass : float
}

let create () = {
  id = 0;
  pos = V.create ();
  speed = V.create ();
  radius = 0.;
  mass = 0.
}

let print b = Printf.printf "{id:%d; pos:" b.id;
  V.print b.pos;
  Printf.printf "; speed:";
  V.print b.speed;
  Printf.printf "; radius: %f, mass: %f}" b.radius b.mass

let is_colliding b1 b2 =
  let delta = V.sub b1.pos b2.pos in
  (V.sp delta delta) <= (b1.radius +. b2.radius) ** 2.


