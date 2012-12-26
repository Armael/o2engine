open Utils

module type Container = sig
  type t
  val empty : unit -> t
  val add : Ball.t -> t -> t
  val iter : (Ball.t -> unit) -> t -> unit
  val map : (Ball.t -> Ball.t) -> t -> t
  val fold : ('a -> Ball.t -> 'a) -> 'a -> t -> 'a
  val is_colliding : Ball.t -> t -> bool
  val collides_with : Ball.t -> t -> Ball.t list
end

module Make = 
  functor (C : Container) ->
struct
  type borders = {
    right : float option;
    left : float option;
    top : float option;
    bottom : float option
  }

  type border_type = 
  Right | Left | Top | Bottom

  type world = {
    balls : C.t;
    borders : borders;
    new_id : int
  }

  let is_border_ok b b_type = 
    let open Ball in
    let open Vector in
    function
    | None -> true
    | Some f -> (match b_type with
      | Right -> b.pos.x +. b.radius <= f
      | Left -> b.pos.x -. b.radius >= f
      | Top -> b.pos.y +. b.radius <= f
      | Bottom -> b.pos.y -. b.radius >= f)

  let is_in b w = 
    (is_border_ok b Right w.borders.right) &&
      (is_border_ok b Left w.borders.left) &&
      (is_border_ok b Top w.borders.top) &&
      (is_border_ok b Bottom w.borders.bottom)

  let new_world () = {
    balls = C.empty ();
    borders = {right = None;
	       left = None;
	       top = None;
	       bottom = None};
    new_id = 0
  }

  let iter f w = C.iter f w.balls

  let add_ball b w =
    let open Ball in
    if is_in b w then
      { w with 
	balls = C.add {b with id = w.new_id} w.balls;
	new_id = w.new_id + 1
      }
    else 
      w

  let set_border border_type value w = 
    let b = (
      match border_type with
      | Right -> {w.borders with right = Some value}
      | Left -> {w.borders with left = Some value}
      | Top -> {w.borders with top = Some value}
      | Bottom -> {w.borders with bottom = Some value}
    ) in
    {w with borders = b}

  let unset_border border_type w = 
    let b = (
      match border_type with
      | Right -> {w.borders with right = None}
      | Left -> {w.borders with left = None}
      | Top -> {w.borders with top = None}
      | Bottom -> {w.borders with bottom = None}
    ) in
    {w with borders = b}

  type collision_type =
  | Border of border_type
  | Ball of Ball.t

  (* Simule le mouvement d'une balle sans tenir compte des collisions
     pendant dt *)
  let simulate_ball_nc dt b =
    let open Ball in
    let open Vector in
    let dx = b.speed.x *. dt in
    let dy = b.speed.y *. dt in
    {b with pos = {
      x = b.pos.x +. dx;
      y = b.pos.y +. dy
    }}

  (* Simule l'évolution du monde sans tenir compte des collisions
     pendant dt *)
  let simulate_nc dt w =
    {w with
      balls = (C.map (simulate_ball_nc dt) w.balls)
    }

  (* Simule le mouvement d'une balle pendant dt *)
  let rec simulate_ball dt b w =
    let collides_one b w =
      let ( >>= ) o f = match o with
	| None -> f ()
	| Some a -> Some a in
      None >>=
	(fun _ -> if not (is_border_ok b Right w.borders.right) then
	    Some (Border Right) else None) >>=
	(fun _ -> if not (is_border_ok b Left w.borders.left) then
	    Some (Border Left) else None) >>=
	(fun _ -> if not (is_border_ok b Top w.borders.top) then
	    Some (Border Top) else None) >>=
	(fun _ -> if not (is_border_ok b Bottom w.borders.bottom) then
	    Some (Border Bottom) else None) in

    let rec dichotomia elapsed dt b w = 
      let open Ball in
      let open Vector in
      let dt_1px = 1. /. (Vector.norm b.speed) in
      if dt < dt_1px then (
	(elapsed, b, w)
      ) else (
	let half_dt = dt /. 2. in
	let half_b = simulate_ball_nc half_dt b
	and half_w = simulate_nc half_dt w in
	if collides_one half_b half_w = None then
	  dichotomia (elapsed +. half_dt) half_dt half_b half_w
	else dichotomia elapsed half_dt b w
      ) in

    if dt <= 0. then b else (
      let new_b = simulate_ball_nc dt b
      and new_w = simulate_nc dt w in
      let collides = collides_one new_b new_w in
      match collides with
      | None -> new_b
      | Some obj ->
	let (t, new_b, new_w) = dichotomia 0. dt b w in
		let open Ball in
		let open Vector in
		(match obj with
		| Border bord -> simulate_ball (dt -. t)
		  (match bord with
		  | Top | Bottom -> {new_b with speed = {new_b.speed with y = -. new_b.speed.y}}
		  | Left | Right -> {new_b with speed = {new_b.speed with x = -. new_b.speed.x}}
		  ) new_w 
		| Ball _ -> new_b (* Collision entre balles pas encore gérée *)
		)
	    )

  (* Simule l'évolution du monde pendant un temps dt *)
  let simulate dt w = 
    { w with
      balls = 
	(C.map (fun b -> simulate_ball dt b w) w.balls)
    }
end
