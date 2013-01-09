open Utils

module type Container = sig
  type cont
  type t = Vector.t * Vector.t * cont
  val empty : Vector.t -> Vector.t -> t
  val add : Ball.t -> t -> t
  val iter : (Ball.t -> unit) -> t -> unit
  val map : (Ball.t -> Ball.t) -> t -> t
  val resize : Vector.t -> Vector.t -> t -> t
  val iterate_solve_collisions : (Ball.t -> Ball.t -> Ball.t * Ball.t) -> t -> t
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
    f : (Ball.t -> Vector.t) list;
    borders : borders;
    restitution : float
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

  let new_world v1 v2 = {
    balls = C.empty v1 v2;
    f = [];
    borders = {right = None;
	       left = None;
	       top = None;
	       bottom = None};
    restitution = 1.
  }

  let iter f w = C.iter f w.balls

  let resize new_v1 new_v2 w = 
    let (v1, v2, _) = w.balls in
    if new_v1 <> v1 || new_v2 <> v2 then
      {w with balls = C.resize v1 v2 w.balls}
    else
      w

  let add_ball b w =
    let open Ball in
    if is_in b w then
      { w with 
	balls = C.add b w.balls
      }
    else 
      w

  let add_f f w =
    { w with
      f = f::w.f
    }

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

  let set_restitution value w =
    { w with restitution = value}

  let b2b_collision_solver w b1 b2 =
    let open Ball in
    let open Vector in
    let delta = b1.pos -- b2.pos in
    let d = norm delta in
    let d_unit = unit delta in
    let mtd = ((b1.radius +. b2.radius) -. d) ** d_unit in
    
    let im1 = 1. /. b1.mass and im2 = 1. /. b2.mass in
    let b1 = {b1 with pos = b1.pos ++ ((im1 /. (im1 +. im2)) ** mtd)} in
    let b2 = {b2 with pos = b2.pos -- ((im2 /. (im1 +. im2)) ** mtd)} in
    
    let v = b1.speed -- b2.speed in
    let vn = v |. d_unit in
    
    if vn > 0. then (b1, b2) else
      let i = (-.(1. +. w.restitution) *. vn) /. (im1 +. im2) in
      let impulse = i ** d_unit in
      let b1 = {b1 with speed = b1.speed ++ (im1 ** impulse)} in
      let b2 = {b2 with speed = b2.speed -- (im2 ** impulse)} in
      (b1, b2)

  let solve_wall_collisions w =
    {w with balls =
	C.map (fun b ->
	  let open Ball in
	  let open Vector in
	  let get = function
	    | None -> failwith "Empty"
	    | Some x -> x in
	  let c = w.restitution in
	  b >>=
	    (fun b ->
	      if w.borders.right <> None &&
		b.pos.x > get w.borders.right -. b.radius then
		{b with pos = {b.pos with x = get w.borders.right -. b.radius};
		  speed = {b.speed with x = -. c *. b.speed.x}} else b) >>=
	    (fun b ->
	      if w.borders.left <> None &&
		b.pos.x < get w.borders.left +. b.radius then
		{b with pos = {b.pos with x = get w.borders.left +. b.radius};
		  speed = {b.speed with x = -. c *. b.speed.x}} else b) >>=
	    (fun b ->
	      if w.borders.top <> None &&
		b.pos.y > get w.borders.top -. b.radius then
		{b with pos = {b.pos with y = get w.borders.top -. b.radius};
		  speed = {b.speed with y = -. c *. b.speed.y}} else b) >>=
	    (fun b ->
	      if w.borders.bottom <> None &&
		b.pos.y < get w.borders.bottom +. b.radius then
		{b with pos = {b.pos with y = get w.borders.bottom +. b.radius};
		  speed = {b.speed with y = -. c *. b.speed.y}} else b)) w.balls}

  (* Simule le mouvement d'une balle sans tenir compte des collisions
     pendant dt *)
  let simulate_ball_nc dt w b =
    let open Ball in
    let open Vector in
    let b = List.fold_left (fun acc f ->
      let coef = dt /. acc.mass in
      let f_acc = f acc in
      {acc with speed = {
	x = acc.speed.x +. (coef ** f_acc).x;
	y = acc.speed.y +. (coef ** f_acc).y
      }}) b w.f in
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
      balls = (C.map (simulate_ball_nc dt w) w.balls)
    }

  (* Simule l'évolution du monde pendant un temps dt *)
  let simulate dt w = 
    let w = solve_wall_collisions w in

    {w with balls =
	C.iterate_solve_collisions (b2b_collision_solver w) (simulate_nc dt w).balls}
end
