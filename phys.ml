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

  let simulate dt w = 
    let open Ball in
    let open Vector in
    { w with
      balls = 
	(C.map (fun b -> 
	  let dx = b.speed.x *. dt in
	  let dy = b.speed.y *. dt in
	  let new_x = b.pos.x +. dx in
	  let new_y = b.pos.y +. dy in
	  let new_b = {b with pos = {x = new_x; y = new_y}} in
	  let ((vx, vy), collided) = 
	    ((b.speed.x, b.speed.y), false) >>=
	      
	      (fun ((vx, vy), collided) ->
		if not (is_border_ok new_b Right w.borders.right) ||
		  not (is_border_ok new_b Left w.borders.left) then
		  ((-.vx, vy), true)
		else ((vx, vy), false)) >>=

	      (fun ((vx, vy), collided) ->
		if not (is_border_ok new_b Top w.borders.top) ||
		  not (is_border_ok new_b Bottom w.borders.bottom) then
		  ((vx, -.vy), true)
		else ((vx, vy), false)) in
	  {
	    (if collided then b else new_b)
	   with speed = {x = vx; y = vy}
	  }
	 ) w.balls)
    }
end
