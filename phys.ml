open Utils

module type Container = sig
  type t
  val empty : unit -> t
  val add : Ball.t -> t -> t
  val iter : (Ball.t -> unit) -> t -> unit
  val map : (Ball.t -> Ball.t) -> t -> t
  val fold : ('a -> Ball.t -> 'a) -> 'a -> t -> 'a
end

module Make = 
  functor (C : Container) ->
struct
  type border = 
  | Right of float
  | Left of float
  | Top of float
  | Bottom of float

  type world = {
    balls : C.t;
    borders : (border list);
    new_id : int
  }

  let is_border_ok b = 
    let open Ball in
    let open Vector in
    function
    | Right f -> b.pos.x +. b.radius <= f
    | Left f -> b.pos.x -. b.radius >= f
    | Top f -> b.pos.y +. b.radius <= f
    | Bottom f -> b.pos.y -. b.radius >= f

  let inside w = 
    List.fold_left
      (fun (l, r, t, b) bord -> match bord with
      | Right f -> (match r with
	| Some x when f < x -> (l, Some f, t, b)
	| _ -> (l, r, t, b))
      | Left f -> (match l with
	| Some x when f > x -> (Some f, r, t, b)
	| _ -> (l, r, t, b))
      | Top f -> (match t with
	| Some x when f < x -> (l, r, Some f, b)
	| _ -> (l, r, t, b))
      | Bottom f -> (match b with
	| Some x when f > x -> (l, r, t, Some f)
	| _ -> (l, r, t, b)))
      (None, None, None, None)
      w.borders

  let is_in b w = 
    List.fold_left
      (fun a bord -> a && (is_border_ok b bord))
      true w.borders

  let new_world () = {
    balls = C.empty ();
    borders = [];
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

  let add_border out w = 
    {w with borders = out::(
      (List.filter (function
      | Right  _ -> (match out with Right  _ -> false | _ -> true)
      | Left   _ -> (match out with Left   _ -> false | _ -> true)
      | Bottom _ -> (match out with Bottom _ -> false | _ -> true)
      | Top    _ -> (match out with Top    _ -> false | _ -> true))
	 w.borders))
    }

  let simulate dt w = 
    let open Ball in
    let open Vector in
    { w with
      balls = 
	(C.map (fun b -> let dx = b.speed.x *. dt in
			 let dy = b.speed.y *. dt in
			 let new_x = b.pos.x +. dx in
			 let new_y = b.pos.y +. dy in
			 let new_b = {b with pos = {x = new_x; y = new_y}} in
			 let ((vx, vy), collided) = 
			   List.fold_left 
			     (fun ((vx, vy), collided) bord ->
			       if not (is_border_ok new_b bord) then
				 ((match bord with
				 | Right f -> (-.vx, vy)
				 | Left f -> (-.vx, vy)
				 | Top f -> (vx, -.vy)
				 | Bottom f -> (vx, -.vy)), true)
			       else 
				 ((vx, vy), collided)) 
			     ((b.speed.x, b.speed.y), false)
			     w.borders in
			 {
			   (if collided then b else new_b)
			  with speed = {x = vx; y = vy}
			 }
	 ) w.balls)
    }
end
