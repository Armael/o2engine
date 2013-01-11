open Utils

module type PhysEngine = sig
  type border_type =
  Right | Left | Top | Bottom
  type world
  val new_world : Vector.t -> Vector.t -> world
  val iter : (Ball.t -> unit) -> world -> unit
  val resize : Vector.t -> Vector.t -> world -> world
  val map : (Ball.t -> Ball.t) -> world -> world
  val remove : Ball.t -> world -> world
  val modify : Ball.t -> Ball.t -> world -> world
  val modify_i : int -> (Ball.t -> Ball.t) -> world -> world

  val set_border : border_type -> float -> world -> world
  val unset_border : border_type -> world -> world
  val set_restitution : float -> world -> world
  val get_ball_collisions_log : world -> (Ball.t * Ball.t) Queue.t
  val get_borders_collisions_log : world -> (Ball.t * border_type) Queue.t
  val log_ball_collisions : bool -> world -> world
  val log_borders_collisions : bool -> world -> world
  val add_ball : Ball.t -> world -> world
  val add_f : (Ball.t -> Vector.t) -> world -> world
  val simulate : float -> world -> world
end

module type GraphicEngine = sig
  type t
  type buffer = {
    mutable width : int;
    mutable height : int;
    priv : t
  }
  val open_buffer : int -> int -> buffer
  val close_buffer : buffer -> unit
  val update : buffer -> unit

  val clear : buffer -> unit
  val resize : int -> int -> buffer -> unit
  val set_color : Color.t -> buffer -> unit
  val moveto : int -> int -> buffer -> unit
  val rmoveto : int -> int -> buffer -> unit
  val draw_rect : int -> int -> int -> int -> buffer -> unit
  val fill_rect : int -> int -> int -> int -> buffer -> unit
  val fill_circle : int -> int -> int -> buffer -> unit
  val draw_circle : int -> int -> int -> buffer -> unit
  val lineto : int -> int -> buffer -> unit
  val rlineto : int -> int -> buffer -> unit
  val set_text_size : int -> buffer -> unit
  val draw_string : int -> int -> string -> buffer -> unit

  val draw : (buffer -> unit) -> buffer -> unit
end

module Int = struct
  type t = int
  let compare = compare
end


module Make =
  functor (P : PhysEngine) ->
    functor (G : GraphicEngine) ->
struct

  module IMap = Map.Make (Int)

  type world = {
    phys : P.world;
    buff : G.buffer;
    borders_follow_buff_size : bool;
    predraw_hooks : (G.buffer -> unit) IMap.t;
    ball_hook : (Ball.t -> G.buffer -> unit);
    postdraw_hooks : (G.buffer -> unit) IMap.t;
    fps : float Queue.t;
    user_action : Ui.status -> world -> world
  }

  type border_type = P.border_type

  let buff_rect buff = 
    let v1 = {Vector.x = 0.; Vector.y = 0.}
    and v2 = {Vector.x = float buff.G.width;
	      Vector.y = float buff.G.height} in
    (v1, v2)

  (* Étend le rectangle (v1, v2) pour laisser des marges de <margin> px de
     chaque côté *)
  let extend (v1, v2) =
    let margin = 50. in
    let open Vector in
    ({x = v1.x -. margin; y = v1.y -. margin},
     {x = v2.x +. margin; y = v2.y +. margin})

  let new_world width height = 
    let buff = G.open_buffer width height in
    let (v1, v2) = extend (buff_rect buff) in
    {
      phys = P.new_world v1 v2;
      buff = buff;
      borders_follow_buff_size = false;
      fps = Queue.create ();
      predraw_hooks = IMap.empty;
      ball_hook = (fun b buf -> 
	let open Vector in
	let open Ball in
	G.set_color b.color buf;
	G.fill_circle (int b.pos.x) (int b.pos.y) (int b.radius) buf;
	G.set_color Color.black buf);
      postdraw_hooks = IMap.empty;
      user_action = (fun _ w -> w)
    }

  let map f w = {w with phys = P.map f w.phys}
  let remove b w = {w with phys = P.remove b w.phys}

  let iter f w = P.iter f w.phys
  let modify b b' w = {w with phys = P.modify b b' w.phys}
  let modify_i i f w = {w with phys = P.modify_i i f w.phys}

  let set_border border_type value w = {w with phys = P.set_border border_type value w.phys}
  let unset_border border_type w = {w with phys = P.unset_border border_type w.phys}
  let set_restitution value w = {w with phys = P.set_restitution value w.phys}
  let borders_follow_buffer_size bool w = {w with borders_follow_buff_size = bool}
  let add_ball b w = {w with phys = P.add_ball b w.phys}
  let add_f f w = {w with phys = P.add_f f w.phys}

  let set_predraw_hooks f w = {w with predraw_hooks = f w.predraw_hooks}
  let set_ball_hook f w = {w with ball_hook = f}
  let set_postdraw_hooks f w = {w with postdraw_hooks = f w.postdraw_hooks}

  let get_ball_collisions_log w = P.get_ball_collisions_log w.phys
  let get_borders_collisions_log w = P.get_borders_collisions_log w.phys
  let log_ball_collisions b w = {w with phys = P.log_ball_collisions b w.phys}
  let log_borders_collisions b w = {w with phys = P.log_borders_collisions b w.phys}

  let set_user_action f w = {w with user_action = f}

  let display w =
    let (nb, sum) = Queue.fold (fun (nb, sum) x -> (nb+.1., sum+.x)) (0., 0.) w.fps in
    let fps_moy = sum /. nb in

    let draw_fps buf =
      G.set_text_size 10 buf;
      G.draw_string (buf.G.width - 50) (buf.G.height - 20)
	(Printf.sprintf "%3.f FPS" fps_moy) buf in

    let open Ball in
    let open Vector in
    G.draw (fun buf ->
      G.clear buf;
      IMap.iter (fun _ f -> f buf) w.predraw_hooks;
      P.iter (fun b ->
	w.ball_hook b buf;
      ) w.phys;
      G.set_color Color.black buf;
      draw_fps buf;
      IMap.iter (fun _ f -> f buf) w.postdraw_hooks) w.buff;
    w
      
  let run fps world =
    let dt = 1. /. (float fps) in

    let resize_world w =
      let (v1, v2) = extend (buff_rect w.buff) in
      {w with phys = P.resize v1 v2 w.phys} in

    let update_borders w = 
      let open G in
      G.update w.buff; 
      if w.borders_follow_buff_size then
	w >>= 
	  set_border P.Right (float w.buff.width) >>=
	  set_border P.Left 0. >>=
	  set_border P.Top (float w.buff.height) >>=
	  set_border P.Bottom 0.
      else w
    in
    let simulate dt w = {w with phys = P.simulate dt w.phys} in

    let run_wait_update_fps f dt =
      let (delay, res) = run_wait f dt in
      let cur_fps = 1. /. delay in
      if not (Queue.is_empty res.fps) then
	ignore (Queue.pop res.fps);
      Queue.push cur_fps res.fps; res in

    let rec loop dt w =
      (run_wait_update_fps (fun () ->
	w >>=
	  update_borders >>=
	  simulate dt >>=
	  w.user_action (Ui.get_status ()) >>=
	  resize_world >>=
	  display) dt) >>=
	loop dt in

    loop dt world
      
end
