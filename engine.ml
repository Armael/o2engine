open Utils

module type PhysEngine = sig
  type border_type =
  Right | Left | Top | Bottom
  type world
  val new_world : unit -> world
  val iter : (Ball.t -> unit) -> world -> unit
  val set_border : border_type -> float -> world -> world
  val unset_border : border_type -> world -> world
  val set_restitution : float -> world -> world
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
  val open_buffer : unit -> buffer
  val close_buffer : buffer -> unit
  val update : buffer -> unit

  val clear : buffer -> unit
  val resize : int -> int -> buffer -> unit
  val set_color : Color.t -> buffer -> unit
  val moveto : int -> int -> buffer -> unit
  val rmoveto : int -> int -> buffer -> unit
  val draw_rect : int -> int -> int -> int -> buffer -> unit
  val fill_circle : int -> int -> int -> buffer -> unit

  val draw : (buffer -> unit) -> buffer -> unit
end

module Make =
  functor (P : PhysEngine) ->
    functor (G : GraphicEngine) ->
struct
  type world = {
    phys : P.world;
    buff : G.buffer;
    borders_follow_buff_size : bool;
    predraw_hook : (int * (G.buffer -> unit)) list;
    postdraw_hook : (int * (G.buffer -> unit)) list;
    predraw_hook_number : int;
    postdraw_hook_number : int
  }
  
  type border_type = P.border_type
  let new_world () = {
    phys = P.new_world ();
    buff = G.open_buffer ();
    borders_follow_buff_size = false;
    predraw_hook = [];
    postdraw_hook = [];
    predraw_hook_number = 0;
    postdraw_hook_number = 0
 }
  
  let set_border border_type value w = {w with phys = P.set_border border_type value w.phys}
  let unset_border border_type w = {w with phys = P.unset_border border_type w.phys}
  let set_restitution value w = {w with phys = P.set_restitution value w.phys}
  let borders_follow_buffer_size bool w = {w with borders_follow_buff_size = bool}
  let add_ball b w = {w with phys = P.add_ball b w.phys}
  let add_f f w = {w with phys = P.add_f f w.phys}
  let set_predraw_hook lf w = {w with predraw_hook = lf}
  let set_postdraw_hook lf w = {w with postdraw_hook = lf}
  let add_predraw_hook f w = (w.predraw_hook_number + 1, {w with predraw_hook = (w.predraw_hook_number + 1, f)::w.predraw_hook})
  let add_postdraw_hook f w = (w.postdraw_hook_number + 1, {w with postdraw_hook = (w.postdraw_hook_number + 1, f)::w.postdraw_hook})
  	let remove_hook i l = List.filter (fun (j,_) -> j = i) l
  let remove_predraw_hook i w =
  {w with predraw_hook = remove_hook i w.predraw_hook}
  let remove_postdraw_hook i w =
  {w with postdraw_hook = remove_hook i w.postdraw_hook}
   


  let display w =
    let open Ball in
    let open Vector in
    G.draw (fun buf ->
      G.clear buf;
      List.iter (fun c -> snd c buf) w.predraw_hook;
      P.iter (fun b ->
	G.set_color b.color buf;
	G.fill_circle (int b.pos.x) (int b.pos.y) (int b.radius) buf;
	G.set_color Color.black buf
      ) w.phys;
      List.iter (fun c -> snd c buf) w.postdraw_hook) w.buff;
    w

  let run fps world =
    let dt = 1. /. (float fps) in

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

    let rec loop dt w =
      (run_wait (fun () ->
	w >>=
	  update_borders >>= 
	  simulate dt >>=
	  display) dt) >>=
	loop dt in

    loop dt world
end
