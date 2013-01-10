open Utils
open Event

module type PhysEngine = sig
  type border_type =
  Right | Left | Top | Bottom
  type world
  val new_world : Vector.t -> Vector.t -> world
  val iter : (Ball.t -> unit) -> world -> unit
  val resize : Vector.t -> Vector.t -> world -> world
  val map : (Ball.t -> Ball.t) -> world -> world

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
  type ('a,'b,'c) world = {
    phys : P.world;
    buff : G.buffer;
    borders_follow_buff_size : bool;
    predraw_hook : (int * ((('a option) * ('b option) * ('c option)) -> G.buffer -> ('a,'b,'c) world-> ('a,'b,'c) world)) list;
    postdraw_hook : (int * ((('a option) * ('b option) * ('c option)) -> G.buffer -> ('a,'b,'c) world -> ('a,'b,'c) world)) list;
    predraw_hook_number : int;
    postdraw_hook_number : int;
    engine_handler : ('a,'b,'c) Event.event_handler
  }
  
  type border_type = P.border_type

  let buff_rect buff = 
    let v1 = {Vector.x = 0.; Vector.y = 0.}
    and v2 = {Vector.x = float buff.G.width;
	      Vector.y = float buff.G.height} in
    (v1, v2)

  let new_world () = 
    let buff = G.open_buffer () in
    let (v1, v2) = buff_rect buff in
    {
      phys = P.new_world v1 v2;
      buff = buff;
      borders_follow_buff_size = false;
      predraw_hook = [];
      postdraw_hook = [];
      predraw_hook_number = 0;
      postdraw_hook_number = 0;
      engine_handler = Event.create_handler ()
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

  let set_world_keypress_handler fkh w = {w with predraw_hook = [] ; postdraw_hook = [] ; engine_handler = {w.engine_handler with keypress_handler = fkh}}
  let set_world_button_handler fbh w ={w with predraw_hook = [] ; postdraw_hook = [] ; engine_handler =  {w.engine_handler with button_handler = fbh}}
  let set_world_pos_handler fph w = {w with predraw_hook = [] ; postdraw_hook = [] ; engine_handler =  {w.engine_handler with pos_handler = fph}}

  let display w =
    let open Ball in
    let open Vector in
    let event_result = handle_event w.engine_handler in
    let pre_new_w = List.fold_right (fun c -> (snd c event_result w.buff)) w.predraw_hook w in
    G.draw (fun buf ->
      G.clear buf;
      P.iter (fun b ->
	G.set_color b.color buf;
	G.fill_circle (int b.pos.x) (int b.pos.y) (int b.radius) buf;
	G.set_color Color.black buf
      ) pre_new_w.phys;) pre_new_w.buff;
      let post_new_w = List.fold_right (fun c -> (snd c event_result pre_new_w.buff)) pre_new_w.postdraw_hook pre_new_w in
    post_new_w

  let run fps world =
    let dt = 1. /. (float fps) in

    let resize_world w =
      let (v1, v2) = buff_rect w.buff in
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

    let rec loop dt w =
      (run_wait (fun () ->
	w >>=
	  update_borders >>= 
	  resize_world >>=
	  simulate dt >>=
	  display) dt) >>=
	loop dt in

    loop dt world
    
  let user_map f w = 
  {w with phys = P.map f w.phys}

end
