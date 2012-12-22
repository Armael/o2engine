open Utils

module type PhysEngine = sig
  type border_type =
  Right | Left | Top | Bottom
  type world
  val new_world : unit -> world
  val iter : (Ball.t -> unit) -> world -> unit
  val set_border : border_type -> float -> world -> world
  val unset_border : border_type -> world -> world
  val add_ball : Ball.t -> world -> world
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
  type color
  val white : color
  val black : color
  val red : color
  val green : color
  val blue : color
  val yellow : color
  val cyan : color
  val magenta : color
  val rgb : int -> int -> int -> color
  val set_color : color -> buffer -> unit
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
  type world = P.world * G.buffer
  type border_type = P.border_type
  let new_world () = (P.new_world (), G.open_buffer ())
  let set_border border_type value w = (P.set_border border_type value (fst w), snd w)
  let unset_border border_type w = (P.unset_border border_type (fst w), snd w)
  let add_ball b w = (P.add_ball b (fst w), snd w)

  let display w =
    let open Ball in
    let open Vector in
    G.draw (fun buf ->
      G.clear buf;
      P.iter (fun b ->
	G.fill_circle (int b.pos.x) (int b.pos.y) (int b.radius) buf) (fst w))
      (snd w);
    w

  let run fps world =
    let dt = 1. /. (float fps) in

    let wait_pass d w =
      Utils.sleep d;
      w in

    let simulate dt w = (P.simulate dt (fst w), snd w) in

    let rec loop dt w =
      w >>=
	wait_pass dt >>=
	simulate dt >>=
	display >>=
	loop dt in

    loop dt world
end
