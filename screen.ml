type t = {
  mutable double_buffering : bool
}

type buffer = {
  mutable width : int;
  mutable height : int;
  priv : t
}

let create_buffer w h = {
  width = w;
  height = h;
  (* Double buffering activé par défaut *)
  priv = { double_buffering = true }
}

let open_buffer () =
  Graphics.open_graph " ";
  let w = Graphics.size_x () and h = Graphics.size_y () in
  let buf = create_buffer w h in
  Graphics.auto_synchronize (not buf.priv.double_buffering);
  buf

let close_buffer buf =
  Graphics.close_graph ()

let update buf =
  buf.width <- Graphics.size_x ();
  buf.height <- Graphics.size_y ()

let clear buf = Graphics.clear_graph ()
let resize w h buf = Graphics.resize_window w h

let set_color color buf = Graphics.set_color color
let moveto x y buf = Graphics.moveto x y
let rmoveto x y buf = Graphics.rmoveto x y
let draw_rect x y w h buf = Graphics.draw_rect x y w h
let fill_circle x y r buf = Graphics.fill_circle x y r

let draw f buf =
  f buf;
  if buf.priv.double_buffering then Graphics.synchronize ();
  update buf
