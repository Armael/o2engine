(* Module abstrayant l'implémentation de la sortie graphique : ici, il
   s'agit de la sortie écran, implémentée grâce au module Graphics.
   Il s'occupe également en interne du double-buffering, puisque
   celui-ci est spécifique à l'implémentation.
*)

(* Informations privées *)
type t = {
  mutable double_buffering : bool
}

(* Buffer d'affichage *)
type buffer = {
  mutable width : int;
  mutable height : int;
  priv : t
}

(* Crée un nouveau buffer d'affichage (n'est pas utilisé par
   l'utilisateur du module) *)
let create_buffer w h = {
  width = w;
  height = h;
  (* Double buffering activé par défaut *)
  priv = { double_buffering = true }
}

(* Ouvre le buffer d'affichage *)
let open_buffer width height =
  Graphics.open_graph (Printf.sprintf " %dx%d" width height);
  let w = Graphics.size_x () and h = Graphics.size_y () in
  let buf = create_buffer w h in
  Graphics.auto_synchronize (not buf.priv.double_buffering);
  buf

(* Ferme le buffer *)
let close_buffer buf =
  Graphics.close_graph ()

(* Met à jour les informations du buffer, notamment sa taille *)
let update buf =
  buf.width <- Graphics.size_x ();
  buf.height <- Graphics.size_y ()

(* Fonctions d'affichage sur le buffer : /!\ il ne faut pas les
   appeller directement /!\, mais les utiliser dans la fonction passée
   en argument à draw.
   Par exemple, pour nettoyer le buffer et dessiner un rectangle, on fait :
   draw (fun buf ->
   clear buf;
   draw_rect 10 10 10 10 buf) buffer
*)
let clear buf = Graphics.clear_graph ()
let resize w h buf = Graphics.resize_window w h

let set_color color buf = Graphics.set_color color
let moveto x y buf = Graphics.moveto x y
let rmoveto x y buf = Graphics.rmoveto x y
let draw_rect x y w h buf = Graphics.draw_rect x y w h
let fill_circle x y r buf = Graphics.fill_circle x y r
let lineto x y buf = Graphics.lineto x y
let rlineto x y buf = Graphics.rlineto x y

let set_text_size n buf = Graphics.set_text_size n
let draw_string x y s buf = Graphics.moveto x y; Graphics.draw_string s

(* Fonction prenant en argument une fonction f réalisant des effets
   (primitives d'affichage), et les applique sur le buffer *)
let draw f buf =
  f buf;
  if buf.priv.double_buffering then Graphics.synchronize ();
  update buf
