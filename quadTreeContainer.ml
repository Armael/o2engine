module O = Ball
open Utils

 (* Rappel: On considère ici (comme le module Graphics) que le point de
    coordonnées (0, 0) est en bas à gauche. Cela ne change rien aux
    algorithmes décrits ici, mais permet d'avoir des notations (top
    left, …) correspondant à ce qui apparait effectivement à l'écran
 *)

 (* Les quatre sous-arbres attachés par un Node sont respectivement les
    sous arbres : bottom left, bottom right, top left, top right

    Let deux Vectors attachés à chaque arbre dans le type t décrivent
    la surface couvert par l'arbre, et correspondent respectivement aux
    coordonnées du coin en bas à gauche et du coin en haut à droite
 *)


type tree = Void | Leaf of O.t list | Node of O.t list * t * t * t * t 
and t = Vector.t * Vector.t * tree
  let open Vector in

let is_in_tree t ball =
  (* Retourne true si ball est dans l'arbre t *)
  match t with 
    (v1, v2, _) -> Rect.is_in_rect v1 v2 ball

let rec print = 
  (* Écrit la structure d'arbre sur la sortie standard *)
  function
  | (_, _, Void) -> Printf.printf "Void"
  | (_, _, Leaf l) -> Printf.printf "Leaf (";
    List.iter (fun b -> Ball.print b; Printf.printf ";") l;
    Printf.printf ")"
  | (_, _, Node (l, a, b, c, d)) -> Printf.printf "Node (";
    List.iter (fun b -> Ball.print b; Printf.printf ";") l;
    print a; Printf.printf "; ";
    print b; Printf.printf "; ";
    print c; Printf.printf "; ";
    print d; Printf.printf ")"

let rec display (v1, v2, t) =
  (* Dessine le quadTree et les balles contenues (attention, un buffer
     Graphics doit être ouvert) *)
  (* Fonction de Debug - nécessite le module Graphics (casse donc
     l'abstraction du système de sortie graphique) *)
  let open Vector in
  let open Ball in
  Graphics.draw_rect (int v1.x) (int v1.y) (int (v2.x -. v1.x)) (int (v2.y -. v1.y));
  match t with
  | Node (l, a, b, c, d) ->
    List.iter (fun b -> Graphics.fill_circle (int b.pos.x) (int b.pos.y) (int b.radius)) l;
    display a;
    display b;
    display c;
    display d
  | Leaf l -> List.iter (fun b -> Graphics.fill_circle (int b.pos.x) (int b.pos.y) (int b.radius)) l
  | _ -> ()

let empty x y  = (x, y, Void) (* Retourne un arbre vide *)
  
let add o depth c =
  (* Ajoute un élément o dans c, de profondeur maximale depth *)
  let open Ball in
  let open Vector in
  let open Rect in
  let rec constr o depth c =
    match c with
    | (vx, vy, Void) ->
      let blr = bl_rect vx vy and brr = br_rect vx vy and
	  tlr = tl_rect vx vy and trr = tr_rect vx vy in
      if (is_in_rect vx vy o) then
	(* Si o est dans le sous-arbre c *)
	if (is_in_multiple_sub_rect vx vy o) then
	  (vx, vy, Node ([o], (fst blr, snd blr, Void),
			 (fst brr, snd brr, Void),
			 (fst tlr, snd tlr, Void),
			 (fst trr, snd trr, Void)))
	(* Si o est dans plusieurs sous-arbres on crée un noeud avec
	   des sous-arbres vides et on l'ajoute au noeud *)
	else if (depth = 0) then (vx, vy, Leaf ([o]))
	(* Si o est à la profondeur max on crée une feuille et on
	   l'ajoute à la feuille *)
	else constr o depth (vx, vy,
			     Node ([],
				   (fst blr, snd blr, Void),
				   (fst brr, snd brr, Void),
				   (fst tlr, snd tlr, Void),
				   (fst trr, snd trr, Void))
	)
	(* Si o est dans un unique sous-arbre on crée un noeud et on
	   le rajoute aux sous-arbres *)
	else c;	(* On retourne l'arbre courant si l'objet ne peut pas être ajouté *)

    | (vx, vy, Leaf (l)) ->
      if (is_in_rect vx vy o) then
	(vx, vy, Leaf (o :: l))	(* On ajoute l'objet à la feuille (il est déja à la profondeur max) *)
      else
	c	(* Retourne l'arbre courant si l'objet ne peut pas être ajouté *)
    | (vx, vy, Node (lo, bl, br, tl, tr)) ->
      if (is_in_rect vx vy o) then
	(* o est dans le sous-arbre *)
	if (is_in_multiple_sub_rect vx vy o) then
	  (* Si o est dans plusieurs sous-arbres on l'ajoute au nœud *)
	  (vx, vy, Node (o :: lo, bl, br, tl, tr))
	else
	  (* Sinon on le rajoute aux sous-arbres *)
	  (vx, vy, Node (lo,
			 constr o (depth - 1) bl,
			 constr o (depth - 1) br,
			 constr o (depth - 1) tl,
			 constr o (depth - 1) tr
	   ))
      else c (* Retourne l'arbre courant si l'objet ne peut pas être ajouté *)
  in constr o depth c

let remove o c =
  (* Retire un élément o dans un arbre c *)
  let open Ball in
  let open Vector in
  (* Retire un élément o d'une liste l *)
  let delete_list l o = List.filter ((<>) o) l in
  let rec aux o c =
    match c with
    | (x, y, Void) -> (x, y, Void)
    | (x, y, Leaf (l)) ->
      (* if is_in_rect x y o then (-- à rajouter peut-être--) *)
      let nl = (delete_list l o) in (* On retire si possible l'élément de la feuille *)
      if nl = [] then (x, y, Void) (* Si il n'y a plus d'éléments dans la feuille, on retourne Void *)
      else (x, y, Leaf (nl))
    (* else c (--à rajouter peut-être--) *)

    | (x, y, Node (lo, bl, br, tl, tr)) ->
      if Rect.is_in_rect x y o then
	begin
	  let (x1, y1, qt1) = aux o bl in
	  let (x2, y2, qt2) = aux o br in
	  let (x3, y3, qt3) = aux o tl in
	  let (x4, y4, qt4) = aux o tr in
	  let newl = delete_list lo o in (* On essaye de retirer l'élément sur le noeud et dans les sous arbres *)
	  if (qt4 = Void && qt3 = Void && qt2 = Void && qt1 = Void && newl = [])
	  then (x, y, Void)
	  (* Si il n'y a plus d'éléments dans l'arbre on retourne Void *)
	  else (x,y, Node (newl, (x1, y1, qt1), (x2, y2, qt2), (x3, y3, qt3), (x4, y4, qt4)))
	(* Sinon, retourner l'arbre sans o *)
	end
      else c (* Retourne c si o ne peut pas être dans le sous-arbre *)
  in
  aux o c

let rec iter f = function
  (* itère f dans l'arbre *)
  | (x,y,Void) -> ()
  | (x,y,Leaf (b)) -> List.iter f b
  | (x,y,Node (lo, bl, br, tl, tr)) -> 
    iter f bl;
    iter f br;
    iter f tl;
    iter f tr
    
let map f depth (c : t) =
  (* retourne l'arbre dont les élément o de c sont maintenant
    (f o) *)
  (* applique la fonction f a tous les elements de la liste et les
     rajoute dans le nouvel arbre *)
  let add_f_all lst ntl = List.fold_left 
    (fun acc x -> add (f x) depth acc) ntl lst in
  let rec aux t nt =
    (* applique add_f_all a chaque element du noeud ou de la feuille
       est reitère avec les sous arbres *)
    match t with
    | (x, y, Void) -> nt
    | (x, y, Leaf (b)) ->
      (add_f_all b nt)
    | (x, y, Node (lo, bl, br, tl, tr)) ->
      let nbl = (aux bl nt) in
      let nbr = (aux br nbl) in
      let ntl = (aux tl nbr) in
      let ntr = (aux tr ntl) in
      add_f_all lo ntr
  in
  aux c (empty (fst3 c) (snd3 c))

exception Collides
let is_colliding b cont =
  try iter
	(fun other_ball -> if O.is_colliding b other_ball then raise Collides)
	cont;
      false with
      | Collides -> true

