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


type cont = Void | Leaf of O.t list | Node of O.t list * t * t * t * t 
and t = Vector.t * Vector.t * cont

let log2 x = (log x) /. (log 2.)

let get_depth v1 v2 =
  (* Retourne la profondeur adéquate pour avoir des feuilles d'environ
     size px de côté *)
  let open Vector in
  let size = 30. in
  int_of_float (max (log2 ((v2.x -. v1.x) /. size)) (log2 ((v2.y -. v1.y) /. size)))

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
    print d; Printf.printf ")";
    print_endline ""

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

let add o c =
  let depth = get_depth (fst3 c) (snd3 c) in
  (* Ajoute un élément o dans c, de profondeur maximale depth *)
  let open Ball in
  let open Vector in
  let open Rect in
  let rec constr o depth c k =
    match c with
    | (vx, vy, Void) ->
      let blr = bl_rect vx vy and brr = br_rect vx vy and
	  tlr = tl_rect vx vy and trr = tr_rect vx vy in
      if (is_in_rect_partial vx vy o) then
	(* Si o est dans le sous-arbre c *)
	if (is_in_multiple_sub_rect vx vy o) then
	  k (vx, vy, Node ([o], (fst blr, snd blr, Void),
			   (fst brr, snd brr, Void),
			   (fst tlr, snd tlr, Void),
			   (fst trr, snd trr, Void)))
	(* Si o est dans plusieurs sous-arbres on crée un noeud avec
	   des sous-arbres vides et on l'ajoute au noeud *)
	else if (depth <= 0) then k (vx, vy, Leaf ([o]))
	(* Si o est à la profondeur max on crée une feuille et on
	   l'ajoute à la feuille *)
	else constr o depth (vx, vy,
			     Node ([],
				   (fst blr, snd blr, Void),
				   (fst brr, snd brr, Void),
				   (fst tlr, snd tlr, Void),
				   (fst trr, snd trr, Void))) k
	(* Si o est dans un unique sous-arbre on crée un noeud et on
	   le rajoute aux sous-arbres *)
	else k c;	(* On retourne l'arbre courant si l'objet ne peut pas être ajouté *)

    | (vx, vy, Leaf (l)) ->
      if (is_in_rect_partial vx vy o) then
	k (vx, vy, Leaf (o :: l))	(* On ajoute l'objet à la feuille (il est déja à la profondeur max) *)
      else
	k c	(* Retourne l'arbre courant si l'objet ne peut pas être ajouté *)
    | (vx, vy, Node (lo, bl, br, tl, tr)) ->
      if (is_in_rect_partial vx vy o) then
	(* o est dans le sous-arbre *)
	if (is_in_multiple_sub_rect vx vy o) then
	  (* Si o est dans plusieurs sous-arbres on l'ajoute au nœud *)
	  k (vx, vy, Node (o :: lo, bl, br, tl, tr))
	else 
	  (* Sinon on le rajoute aux sous-arbres *)
	  (constr o (depth - 1) bl (fun v1 ->
	    constr o (depth - 1) br (fun v2 ->
	      constr o (depth - 1) tl (fun v3 ->
		constr o (depth - 1) tr (fun v4 ->
		  k (vx, vy, Node (lo, v1, v2, v3, v4)))))))
      else k c (* Retourne l'arbre courant si l'objet ne peut pas être ajouté *)
  in constr o depth c (fun x -> x)

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
  | (x,y,Node (lo, bl, br, tl, tr)) -> List.iter f lo;
    iter f bl;
    iter f br;
    iter f tl;
    iter f tr

let map f (c : t) =
  (* retourne l'arbre dont les élément o de c sont maintenant (f o) *)
  (* applique la fonction f a tous les elements de la liste et les
     rajoute dans le nouvel arbre *)
  let add_f_all lst ntl = List.fold_left 
    (fun acc x -> add (f x) acc) ntl lst in
  let rec aux t nt =
    (* applique add_f_all a chaque element du noeud ou de la feuille
       est reitère avec les sous arbres *)
    match t with
    | (_, _, Void) -> nt
    | (_, _, Leaf (b)) -> add_f_all b nt
    | (x, y, Node (lo, bl, br, tl, tr)) ->
      let nbl = (aux bl nt) in
      let nbr = (aux br nbl) in
      let ntl = (aux tl nbr) in
      let ntr = (aux tr ntl) in
      add_f_all lo ntr
  in
  aux c (empty (fst3 c) (snd3 c))

let modify b new_b c =
  c >>=
    remove b >>=
    add new_b

let modify_i i f c =
  map (fun b -> if b.Ball.id = i then f b else b) c

let resize new_v1 new_v2 c =
  map (fun x -> x) (new_v1, new_v2, trd3 c)

exception Collides
let is_colliding b cont =
  try iter
	(fun other_ball -> if O.is_colliding b other_ball then raise Collides)
	cont;
      false with
      | Collides -> true

(* Zipper sur les quad Trees : définition de l'« emplacement » dans un
   qtree *)

type parent_data = Vector.t * Vector.t * (Ball.t list)

type cxt =
| Top
| BL of cxt * parent_data * t * t * t  (* bottom left branch *)
| BR of cxt * parent_data * t * t * t  (* bottom right branch *)
| TL of cxt * parent_data * t * t * t  (* top left branch *)
| TR of cxt * parent_data * t * t * t  (* top right branch *)

type location = t * cxt

(* Fonctions permettant de se déplacer dans l'arbre, toutes de type
   location -> location *)

let bl (t, c) = match trd3 t with
  | Void | Leaf _ -> failwith "bl"
  | Node (ol, bl, br, tl, tr) -> (bl, BL (c,
					  (fst3 t, snd3 t, ol),
					  br, tl, tr))

let br (t, c) = match trd3 t with
  | Void | Leaf _ -> failwith "br"
  | Node (ol, bl, br, tl, tr) -> (br, BR (c,
					  (fst3 t, snd3 t, ol),
					  bl, tl, tr))

let tl (t, c) = match trd3 t with
  | Void | Leaf _ -> failwith "tl"
  | Node (ol, bl, br, tl, tr) -> (tl, TL (c,
					  (fst3 t, snd3 t, ol),
					  bl, br, tr))

let tr (t, c) = match trd3 t with
  | Void | Leaf _ -> failwith "tr"
  | Node (ol, bl, br, tl, tr) -> (tr, TR (c,
					  (fst3 t, snd3 t, ol),
					  bl, br, tl))

let top t = (t, Top)

let up (t, c) = match c with
  | Top -> failwith "up"
  | BL (c', dat, br, tl, tr) -> ((fst3 dat, snd3 dat, 
				  Node (trd3 dat, t, br, tl, tr)), c')
  | BR (c', dat, bl, tl, tr) -> ((fst3 dat, snd3 dat,
				  Node (trd3 dat, bl, t, tl, tr)), c')
  | TL (c', dat, bl, br, tr) -> ((fst3 dat, snd3 dat,
				  Node (trd3 dat, bl, br, t, tr)), c')
  | TR (c', dat, bl, br, tl) -> ((fst3 dat, snd3 dat,
				  Node (trd3 dat, bl, br, tl, t)), c')

let rec upmost loc = 
  try upmost (up loc) with
    Failure _ -> loc

let find b t =
  let open Rect in
  let is_in_rect (u, v) = is_in_rect u v b in
  let rec aux ((v1, v2, t), c) = 
    let loc = ((v1, v2, t), c) in
    if not (is_in_rect (v1, v2)) then
      failwith "find"
    else match t with 
    | Void | Leaf _ -> loc
    | Node _ ->
      if is_in_multiple_sub_rect v1 v2 b then
	loc
      else if is_in_rect (bl_rect v1 v2) then
	aux (bl loc)
      else if is_in_rect (br_rect v1 v2) then
	aux (br loc)
      else if is_in_rect (tl_rect v1 v2) then
	aux (tl loc)
      else 
	aux (tr loc) in
  aux (top t)

(* Implémentation du parcours en profondeur grâce au zipper *)

(* t -> location *)
let start_loc t = top t

(* location -> location : étape suivante pour le parcours. *)
let rec next loc = 
  try tl loc with
    Failure _ -> go_up loc
and go_up loc = match snd loc with
  | Top -> failwith "next"
  | TL _ -> next_tr (up loc)
  | TR _ -> next_bl (up loc)
  | BL _ -> next_br (up loc)
  | BR _ -> go_up (up loc)
and next_tr loc =
  try tr loc with
    Failure _ -> go_up loc
and next_bl loc =
  try bl loc with
    Failure _ -> go_up loc
and next_br loc =
  try br loc with
    Failure _ -> go_up loc

(* Descend dans la branche BR le plus possible *)
let rec end_br loc =
  try end_br (br loc) with
    Failure _ -> loc

let end_loc t = end_br (top t)

let rec prev loc = match snd loc with
  | Top -> failwith "prev"
  | TL _ -> up loc
  | TR _ -> end_br (tl (up loc))
  | BL _ -> end_br (tr (up loc))
  | BR _ -> end_br (bl (up loc))

let iterate_solve_collisions solver cont =
  (* iterate: location -> t *)
  let rec iterate loc =
    (* solve_collision : O.t -> location -> O.t * location *)
    let rec solve_collision it ((v1, v2, t), c) =
      let loc = ((v1, v2, t), c) in
      match t with
      | Void -> (it, loc)
      | Leaf l -> 
	let solved = trd3 (ListContainer.iterate_solve_collisions solver
			     (v1, v2, (it::l))) in
	(List.hd solved, ((v1, v2, Leaf (List.tl solved)), c))
      | Node (l, bl', br', tl', tr') ->
	let it::solved_l = trd3 (ListContainer.iterate_solve_collisions solver
				   (v1, v2, (it::l))) in
	let new_loc = ((v1, v2, Node (solved_l, bl', br', tl', tr')), c) in
	let (it, solved_bl) = solve_collision it (bl new_loc) in
	let (it, solved_br) = solve_collision it (br (up solved_bl)) in
	let (it, solved_tl) = solve_collision it (tl (up solved_br)) in
	let (it, solved_tr) = solve_collision it (tr (up solved_tl)) in
	(it, up solved_tr)
    in

    let ((v1, v2, t), c) = loc in
    let new_loc =
      (match t with
      | Void -> loc
      | Leaf balls ->
	((v1, v2, 
	  Leaf (trd3 (ListContainer.iterate_solve_collisions solver
			(v1, v2, balls)))), c)
      | Node (l, bl', br', tl', tr') ->
	let l = (trd3 (ListContainer.iterate_solve_collisions solver
			 (v1, v2, l))) in
	let new_loc = ((v1, v2, Node (l, bl', br', tl', tr')), c) in
	let (new_loc, new_l) = List.fold_left (fun (new_loc, new_l) it ->
	  let (new_it, n_loc) = solve_collision it (bl new_loc) in
	  let (new_it, n_loc) = solve_collision new_it (br (up n_loc)) in
	  let (new_it, n_loc) = solve_collision new_it (tl (up n_loc)) in
	  let (new_it, n_loc) = solve_collision new_it (tr (up n_loc)) in
	  (up n_loc, new_it::new_l)) (new_loc, []) l in
	let ((_, _, Node (_, bl, br, tl, tr)), c) = new_loc in
	let loc = ((v1, v2, Node (new_l, bl, br, tl, tr)), c) in
	loc) in

    try iterate (next new_loc) with
    | Failure _ -> fst (upmost new_loc)
  in
  iterate (start_loc cont)

