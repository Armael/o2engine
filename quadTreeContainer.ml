module O = Ball

type tree = Void | Leaf of O.t list | Node of O.t list * t * t * t * t
and
(*haut gauche - bas droite*)
 t = vect * vect * tree

let is_in_rect v1 v2 ball = 
(*retourne vrai si le centre de ball est dans le rectangle definie par v1 v2*) 
  let open Ball in
  let open Vector in
  (ball.pos.x >= v1.x && ball.pos.x <= v2.x && ball.pos.y >= v1.y && ball.pos.y <= v2.y) 

let is_in_rect_partial v1 v2 ball =
(*retourne vrai si ball est en partie dans le rectangle definie par v1 v2*) 
  let open Ball in
  let open Vector in
			(((ball.pos.x +. ball.rad >= v1.x) && (ball.pos.x +. ball.rad) <= v2.x) || ((ball.pos.x -. ball.rad >= v1.x) && (ball.pos.x -. ball.rad) <= v2.x))
			&& (((ball.pos.y +. ball.rad >= v1.y) && (ball.pos.y +. ball.rad) <= v2.y) || ((ball.pos.y -. ball.rad >= v1.y) && (ball.pos.y -. ball.rad) <= v2.y))

let is_in_multiple_sub_rect vx vy ball =	
(*retourne vrai si ball est dans plusieur sous rectangle*) 
  let open Ball in
  let open Vector in		
    let br1 = is_in_rect_partial vx {x = (vy.x +. vx.x) /. 2.; y = (vy.y +. vx.y) /. 2.} ball in
    let br2 = is_in_rect_partial {vx with x = (vy.x +. vx.x) /. 2.} {vy with y = (vy.y +. vx.y) /. 2.} ball in
    let br3 = is_in_rect_partial {vx with y = (vy.y +. vx.y) /. 2.} {vy with x = (vy.x +. vx.x) /. 2.} ball in
    let br4 = is_in_rect_partial {x = (vy.x +. vx.x) /. 2.; y = (vy.y +. vx.y) /. 2.} vy ball in
								(br1 && br2) || (br1 && br3) || (br1 && br4) || (br2 && br3) || (br2 && br4) || (br3 && br4)

let is_in_tree t ball =
(*retourne vrai si ball est dans l'arbre t*) 
  match t with 
    (v1, v2, _) -> is_in_rect v1 v2 ball

let rec print = 
(*affiche l'arbre*)
  function
  | (_, _, Void) -> Printf.printf "Void"
  | (_, _, Leaf l) -> Printf.printf "Leaf (";
    List.iter (fun b -> Ball.print b; Printf.printf ";") l;
    Printf.printf ")"
  | (_, _, Node (a, b, c, d)) -> Printf.printf "Node (";
    print a; Printf.printf "; ";
    print b; Printf.printf "; ";
    print c; Printf.printf "; ";
    print d; Printf.printf ")"

let empty x y  = (x,y,Void) (*arbre vide*)
  
let add o depth c =
(*ajoute un element o dans c*)
  let open Ball in
  let open Vector in
				let rec constr o depth c =
					match c with
						| (vx, vy, Void) ->
								if (is_in_rect vx vy o) then
									(*o est dans le sous arbre*) 
									if (is_in_multiple_sub_rect vx vy o) then
									(vx, vy, Node ([o], (vx, {x = (vy.x +. vx.x) /. 2.; y = (vy.y +. vx.y) /. 2.}, Void),
												({vx with x = (vy.x +. vx.x) /. 2.}, {vy with y = (vy.y +. vx.y) /. 2.}, Void),
												({vx with y = (vy.y +. vx.y) /. 2.}, {vy with x = (vy.x +. vx.x) /. 2.}, Void),
												({x = (vy.x +. vx.x) /. 2.; y = (vy.y +. vx.y) /. 2.}, vy, Void)))
												(*si o est dans plusieur sous-arbre on cree un noeud avec des sous arbre vide et on l'ajoute au noeud*)
									else if (depth = 0) then (vx, vy, Leaf ([o]))
												(*si o est a la profondeur max on crŽe un feuille et on l'ajoute a la feuille*)
									else constr o depth (vx, vy,
												Node ([],
														(vx, {x = (vy.x +. vx.x) /. 2.; y = (vy.y +. vx.y) /. 2.}, Void),
														({vx with x = (vy.x +. vx.x) /. 2.}, {vy with y = (vy.y +. vx.y) /. 2.}, Void),
														({vx with y = (vy.y +. vx.y) /. 2.}, {vy with x = (vy.x +. vx.x) /. 2.}, Void),
														({x = (vy.x +. vx.x) /. 2.; y = (vy.y +. vx.y) /. 2.}, vy, Void))
											)
											(*si o est dans un unique sous arbre on crŽe un noeud et on le rajoute aux sous arbres*)
								else c;	(*retourne le l'arbre courant si l'objet ne peut pas tre ajoutŽ*)

						| (vx, vy, Leaf (l)) ->
								if (is_in_rect vx vy o) then
								(vx, vy, Leaf (o :: l))	(*ajoute l'objet a la feuille (il est deja a la profondeur max)*)
								else
									c	(*retourne le l'arbre courant si l'objet ne peut pas tre ajoutŽ*)
						| (vx, vy, Node (lo, hl, hr, ll, lr)) ->
							if (is_in_rect vx vy o) then
										(*o est dans le sous arbre*) 
								if (is_in_multiple_sub_rect vx vy o) then												
								(*si o est dans plusieur sous-arbre on l'ajoute au noeud*)
								(vx, vy, Node (o :: lo, hl, hr, ll, lr))
								else
								(*sinon on le rajoute aux sous-arbre*)
								(vx, vy, Node (lo,
											constr o (depth - 1) hl,
												constr o (depth - 1) hr,
													constr o (depth - 1) ll,
														constr o (depth - 1) lr
										))
							else c (*retourne le l'arbre courant si l'objet ne peut pas tre ajoutŽ*)
				in constr o depth c

let remove o c =
(*retire un element o dans un arbre c*)
  let open Ball in
  let open Vector in
		let rec delete_list l o =
		(*retire un element o une liste l*)
			match l with
				| [] -> []
				| x :: ll when x = o -> delete_list ll o
				| x :: ll -> x :: (delete_list ll o)
		in
			let rec aux o c =
				match c with
					| (x, y, Void) -> (x, y, Void)
					| (x, y, Leaf (l)) ->
							(*if is_in_rect x y o then (--a rajouter peut tre--) *)
								let nl = (delete_list l o) in		(*retire si possible l'ŽlŽment de la feuille*)
								if nl = [] then (x, y, Void) (*si il n'y a plus d'element dans la feuille retourne le vide*)
								else (x, y, Leaf (nl))
       (*else c(--a rajouter peut tre--) *)

					| (x, y, Node (lo, hl, hr, ll, lr)) ->
							if is_in_rect x y o then
							begin
								let (x1, y1, qt1) = aux o hl in
									let (x2, y2, qt2) = aux o hr in
										let (x3, y3, qt3) = aux o ll in
											let (x4, y4, qt4) = aux o lr in
												let newl = delete_list lo o in(*essaye de retirer l'ŽlŽment sur le noeud et dans les sous arbres*)
													if (qt4 = Void && qt3 = Void && qt2 = Void && qt1 = Void && newl = [])
													then (x, y, Void)  
													(*si il n'y a plus d'ŽlŽment dans l'arbre retourner le vide*)
													else (x,y,Node (newl, (x1, y1, qt1), (x2, y2, qt2), (x3, y3, qt3), (x4, y4, qt4)))
													(*sinon retourner l'arbre sans o*)
							end
							else c		(*retourne c si o ne peut pas etre dans le sous arbre*)
			in
				aux o c

let rec iter f = function
(*iter f dans l'arbre*)
  | (x,y,Void) -> ()
  | (x,y,Leaf (b)) -> List.iter f b
  | (x,y,Node (lo,hl, hr, ll, lr)) -> iter f hl;
    iter f hr;
    iter f ll;
    iter f lr
    
let map f depth (c : t) =
	(*retourne l'arbre dont les ŽlŽment o de c sont maintenant (f o)*)
 let fst3 (a,b,c)=a in
 let snd3 (a,b,c)=b in
 	(*applique la fonction f a tous les elements de la liste et les rajoute dans le nouvel arbre*)
	let rec aux2 l ntl =
		match l with
			| [] -> ntl
			| a :: ll -> aux2 ll (add (f a) depth ntl)
	in
		let rec aux t nt =
			(*applique aux2 a chaque element du noeud ou de la feuille est reitre avec les sous arbres*)
			match t with
				| (x, y, Void) -> nt
				| (x, y, Leaf (b)) ->
						(aux2 b nt)
				| (x, y, Node (lo, hl, hr, ll, lr)) ->
						let nhl = (aux hl nt) in
							let nhr = (aux hr nhl) in
								let nll = (aux ll nhr) in
									let nlr = (aux lr nll) in
										aux2 lo nlr
		in
			aux c (empty (fst3 c) (snd3 c))

exception Collides
let is_colliding b cont =
  try iter
       (fun other_ball -> if O.is_colliding b other_ball then raise Collides)
       cont;
      false with
  | Collides -> true

