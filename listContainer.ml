module O = Ball

type t = O.t list
let empty () = []
let add o c = o::c
let iter f (c:t) = List.iter f c
let map f (c:t) = List.map f c
let fold f acc (c:t) = List.fold_left f acc c

(* Pareil qu'avec un fold avec Ball.is_colliding, mais retourne (en
   utilisant en interne une exception) dès qu'une collision a été
   trouvée. Légèrement optimisant donc. *)
exception Collides
let is_colliding b cont = 
  try iter 
	(fun other_ball -> if O.is_colliding b other_ball then raise Collides)
	cont;
      false with
  | Collides -> true

let collides_with b cont =
  fold
    (fun acc other_b -> 
      if b <> other_b && O.is_colliding b other_b then other_b::acc else acc)
    []
    cont
