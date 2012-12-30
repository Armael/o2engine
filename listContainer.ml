module O = Ball

type t = O.t list
let empty () = []
let add o c = o::c
let iter f (c:t) = List.iter f c
let map f (c:t) = List.map f c

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

type 'a loc = 'a list * 'a * 'a list

let start_loc l = ([], List.hd l, List.tl l)
let next (prev, it, next) = (it::prev, List.hd next, List.tl next)
let prev (prev, it, next) = (List.tl prev, List.hd prev, it::next)
let end_loc l = let r = List.rev l in
	    (List.tl r, List.hd r, [])

let iterate_solve_collisions solver cont =
  let rec iterate (p, it, n) =
    let loc = (p, it, n) in
    let rec solve_collision it (p, obj, n) =
      if O.is_colliding it obj then (
	let (new_it, new_obj) = solver it obj in
	if n = [] then (new_it, (p, new_obj, n)) else
	  let (u, v) = solve_collision new_it (next (p, new_obj, n)) in
	  (u, prev v)
      ) else (
	if n = [] then (it, (p, obj, n)) else 
	  let (u, v) = solve_collision it (next (p, obj, n)) in
	  (u, prev v)
      ) in
    if n = [] then List.rev (it::p) else
      let (new_it, loc') = solve_collision it (next loc) in
      let (p, u, n) = prev loc' in
      iterate (next (p, new_it, n)) in

  iterate (start_loc cont)
