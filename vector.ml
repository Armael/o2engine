type t = { x : float; y : float }

let pi = 3.1415926535897932

let create () = { x = 0.; y = 0. }

(* Addition de vecteurs *)
let add v1 v2 = { x = v1.x +. v2.x; y = v1.y +. v2.y }
(* "Soustraction" de vecteurs :calcule p2 + (-p1) *)
let neg v = { x = -. v.x ; y = -. v.y }
(* Homothétie *)
let scale k p = { x = k *. p.x; y = k *. p.y }
(* Produit scalaire *)
let sp v1 v2 = v1.x *. v2.x +. v1.y *. v2.y
(* Rotation d'angle a pour un vecteur *)
let rot a v = { x = v.x *. (cos a) -. v.y *. (sin a); y = v.x *. (sin a) +. v.y *. (cos a) }
(* Norme *)
let norm v = sqrt (sp v v)

let ( ++ ) = add
let ( ** ) = scale
let ( |. ) = sp

let print v = Printf.printf "(%f,%f)" v.x v.y
