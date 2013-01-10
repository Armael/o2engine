(* Module de manipulation de vecteurs *)

type t = { x : float; y : float; }
val create : unit -> t

(* Addition de vecteurs *)
val add : t -> t -> t
(* Opposé d'un vecteur *)
val neg : t -> t
(* "Soustraction" de vecteurs : calcule v1 + (-v2) *)
val sub : t -> t -> t
(* Homothétie *)
val scale : float -> t -> t
(* Produit scalaire *)
val sp : t -> t -> float
(* Rotation d'angle a pour un vecteur *)
val rot : float -> t -> t
(* Norme *)
val norm : t -> float
(* Vecteur unitaire ayant la même direction que v *)
val unit : t -> t

(* add *)
val ( ++ ) : t -> t -> t
(* sub *)
val ( -- ) : t -> t -> t
(* scale *)
val ( ** ) : float -> t -> t
(* sp *)
val ( |. ) : t -> t -> float

val print : t -> unit
