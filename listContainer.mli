(* Conteneur list : il s'agit d'une liste ainsi qu'un rectangle
   définissant la zone qu'elle «contient" *)
type cont = Ball.t list
type t = Vector.t * Vector.t * cont

(* Retourne un conteneur vide de taille (v1, v2) *)
val empty : Vector.t -> Vector.t -> t
(* Ajoute une balle au conteneur *)
val add : Ball.t -> t -> t
(* Itère f sur le conteneur *)
val iter : (Ball.t -> unit) -> t -> unit
(* Applique f à chaque élément du conteneur *)
val map : (Ball.t -> Ball.t) -> t -> t

(* Redimensionne le rectangle du conteneur et supprime les balles en
   dehors *)
val resize : Vector.t -> Vector.t -> t -> t

(* Pareil qu'avec un fold avec Ball.is_colliding, mais retourne (en
   utilisant en interne une exception) dès qu'une collision a été
   trouvée. Légèrement optimisant donc. *)
val is_colliding : Ball.t -> t -> bool

(* Itère sur la structure de données, et résout les collisions des
   balles deux à deux : la fonction solver passée en argument retourne
   deux balles modifiées après une collision *)
val iterate_solve_collisions : (Ball.t -> Ball.t -> Ball.t * Ball.t) -> t -> t
