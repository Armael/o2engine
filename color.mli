(* Module permettant d'abstraire la gestion des couleurs. Utilise pour
   son implémentation le module Graphics *)

type t = Graphics.color

val white : t
val black : t
val red : t
val green : t
val blue : t
val yellow : t
val cyan : t
val magenta : t

(* Renvoie une couleur de composantes RGB les trois entiers (entre 0
   et 255) passés en argument *)
val rgb : int -> int -> int -> t
