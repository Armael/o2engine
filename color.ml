(* Module permettant d'abstraire la gestion des couleurs. Utilise pour
   son implémentation le module Graphics *)

type t = Graphics.color

let white = Graphics.white
let black = Graphics.black
let red = Graphics.red
let green = Graphics.green
let blue = Graphics.blue
let yellow = Graphics.yellow
let cyan = Graphics.cyan
let magenta = Graphics.magenta

(* Renvoie une couleur de composantes RGB les trois entiers (entre 0
   et 255) passés en argument *)
let rgb = Graphics.rgb
