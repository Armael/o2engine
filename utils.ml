let int = int_of_float
let float = float_of_int

(* Cet opérateur permet de chainer aisément des fonctions
   « modifiant » de manière fonctionnelle (en retournant une copie
   modifiée) un élément. Par exemple :

   1 >>=
   (( + ) 1) >>=
   (( * ) 2) >>=
   print_int
   
   incrémente 1 de 1, le multiplie par 2 et l'affiche
*)
let ( >>= ) w f = f w

let fst3 (a, b, c) = a
let snd3 (a, b, c) = b
let trd3 (a, b, c) = c

(* Dort pendant un temps t flottant (c'est plus précis que Unix.sleep
   qui attend au minimum une seconde *)
let rec sleep t =
  if t > 0. then
    let now = Unix.gettimeofday () in
    (try ignore (Unix.select [] [] [] t) with
    | _ -> ());
    sleep (t -. ((Unix.gettimeofday ()) -. now))

(* Exécute la fonction f : unit -> 'a, attend le temps t, *en prenant
   en compte le temps d'exécution de la fonction*, et renvoie la
   valeur de retour de f *)
let run_wait f t =
  let start = Unix.gettimeofday () in
  let ret = f () in
  sleep (t -. (Unix.gettimeofday ()) +. start);
  ret
