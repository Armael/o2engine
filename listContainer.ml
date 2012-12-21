module O = Ball

type t = O.t list
let empty () = []
let add o c = o::c
let iter f (c:t) = List.iter f c
let map f (c:t) = List.map f c
let fold f acc (c:t) = List.fold_left f acc c
