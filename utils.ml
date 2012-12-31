let int = int_of_float
let float = float_of_int

let ( >>= ) w f = f w

let fst3 (a, b, c) = a
let snd3 (a, b, c) = b
let trd3 (a, b, c) = c

let rec sleep t =
  if t > 0. then
    let now = Unix.gettimeofday () in
    (try ignore (Unix.select [] [] [] t) with
    | _ -> ());
    sleep (t -. ((Unix.gettimeofday ()) -. now))
