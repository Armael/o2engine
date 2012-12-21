let int = int_of_float
let float = float_of_int

let rec sleep t =
  if t > 0. then
    let now = Unix.gettimeofday () in
    (try ignore (Unix.select [] [] [] t) with
    | _ -> ());
    sleep (t -. ((Unix.gettimeofday ()) -. now))
