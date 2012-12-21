open Utils

module Engine = Phys.Make (ListContainer)

let rec add_balls xm ym n w =
  if n = 0 then w else (
    let r = (Random.float 35.) +. 5. in
    let x = (Random.float xm -. (2.*.r)) +. r in
    let y = (Random.float ym -. (2.*.r)) +. r in
    let vx = (if Random.bool () then 1. else -1.) *. (Random.float 500.) in
    let vy = (if Random.bool () then 1. else -1.) *. (Random.float 500.) in
    let open Ball in
    let open Vector in
    add_balls xm ym (n-1) 
      (Engine.add_ball {
	id = 0;
	pos = {x = x; y = y};
	speed = {x = vx; y = vy};
	radius = r;
	mass = 5.
      } w)
  )

let () =
  Graphics.open_graph " ";
  Graphics.auto_synchronize false;
  Random.self_init ();
  let xm = float (Graphics.size_x ()) in
  let ym = float (Graphics.size_y ()) in

  let open Engine in
  new_world () >>=
    add_border (Left 0.) >>=
    add_border (Right xm) >>=
    add_border (Bottom 0.) >>=
    add_border (Top ym) >>=
    add_balls xm ym 20 >>=
    loop (1./.60.)
    
    
    
