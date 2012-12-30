open Utils

module PhysEngine = Phys.Make (ListContainer)
module Engine = Engine.Make (PhysEngine) (Screen)

let rec add_random_balls xm ym n w =
  if n = 0 then w else (
    let r = (Random.float 35.) +. 5. in
    let x = (Random.float (xm -. (2.*.r))) +. r in
    let y = (Random.float (ym -. (2.*.r))) +. r in
    let vx = (if Random.bool () then 1. else -1.) *. (Random.float 500.) in
    let vy = (if Random.bool () then 1. else -1.) *. (Random.float 500.) in
    let open Ball in
    let open Vector in
    let rand_ball = {
      id = 0;
      pos = {x = x; y = y};
      speed = {x = vx; y = vy};
      radius = r;
      mass = 0.001 *. r*.r;
      color = Color.black
    } in
    
    let open Engine in
    let open PhysEngine in
    if ListContainer.is_colliding rand_ball w.phys.balls then
      add_random_balls xm ym n w
    else (
      add_random_balls xm ym (n-1) 
	(Engine.add_ball rand_ball w)
    )
  )

let () =
  let open Screen in
  let open Engine in
  let world = Engine.new_world () in
  Random.self_init ();
  let xm = float world.buff.width in
  let ym = float world.buff.height in


  let open Ball in
  let open Vector in

  world >>=
    borders_follow_buffer_size true >>=
    add_f (fun b -> b.mass ** {x = 0.; y = -1000.}) >>=
    add_random_balls xm ym 70 >>= 
    set_restitution 0.8 >>=
    run 60
