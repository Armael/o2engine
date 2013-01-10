open Utils
open MainWindow

module C = QuadTreeContainer
module PhysEngine = Phys.Make (C)

module Engine = Engine.Make (PhysEngine) (Screen)

let rec add_random_balls size_min size_max speed_max xm ym n w =
  let adr = add_random_balls size_min size_max speed_max xm ym in
  if n = 0 then w else (
    let r = (Random.float (size_max -. size_min)) +. size_min in
    let x = (Random.float (xm -. (2.*.r))) +. r in
    let y = (Random.float (ym -. (2.*.r))) +. r in
    let vx = (if Random.bool () then 1. else -1.) *. (Random.float speed_max) in
    let vy = (if Random.bool () then 1. else -1.) *. (Random.float speed_max) in
    let open Ball in
    let open Vector in
    let rand_ball = {
      id = 0;
      pos = {x = x; y = y};
      speed = {x = vx; y = vy};
      radius = r;
      mass = 0.001 *. r*.r;
      color = Color.rgb (Random.int 128) (Random.int 128) (Random.int 128)
    } in
    
    let open Engine in
    let open PhysEngine in
    if C.is_colliding rand_ball w.phys.balls then
      adr n w
    else (
      adr (n-1) (Engine.add_ball rand_ball w)
    )
  )

let () =
  let open Screen in
  let open Engine in
  let i = openWindow () in
  
  let world = Engine.new_world 600 600 in
  Random.self_init ();
  let xm = float world.buff.width in
  let ym = float world.buff.height in 
  let open Ball in
  let open Vector in
  match i with
  | 0 ->
    (*let etape = ref false in
    let posmem = ref(0,0) in*)

    let rec ballsWithCoordList l rad w = match l with
      | [] -> w
      | (x,y,i)::ll -> let newBall = Ball.create () in
		       ballsWithCoordList ll rad
			 (add_ball {newBall with pos = {x = x; y = y};
			   radius = rad; id = i;
			   mass = 5.;
			   color = (if i = 0 then Color.blue else Color.red)} w)
    in

    world >>=
      borders_follow_buffer_size true >>=
      ballsWithCoordList [(50., ym/.2., 0); (xm /. 2., ym /. 2., 1);
			  ((xm /. 2.) +. 30., (ym /. 2.) +. 30., 1);
			  ((xm /. 2.) +. 30., (ym /. 2.) -. 30., 1);
			  (xm /. 2. +. 60., ym /. 2., 1);
			  (xm /. 2. +. 60., ym /. 2. +. 60., 1);
			  (xm /. 2. +. 60., ym /. 2. -. 60., 1); 
			  (xm /. 2. +. 90., ym /. 2. -. 30., 1);
			  (xm /. 2. +. 90., ym /. 2. -. 90., 1);
			  (xm /. 2. +. 90., ym /. 2. +. 30., 1);
			  (xm /. 2. +. 90., ym /. 2. +. 90., 1)] 20. >>=
      (* Frottements fluides *)
      add_f (fun b -> 
	let open Ball in
	(-. 0.001 *. (norm b.speed)) ** b.speed) >>=
      (* Frottements solides *)
      add_f (fun b ->
	let open Ball in
	let n = Vector.norm b.speed in
	if n > 0. then 
	  (-. 200. *. (1. /. n)) ** b.speed
	else {Vector.x = 0.; Vector.y = 0.}) >>=
      set_restitution 0.99 >>=
      set_user_action (fun ui_state w ->
	let w = set_postdraw_hook [] w in
	List.fold_left (fun w (t, _) -> match t with
	| Ui.Slide (v1, v2) -> modify_i 0
	  (fun b -> {b with speed = Vector.scale (2.) (Vector.sub v2 v1)}) w
	| Ui.Sliding (v1, v2) -> set_postdraw_hook
	  [0, (fun buf -> Screen.moveto (int v1.x) (int v1.y) buf;
	    Screen.lineto (int v2.x) (int v2.y) buf)] w
	| _ -> w) w ui_state) >>=
      run 60
      
  | 1 -> world >>=
    borders_follow_buffer_size true >>=
    add_f (fun b -> b.mass ** {x = 0.; y = -1000.}) >>=
    add_random_balls 5. 35. 400. xm ym 70 >>= 
    set_restitution 0.9 >>=
    set_user_action (fun status w ->
      let open Ui in
      List.iter (fun (t, _) -> (match t with 
      | Keypress c -> Printf.printf "Keypress : %c" c; print_endline ""
      | Button_up -> Printf.printf "Button up"; print_endline ""
      | Slide(v1,v2)-> Printf.printf "Slide : (%f,%f)->(%f,%f)" v1.x v1.y v2.x v2.y; print_endline ""
      | Sliding(v1, v2) -> Printf.printf "Sliding : (%f,%f) -> (%f,%f)\n%!" v1.x v1.y v2.x v2.y
      | Button_down -> Printf.printf "Button down"; print_endline "");
      ) status;
      w) >>=
    run 60

  | 2 -> world >>=
    borders_follow_buffer_size true >>=
    add_random_balls 2. 2. 500. xm ym 2000 >>= 
    set_restitution 1. >>=
    run 60

  | _ -> world >>=
    borders_follow_buffer_size true >>=
    add_random_balls 10. 10. 300. xm ym 300 >>= 
    set_restitution 1. >>=
    run 60
