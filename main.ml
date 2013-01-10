open Utils
open MainWindow

module C = ListContainer
module PhysEngine = Phys.Make (C)

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
      color = Color.rgb (Random.int 128) (Random.int 128) (Random.int 128)
    } in
    
    let open Engine in
    let open PhysEngine in
    if C.is_colliding rand_ball w.phys.balls then
      add_random_balls xm ym n w
    else (
      add_random_balls xm ym (n-1) 
	(Engine.add_ball rand_ball w)
    )
  )

let () =
  let open Screen in
  let open Engine in
  let i = openWindow() in
  
  let world = Engine.new_world () in
  Random.self_init ();
  let xm = float world.buff.width in
  let ym = float world.buff.height in 
  let open Ball in
  let open Vector in
  match i with
  | 0 ->
    (*let etape = ref false in
    let posmem = ref(0,0) in*)

    let rec ballsWithCoordList l rad w=
      match l with
      | [] -> w
      | (x,y,i)::ll when i = 0 -> let newBall = Ball.create() in
				  ballsWithCoordList ll rad
				    (add_ball {newBall with pos = {x = x;y = y};
				      radius = rad;id = i;
				      mass =5.;color = Color.blue} w)
      | (x,y,i)::ll -> let newBall = Ball.create() in
		       ballsWithCoordList ll rad
			 (add_ball {newBall with pos = {x = x;y = y};
			   radius = rad;
			   id = i;
			   mass =5.;
			   color = Color.red} w)
    in
    world >>=
      (*set_world_keypress_handler (fun c -> ()) >>=
      set_world_button_handler (fun b -> b) >>=
      set_world_pos_handler (fun (i,j) -> (i,j)) >>=
      set_predraw_hook 
      [
   	(0, (fun (a,b,p) (buf:Screen.buffer) (w : (unit, bool, (int*int)) Engine.world) -> 
   	  match a, b, p, !etape with
   	  | Some (), _, _, _ -> w
   	  | _, Some true, Some(i, j), false ->
   	    etape := true; 
   	    posmem := (i,j);
   	    w;
   	  | _, Some true, Some(i,j), true ->    	  						
   	    user_map
   	      (fun b ->
   		match b.id with
   		| 0 -> {b with speed = {x = float_of_int (fst (!posmem) - i);
				       y = float_of_int (snd (!posmem) - j)}}
   		| _ -> b) w
   	  | _, _, _, _ -> w))
      ] >>=*)
      borders_follow_buffer_size true >>=
      ballsWithCoordList [(50.,ym/.2.,0);(xm/.2. ,ym/.2.,1);((xm/.2.) +. 30.,(ym/.2.) +. 30.,1);
			  ((xm/.2.) +. 30.,(ym/.2.) -. 30.,1);(xm/.2. +. 60. ,ym/.2.,1);(xm/.2. +. 60. ,ym/.2.+. 60.,1);
			  (xm/.2. +. 60. ,ym/.2.-. 60.,1);(xm/.2. +. 90. ,ym/.2.-. 30.,1);(xm/.2. +. 90. ,ym/.2.-. 90.,1);
			  (xm/.2. +. 90. ,ym/.2.+. 30.,1);(xm/.2. +. 90. ,ym/.2.+. 90.,1)] 20. >>= 
      set_restitution 0.8 >>=
      run 60
      
  | 1 -> world >>=
    borders_follow_buffer_size true >>=
    add_f (fun b -> b.mass ** {x = 0.; y = -1000.}) >>=
    add_random_balls xm ym 70 >>= 
    set_restitution 0.8 >>=
    set_user_action (fun status w ->
      let open Ui in
      List.iter (fun (t, _) -> (match t with 
      | Keypress c -> Printf.printf "Keypress : %c" c; print_endline ""
      | Button_up -> Printf.printf "Button up"; print_endline ""
      | Slide(v1,v2)-> Printf.printf "Slide : (%f,%f)->(%f,%f)" v1.x v1.y v2.x v2.y; print_endline ""
      | Button_down -> Printf.printf "Button down"; print_endline "");
      ) status;
      w) >>=
    run 60

  | 2 -> world >>=
    borders_follow_buffer_size true >>=
    add_f (fun b -> b.mass ** {x = 0.; y = -1000.}) >>=
    add_random_balls xm ym 70 >>= 
    set_restitution 0.8 >>=
    run 60

  | _ -> world >>=
    borders_follow_buffer_size true >>=
    add_f (fun b -> b.mass ** {x = 0.; y = -1000.}) >>=
    add_random_balls xm ym 70 >>= 
    set_restitution 0.8 >>=
    run 60
