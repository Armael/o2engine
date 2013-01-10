open Utils

module C = QuadTreeContainer
module PhysEngine = Phys.Make (C)

module Engine = Engine.Make (PhysEngine) (Screen)

let balls_list xm ym = [(50., ym/.2., 0); (xm /. 2., ym /. 2., 1);
			((xm /. 2.) +. 30., (ym /. 2.) +. 30., 1);
			((xm /. 2.) +. 30., (ym /. 2.) -. 30., 1);
			(xm /. 2. +. 60., ym /. 2., 1);
			(xm /. 2. +. 60., ym /. 2. +. 60., 1);
			(xm /. 2. +. 60., ym /. 2. -. 60., 1); 
			(xm /. 2. +. 90., ym /. 2. -. 30., 1);
			(xm /. 2. +. 90., ym /. 2. -. 90., 1);
			(xm /. 2. +. 90., ym /. 2. +. 30., 1);
			(xm /. 2. +. 90., ym /. 2. +. 90., 1)]

let () =
  let open Screen in
  let open Engine in
  let open Ball in
  let open Vector in
  let world = Engine.new_world 800 600 in
  let xm = float world.buff.width
  and ym = float world.buff.height in
  
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
    ballsWithCoordList (balls_list xm ym) 20. >>=

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
      else {x = 0.; y = 0.}) >>=

    set_restitution 0.99 >>=

    set_user_action (fun ui_state w ->
      let w = set_postdraw_hook [] w in
      List.fold_left (fun w (t, _) -> match t with
      | Ui.Slide (v1, v2) -> modify_i 0
	(fun b -> {b with speed = (2.) ** (v2 -- v1)}) w
      | Ui.Sliding (v1, v2) -> set_postdraw_hook
	[0, (fun buf -> moveto (int v1.x) (int v1.y) buf;
	  lineto (int v2.x) (int v2.y) buf)] w
      | _ -> w) w ui_state) >>=

    run 60
