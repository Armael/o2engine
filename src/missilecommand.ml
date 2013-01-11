open Utils
open Ui

module C = QuadTreeContainer
module PhysEngine = Phys.Make (C)

module G = (Screen)
module Engine = Engine.Make (PhysEngine) (G)

let next_missile_delay = ref 100
let defense_ball_exist = ref false
let launch = ref false

let width = 800 and height = 500 

let draw_gradient v1 v2 color1 color2 buf =
  let open Vector in
  let left = v1.x and right = v2.x in
  let bottom = v1.y and top = v2.y in
  let (r1, g1, b1) = Color.get_rgb color1
  and (r2, g2, b2) = Color.get_rgb color2 in
  let delta = top -. bottom in
  let r_incr = (float (r2 - r1)) /. delta in
  let g_incr = (float (g2 - g1)) /. delta in
  let b_incr = (float (b2 - b1)) /. delta in
  
  for i = 0 to int delta do
    let c = Color.rgb (int (float r1 +. (float i) *. r_incr))
      (int (float g1 +. (float i) *. g_incr))
      (int (float b1 +. (float i) *. b_incr)) in
    G.set_color c buf;
    G.moveto (int left) (int bottom + i) buf;
    G.lineto (int right) (int bottom + i) buf
  done    

let draw_background buf =
  let open Vector in
  let lim_ground = 40 in
  draw_gradient {x = 0.; y = 0.} {x = float width; y = float lim_ground}
    (Color.rgb 28 13 13) (Color.rgb 91 50 45) buf;
  draw_gradient {x = 0.; y = float lim_ground} {x = float width; y = float height}
    (Color.rgb 183 196 218) (Color.rgb 85 114 168) buf;
  G.moveto 0 lim_ground buf;
  G.set_color Color.black buf;
  G.lineto width lim_ground buf

let new_missile () =
  let open Vector in
  let open Ball in
  let x = Random.float (float width)
  and y = float height in
  let speed_norm = 150. in
  let vx = Random.float (float width) -. x in
  let speed = speed_norm ** (unit {x = vx; y = -. (abs_float ((Random.float 5. +. 0.5) *. vx))}) in
  {pos = {x = x; y = y};
   speed = speed;
   radius = 18.; id = 1;
   mass = 5.; color = Color.rgb 95 95 95}

let rec read_action l w =
  let open Engine in
  let open Ball in
  let open Vector in
  let rocket_speed = 700. in
  match l with
  | [] -> w
  | (Slide(v1,v2),pos)::ll -> 
    read_action ll w;
  | (Keypress c, pos)::ll -> launch := false;
    if !defense_ball_exist then (
      map 
	(fun b ->
	  match b.id with
	  | 0 -> {b with pos = {x = float (width / 2);y = 20.}; speed = {x = 0.;y = 0.}}
	  | _ -> b) w >>=
	read_action ll
    ) else
      let newBall = Ball.create() in
      read_action ll
	(add_ball {newBall with pos = {x = float (width / 2);y = 20.};
	  radius = 12.;id = 0;
	  mass =5.;color = Color.red} w);
  | (Button_up, Pos(x,y))::ll -> if !defense_ball_exist && not (!launch) then (
    launch := true;
    read_action ll
      (map
   	 (fun b ->
   	   match b.id with
   	   | 0 -> {b with speed = rocket_speed ** (unit (sub {x = (float_of_int x);
							      y = (float_of_int y)}
							   {x = float (width / 2);y = 20.}))}
   	   | _ -> b) w)
      
  ) else read_action ll w;
  | (_, pos)::ll ->
    read_action ll w

let () =
  let open G in
  let open Engine in  

  let world = Engine.new_world width height in
  Random.self_init ();
  let open Ball in
  let open Vector in

  world >>=
    set_border PhysEngine.Bottom 0. >>=
    set_user_action (fun uia w ->
      defense_ball_exist := false;
      iter (fun b -> defense_ball_exist := !defense_ball_exist || (b.id = 0)) w;

      next_missile_delay := !next_missile_delay - 1;
      if !next_missile_delay = 0 then (
	next_missile_delay := 100;
	(add_ball (new_missile ()) w) >>=
	  read_action uia;
      ) else read_action uia w
    ) >>= 
    set_predraw_hooks (fun m -> IMap.add 0 draw_background m) >>=
    run 60
