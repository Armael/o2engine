open Utils
open Ui

module C = QuadTreeContainer
module PhysEngine = Phys.Make (C)

module G = (Screen)
module Engine = Engine.Make (PhysEngine) (G)

let next_asteroid_delay = ref 100
let defense_ball_exist = ref false
let launch = ref false
let level = ref 0
let nb_astroid_until_next_level = ref 8
let score = ref 0
let life = ref 10


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
  G.lineto width lim_ground buf;

  let rect c x y w h =
    G.set_color c buf;
    G.fill_rect x y w h buf;
    G.set_color Color.black buf;
    G.draw_rect x y w h buf in

  (* Dessin de la base *)
  rect (Color.rgb 126 38 0) 122 10 107 27;
  rect (Color.rgb 48 89 24) 227 10 73 47;
  rect (Color.rgb 122 8 57) 300 10 73 85;
  rect (Color.rgb 117 101 8) 316 10 120 55;
  rect (Color.rgb 10 81 48) 436 10 122 106;
  rect (Color.rgb 31 42 2) 497 10 123 55

let draw_object b buf =
  let open Ball in
  let open Vector in
  if b.id = 1 then
    Asteroid.draw_asteroid b buf
  else if b.id = 2 then
    BigAsteroid.draw_asteroid b buf
  else (
    G.set_color b.color buf;
    G.fill_circle (int b.pos.x) (int b.pos.y) (int b.radius) buf
  )

let new_missile () =
  let open Vector in
  let open Ball in
  let x = Random.float (float width)
  and y = float height in
  let speed_norm = 150. in
  let vx = Random.float (float width) -. x in
  let speed = speed_norm ** (unit {x = vx; y = -. (abs_float ((Random.float 5. +. 0.5) *. vx))}) in
  let ast_type = if Random.int 3 = 1 then 1 else 2 in
  let radius = if ast_type = 1 then 15. else 27. in
  let mass = if ast_type = 1 then 5. else 10. in
  {pos = {x = x; y = y};
   speed = speed;
   radius = radius; id = ast_type;
   mass = mass; color = Color.black}

let rec read_action l w =
  let open Engine in
  let open Ball in
  let open Vector in
  let rocket_speed = 700. in
  match l with
  | [] -> w
  
  | (Sliding(v1,v2),pos)::ll -> read_action ll w;
  
  | (Slide(v1,v2),pos)::ll -> 
    read_action ll w;
    
  | (Keypress c, pos)::ll -> launch := false;
    if !defense_ball_exist then (
      map 
	(fun b ->
	  match b.id with
	  | 0 -> {b with pos = {x = float (width / 2);y = 50.}; speed = {x = 0.;y = 0.}}
	  | _ -> b) w >>=
	read_action ll
    ) else
      let newBall = Ball.create() in
      read_action ll
	(add_ball {newBall with pos = {x = float (width / 2);y = 50.};
	  radius = 12.;id = 0;
	  mass =5.;color = Color.red} w);
	  
	|(Button_down, Pos(x,y))::ll ->read_action ll w;
	
  | (Button_up, Pos(x,y))::ll ->
  score := !score - 20;
  if not(!defense_ball_exist) then (
      let newBall = Ball.create() in
       launch := true;
       defense_ball_exist := true;
      read_action ll
		(add_ball {newBall with pos = {x = float (width / 2);y = 50.};
	  radius = 12.;id = 0;
	  mass =5.;color = Color.red} w) >>=
	   (map
   	 (fun b ->
   	   match b.id with
   	   | 0 -> {b with speed = rocket_speed ** (unit (sub {x = (float_of_int x);
							      y = (float_of_int y)}
							   {x = float (width / 2);y = 50.}))}
   	   | _ -> b))
   	)
	  else (
	  defense_ball_exist := false; 
    launch := true;
    read_action ll
      (map
   	 (fun b ->
   	   match b.id with
   	   | 0 -> {b with pos = {x = float (width / 2);y = 50.}; speed = rocket_speed ** (unit (sub {x = (float_of_int x);
							      y = (float_of_int y)}
							   {x = float (width / 2);y = 50.}))}
   	   | _ -> b) w))


let () =
  let open G in
  let open Engine in  
  Random.self_init ();

  let open Ball in
  let open Vector in

		let rec main_world() = 
		Engine.new_world width height >>=
		set_user_action (fun uia w ->
		let rec read l w= 
		match l with 
		|[]->w
		|(Keypress c, pos)::ll -> world()
		|_::ll-> read ll w
		in 
		read uia w
		)
		>>=  
		set_predraw_hooks (fun m -> IMap.add 0 draw_background m) >>=
		set_postdraw_hooks (fun m ->      
    IMap.add 0 (fun buf ->
     if !life <= 0 then    
    G.draw_string (buf.G.width / 2 - 50) (buf.G.height / 2 + 20)
	(Printf.sprintf "Vous avez perdu") buf;
   G.draw_string (buf.G.width / 2 - 50) (buf.G.height / 2)
	(Printf.sprintf "Votre score : %d" !score) buf;
		G.draw_string (buf.G.width / 2 - 200) (buf.G.height / 2 - 20)
	(Printf.sprintf "Appuyez sur une touche du clavier pour lancer un nouveau jeu") buf) m)

    and world() = 
     life := 10;
    	 Engine.new_world width height >>=
     log_ball_collisions true >>=
     log_borders_collisions true >>=
    set_border PhysEngine.Bottom 30. >>=
    set_user_action (fun uia w ->
      let ball_log = get_ball_collisions_log w in
      	let borders_log  = get_borders_collisions_log w in
      		while not (Queue.is_empty ball_log) do
      			let (b1,b2) = Queue.pop ball_log in
      				match (b1.id,b2.id) with
      				|(0,_)->score := !score  + 50
      				|(_,0)->score := !score  + 50
      				|_->()
      			done;
      while not (Queue.is_empty borders_log) do
      	let (b,a) = Queue.pop borders_log in
			match b.id with
						|0->()
			      	|_->
			      	if b.pos.x >= 122. && b.pos.x <= 620. then 
			      	(
			      	life := !life - b.id;
			      	)
			  done;
      defense_ball_exist := false;
      iter (fun b -> defense_ball_exist := !defense_ball_exist || (b.id = 0)) w;
      next_asteroid_delay := !next_asteroid_delay - 1;
      
      if !next_asteroid_delay = 0 then (
      if !nb_astroid_until_next_level = 0 then(
      nb_astroid_until_next_level := 8;
      level := min(!level + 1) 13);
      
  nb_astroid_until_next_level := !nb_astroid_until_next_level - 1;
	next_asteroid_delay := 100 - (5 * !level);
		(add_ball (new_missile ()) w)>>= 
		map (fun b ->if ((b.radius +. b.pos.y -. 30. -. 10. <= 0.) && (b.speed.y > 0.) && (b.id <> 0)) then 
      {b with pos = {x = -1000. ;y = -1000.}} else b) >>=
	   read_action uia;
      )else 
      (
      if !life <= 0 then
      (
      main_world()
      )
      else (map (fun b ->if ((b.pos.y -. b.radius -. 30. -. 10. <= 0.) && (b.speed.y > 0.) && (b.id <> 0)) then 
      {b with pos = {x = -1000. ;y = -1000.}} else b) w)>>= read_action uia;
      )
    )>>= 
    set_predraw_hooks (fun m -> IMap.add 0 draw_background m) >>=
    set_postdraw_hooks (fun m ->      
    IMap.add 0 (fun buf -> G.draw_string 50 (buf.G.height - 20)
	(Printf.sprintf "Score : %d" !score) buf;
		G.draw_string 50 (buf.G.height - 40)
	(Printf.sprintf "Life : %d" !life) buf) m)
    >>=
    set_ball_hook draw_object 
    in
    
    main_world()>>=
    run 60
