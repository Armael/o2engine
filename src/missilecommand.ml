open Utils
open Ui

module C = QuadTreeContainer
module PhysEngine = Phys.Make (C)

module Engine = Engine.Make (PhysEngine) (Screen)

let next_missile_delay = ref 100
let defense_ball_exist = ref false
let launch = ref false

let rec read_action l w =
  let open Engine in
  let open Ball in
  let open Vector in
  match l with
  | [] -> w
  | (Slide(v1,v2),pos)::ll -> 
    read_action ll w;
  | (Keypress c, pos)::ll -> launch := false;
    if !defense_ball_exist then (
      map 
	(fun b ->
	  match b.id with
	  | 0 -> {b with pos = {x = 400.;y = 20.}; speed = {x = 0.;y = 0.}}
	  | _ -> b) w >>=
	read_action ll
    ) else
      let newBall = Ball.create() in
      read_action ll
	(add_ball {newBall with pos = {x = 400.;y = 20.};
	  radius = 12.;id = 0;
	  mass =5.;color = Color.red} w);
  | (Button_up, Pos(x,y))::ll -> if !defense_ball_exist && not (!launch) then (
    launch := true;
    read_action ll
      (map
   	 (fun b ->
   	   match b.id with
   	   | 0 -> {b with speed = (sub {x = (float_of_int x);
					y = (float_of_int y)}
				     {x = 400.;y = 20.} )}
   	   | _ -> b) w)
      
  ) else read_action ll w;
  | (_, pos)::ll ->
    read_action ll w

let () =
  let open Screen in
  let open Engine in  

  let world = Engine.new_world 800 500 in
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
	(add_ball {pos = {x =  (Random.float 450.) +. 30.;y = 480.};
	  	   speed = {x =  (Random.float 50.) -. 25.;y =  -.((Random.float 30.) +. 200.)};
	 	   radius = 18.;id = 1;
	 	   mass =5.;color = Color.black} w) >>=
	  read_action uia;
      ) else read_action uia w
    ) >>= 

    run 60
