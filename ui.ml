type t = Keypress of char | Button_up | Button_down | Slide of Vector.t * Vector.t
type pos = Pos of int * int
type status = ((t * pos) list)

let mouse_memory = ref false
let mouse_pos_memory = ref (Vector.create ())


let get_status () =
  let button_get button_stat pos l =
    let aux button_stat pos l =
      match button_stat <> !mouse_memory, button_stat with
      |true, true -> mouse_pos_memory := {Vector.x = float_of_int (fst pos); Vector.y = float_of_int (snd pos)};
      							  (Button_up, Pos (fst pos,snd pos))::l
      |true, false -> 
      									if ((!mouse_pos_memory).Vector.x <> (float_of_int (fst pos)) ||  (!mouse_pos_memory).Vector.y <> (float_of_int (snd pos)))
      									then 
      									((Slide (!mouse_pos_memory,{Vector.x = float_of_int (fst pos); Vector.y = float_of_int (snd pos)}), 
      									Pos (fst pos,snd pos)))::
      									(Button_down, Pos (fst pos,snd pos))::l
      									else(Button_down, Pos (fst pos,snd pos))::l
      |false, _ -> l
    in
    let res = aux button_stat pos l in
    mouse_memory := button_stat;
    res
  in

  let key_get key_stat key_char pos l=
    match key_stat with
    |true -> ((Keypress (key_char())), Pos (fst pos,snd pos))::l
    |false -> l
  in

  let rec get_event b i =
    match b, i with 
    | (false, _) -> []
    | (_, 0) -> []
    | (true, _) ->
      let statbe = Graphics.wait_next_event [Graphics.Button_down;
	   				     Graphics.Button_up; 
	   				     Graphics.Poll] in 
      let open Graphics in
      (button_get statbe.button 
	 ((statbe.mouse_x, statbe.mouse_y))
	 (key_get (key_pressed()) read_key 
	    (((fst (mouse_pos())), snd (mouse_pos())))
	    (get_event (key_pressed()) (i-1)))) 
  in
  get_event true 5
