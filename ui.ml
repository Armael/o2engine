type t = Keypress of char | Button_up | Button_down
type pos = Pos of int * int
type status = ((t * pos) list)

let mouse_memory = ref false

let get_status () =
  let button_get button_stat pos l =
    let aux button_stat pos l =
      match button_stat = !mouse_memory, button_stat with
      |true, true -> (Button_up, pos)::l
      |true, false -> (Button_down, pos)::l
      |false, _ -> l
    in
    let res = aux button_stat pos l in
    mouse_memory := button_stat;
    res
  in

  let key_get key_stat key_char pos l=
    match key_stat with
    |true -> ((Keypress key_char), pos)::l
    |false -> l
  in

  let rec get_event b =
    match b with 
    |false -> []
    |true ->
      let statbe = Graphics.wait_next_event [Graphics.Button_down;
	   				     Graphics.Button_up; 
	   				     Graphics.Poll] in 
      let statke = Graphics.wait_next_event [Graphics.Key_pressed;
	  				     Graphics.Poll] in
      let open Graphics in
      (button_get statbe.button 
	 (statbe.mouse_x,statbe.mouse_y)
	 (key_get statke.keypressed statke.key 
	    (statke.mouse_x,statke.mouse_y)
	    (get_event statke.keypressed))) 
  in
  get_event true
