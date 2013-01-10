type ('a,'b,'c) event_handler =
  { keypress_handler : char -> 'a;
    button_handler : bool -> 'b;
    pos_handler : (int*int)-> 'c }

let create_handler ()= 
  { keypress_handler = (fun c -> ());
    button_handler = (fun b -> ());
    pos_handler = (fun (i, j) -> ()) }

let set_keypress_handler fkh h = {h with keypress_handler = fkh}
let set_button_handler fkh h = {h with button_handler = fkh}
let set_pos_handler fkh h = {h with pos_handler = fkh}
let mouse_memory = ref false

let handle_event event_hand =
  let status = Graphics.wait_next_event [Graphics.Key_pressed;
					 Graphics.Button_down;
					 Graphics.Button_up;
					 Graphics.Poll] in 
  let aux b f a=
    match b with 
    | true -> Some (f a)
    | false -> None in
  
  let res = 
    open Graphics in
  (aux status.keypressed event_hand.keypress_handler status.key,
   aux (!mouse_memory = status.button) event_hand.button_handler status.button,
   Some (event_hand.pos_handler (status.mouse_x, status.mouse_y))) in

mouse_memory := status.button;
res

