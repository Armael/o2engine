type ('a,'b,'c) event_handler =
{
keypress_handler : char -> 'a;
button_handler : bool -> 'b;
pos_handler : (int*int)-> 'c 
}

let create_handler x y z= 
{
keypress_handler = (fun c -> x);
button_handler = (fun b -> y);
pos_handler = (fun (i,j)-> z) 
}

let set_keypress_handler fkh h={h with keypress_handler = fkh}
let set_button_handler fkh h={h with button_handler = fkh}
let set_pos_handler fkh h={h with pos_handler = fkh}
let mouse_memory = ref false

let handle_event event_hand =
	let status = Graphics.wait_next_event [Graphics.Key_pressed; Graphics.Button_down; Graphics.Button_up;Graphics.Poll]
		in 
			let aux b f a=
			match b with 
			|true->Some (f a)
			|false->None
			in
			
		let res = (aux status.Graphics.keypressed event_hand.keypress_handler status.Graphics.key,
		aux (!mouse_memory = status.Graphics.button) event_hand.button_handler status.Graphics.button,
		Some (event_hand.pos_handler (status.Graphics.mouse_x, status.Graphics.mouse_y))) in
		mouse_memory := status.Graphics.button;
		res

