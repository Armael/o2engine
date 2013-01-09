type ('a,'b,'c) event_handler =
{
keypress_handler : char -> 'a;
button_handler : bool -> 'b;
pos_handler : (int*int)-> 'c 
}

val create_handler : unit -> ('a,'b,'c) event_handler

val set_keypress_handler : (char -> 'a) -> ('a,'b,'c) event_handler -> ('a,'b,'c) event_handler
val set_button_handler : (char -> 'a) -> ('a,'b,'c) event_handler -> ('a,'b,'c) event_handler
val set_pos_handler : (char -> 'a) -> ('a,'b,'c) event_handler -> ('a,'b,'c) event_handler

val handle_event : ('a,'b,'c) event_handler -> ('a option) * ('b option) * ('c option)
