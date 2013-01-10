type t = Keypress of char | Button_up | Button_down
type pos = Pos of int * int
type status = ((t * pos) list)

val get_status : unit -> status
