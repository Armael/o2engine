type t = Keypress of char | Button_up | Button_down
	 | Slide of Vector.t * Vector.t | Sliding of Vector.t * Vector.t
type pos = Pos of int * int
type status = ((t * pos) list)

val get_status : unit -> status
