type t = { x : float; y : float; }
val create : unit -> t
val add : t -> t -> t
val neg : t -> t
val scale : float -> t -> t
val sp : t -> t -> float
val rot : float -> t -> t
val norm : t -> float
val ( ++ ) : t -> t -> t
val ( -- ) : t -> t -> t
val ( ** ) : float -> t -> t
val ( |. ) : t -> t -> float
val print : t -> unit
