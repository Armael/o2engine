(* Module de manipulation de « rectangles » : un rectangle est la
   donnée d'un couple de vecteurs (v1, v2), v1 correspondant aux
   coordonnées du coin inférieur gauche, v2 les coordonnées du coin
   supérieur droit *)

let is_in_rect v1 v2 ball = 
  (* Retourne true si le centre de ball est dans le rectangle défini par v1 et v2 *) 
  let open Ball in
  let open Vector in
  (ball.pos.x >= v1.x
   && ball.pos.x <= v2.x
   && ball.pos.y >= v1.y
   && ball.pos.y <= v2.y)

let is_in_rect_partial v1 v2 ball =
  (* Retourne true si ball est en partie dans le rectangle défini par
     v1 et v2 (son centre est en dedans mais elle peut dépasser) *)
  let open Ball in
  let open Vector in
  (((ball.pos.x +. ball.radius >= v1.x)
    && (ball.pos.x +. ball.radius <= v2.x))
   ||
     ((ball.pos.x -. ball.radius >= v1.x)
      && (ball.pos.x -. ball.radius <= v2.x)))
  &&
    (((ball.pos.y +. ball.radius >= v1.y)
      && (ball.pos.y +. ball.radius) <= v2.y)
     ||
       ((ball.pos.y -. ball.radius >= v1.y)
	&& (ball.pos.y -. ball.radius <= v2.y)))

let moy a b = (a +. b) /. 2.

let bl_rect v1 v2 =
  (* Retourne le sous-rectangle inférieur gauche de (v1, v2) *)
  let open Vector in
  (v1, 
   {x = moy v1.x v2.x;
    y = moy v1.y v2.y})

let br_rect v1 v2 =
  (* Retourne le sous-rectangle inférieur droit de (v1, v2) *)
  let open Vector in
  ({v1 with x = moy v1.x v2.x},
   {v2 with y = moy v1.y v2.y})

let tl_rect v1 v2 =
  (* Retourne le sous-rectangle supérieur gauche de (v1, v2) *)
  let open Vector in
  ({v1 with y = moy v1.y v2.y},
   {v2 with x = moy v1.x v2.x})

let tr_rect v1 v2 =
  (* Retourne le sous-rectangle supérieur droit de (v1, v2) *)
  let open Vector in
  ({x = moy v1.x v2.x; 
    y = moy v1.y v2.y},
   v2)

let is_in_multiple_sub_rect vx vy ball =	
  (* Retourne true si ball est dans plusieurs sous-rectangles *)
  let is_in_rect_partial (u, v) b = is_in_rect_partial u v b in
  let open Ball in
  let open Vector in		
  let br1 = is_in_rect_partial (bl_rect vx vy) ball in
  let br2 = is_in_rect_partial (br_rect vx vy) ball in
  let br3 = is_in_rect_partial (tl_rect vx vy) ball in
  let br4 = is_in_rect_partial (tr_rect vx vy) ball in
  (br1 && (br2 || br3 || br4))
  || (br2 && (br3 || br4))
  || (br3 && br4)
