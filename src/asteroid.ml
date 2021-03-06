open Utils

module G = (Screen)

let draw_asteroid b buf =
  let open Ball in
  let open Vector in
G.set_color (Color.rgb 104 104 104) buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) + 0) 1 1 buf;
G.set_color (Color.rgb 135 135 135) buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) + 7) 1 1 buf;
G.set_color (Color.rgb 82 82 82) buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) -11) 1 1 buf;
G.set_color (Color.rgb 76 76 76) buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) + 8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) -9) 1 1 buf;
G.set_color (Color.rgb 129 129 129) buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) + 10) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) + 10) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) + 10) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) + 10) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) + 10) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) + 9) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) + 9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) + 9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) + 9) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) + 8) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) + 8) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) + 8) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) + 8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) + 8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) + 8) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) -10) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) -11) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) -10) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) -11) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) -10) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) -12) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) -11) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) -10) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) -12) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) -11) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) -10) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) -12) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) -11) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) -12) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) -11) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) -10) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) -12) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) -11) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) -10) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) -11) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) -11) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) -10) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) -11) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) -10) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) -10) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) -10) ((int b.pos.y) -11) 1 1 buf;
G.set_color (Color.rgb 113 113 113) buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) + 6) 1 1 buf;
G.set_color (Color.rgb 115 115 115) buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) + 10) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) + 9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) + 9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) + 9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) + 9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) + 9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) + 9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) + 8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) + 8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) + 8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) + 8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) + 8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) + 8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) + 8) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 11) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 11) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) -10) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) -10) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) -9) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) -8) ((int b.pos.y) -13) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) -13) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -13) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -13) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -13) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -13) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) -13) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) -14) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) -14) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -14) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -14) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -14) 1 1 buf;
G.set_color (Color.rgb 119 119 119) buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) + 9) 1 1 buf;
G.set_color (Color.rgb 147 147 147) buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) -13) 1 1 buf;
G.set_color (Color.rgb 131 131 131) buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) + 10) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) + 4) 1 1 buf;
G.set_color (Color.rgb 91 91 91) buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) + 7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) + 6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 11) ((int b.pos.y) + 5) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 11) ((int b.pos.y) + 4) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 11) ((int b.pos.y) + 3) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 11) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 12) ((int b.pos.y) + 2) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 11) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 12) ((int b.pos.y) + 1) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) + 11) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) + 12) ((int b.pos.y) + 0) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) + 12) ((int b.pos.y) -1) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 11) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) + 12) ((int b.pos.y) -2) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 11) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 12) ((int b.pos.y) -3) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 11) ((int b.pos.y) -4) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 11) ((int b.pos.y) -5) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 11) ((int b.pos.y) -6) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) + 10) ((int b.pos.y) -7) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) + 9) ((int b.pos.y) -8) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 3) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) + 8) ((int b.pos.y) -9) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) -10) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) + 6) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) + 7) ((int b.pos.y) -11) 1 1 buf;
G.fill_rect ((int b.pos.x) -7) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) -6) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) + 4) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) + 5) ((int b.pos.y) -12) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -13) 1 1 buf;
G.fill_rect ((int b.pos.x) + 1) ((int b.pos.y) -13) 1 1 buf;
G.fill_rect ((int b.pos.x) + 2) ((int b.pos.y) -13) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -14) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -14) 1 1 buf;
G.fill_rect ((int b.pos.x) + 0) ((int b.pos.y) -14) 1 1 buf;
G.fill_rect ((int b.pos.x) -5) ((int b.pos.y) -15) 1 1 buf;
G.fill_rect ((int b.pos.x) -4) ((int b.pos.y) -15) 1 1 buf;
G.fill_rect ((int b.pos.x) -3) ((int b.pos.y) -15) 1 1 buf;
G.fill_rect ((int b.pos.x) -2) ((int b.pos.y) -15) 1 1 buf;
G.fill_rect ((int b.pos.x) -1) ((int b.pos.y) -15) 1 1 buf;
